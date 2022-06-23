############################################
# Function that calculates the conditional #
# Cook's distance for GLMM based on the    #
# Pinho, Nobre and Singer proposal         #
############################################


#Auxiliar function for preparation of the random effects vector
prepare.ranef<-function(u){
  p<-ncol(u)
  q<-nrow(u)
  out<-c()
  k<-1
  for(i in 1:q){ #for each row
    for(j in 1:p){ #traverse the row
      out[k]<-u[i,j] #placing the element in the vector
                     #so that the random effects for each
                     #unit are next to each other
      k<-k+1
    }
  }
  return(out)
}

#Inputs: fit -> a "merMod" object from the lmer function (package lme4 build 1.0-5)
#        family -> A string identifier of the distribution family for the
#                  response variable. Currently only "poisson", 
#                  negative binomial ("nb") and "gamma" are hardcoded. 
#                  You can add your own easily (shown bellow). 
#                  Attempts to make a general family identifier will be made 
#                  eventually.

CondCook.glmm<-function(fit,family){
  
  dims<-getME(fit,"devcomp")$dims # several informations about the model
  N<-dims[1] #sample size
  s<-ncol(ranef(fit)[[1]]) #number of random effects per unit
  q<-nrow(getME(fit,"Zt"))/s #number of units
  D<- as.matrix(solve(getME(fit,"Lambda"))) #Inverse of covariance matrix D
  
  CD<-matrix(rep(0,N*3),N,3)  #stores the components of the Cook's distance
  #Column 1 -> beta
  #Column 2 -> u
  #Column 3 -> beta, u
  
  lbeta<-0  #Derivative of l_{(m)}(\widehat(\beta)) with respect to beta
  lbetabeta<-0  #Second derivative of l_{(m)}(\widehat(\beta)) with respect to beta
  lu<-0  #Derivative of l_{(m)}(\widehat(u_s)) with respect to u
  luu<-0 #Second derivative of l_{(m)}(\widehat(u_s)) with respect to u
  
  Em<-function(m) (getME(fit,"y")-getME(fit,"mu"))[-m] #residuals Y-\hat{Y}
  
  #The switch-case bellow creates a suitable function for calculating the residuals
  #Y-\hat{Y} (given u, the random effects) and \ddot{C}_{\eta\eta} in the absence
  #of the m-th observation.
  switch(family, #This is a tricky but useful syntax for swith-case in R.
     poisson={ #assuming a log link
        Bm<-function(m) diag(getME(fit,"mu")[-m])  #matrix \ddot{C}_{\eta\eta}
        A<-1 #A^{-1}(\phi)
     },
     nb={ #assuming a log link
       getNBdisp <- function(object) { # taken from the gitHub repository of the lme4 package
         get(".Theta",envir=environment(object@resp$family$aic)) #this theta is the one in Venables and Ripley (1999)
       }
       k<- 1/getNBdisp(fit) #let k = 1/theta
       Bm<-function(m){
         mu2<-getME(fit,"mu")[-m]
         y2<-getME(fit,"y")[-m]
         out<-diag(mu2*(1+k*y2)*(1+k*mu2)^(-2))
         return(out)  #matrix \ddot{C}_{\eta\eta}
       } 
       Em<-function(m) (getME(fit,"y")-getME(fit,"mu"))[-m]/(1+k*getME(fit,"mu")[-m]) #residuals Y-\hat{Y}
       A<-1 #A^{-1}(\phi)
     },
     gamma.cannonical={
       Bm<-function(m) diag((getME(fit,"mu")^2)[-m])  #matrix \ddot{C}_{\eta\eta}
       A<-1 #A^{-1}(\phi) # not really 1, but only affects the scale
     },
     gamma.inverse={
       Bm<-function(m) diag(-(getME(fit,"mu"))[-m])  #matrix \ddot{C}_{\eta\eta}
       A<-1 #A^{-1}(\phi) # not really 1, but only affects the scale
     },
     gamma.log={
       Bm<-function(m) diag(log((getME(fit,"mu")))[-m])  #matrix \ddot{C}_{\eta\eta}
       A<-1 #A^{-1}(\phi) # not really 1, but only affects the scale
     },
     #Here is what to add if you need to hardcode another family.
     #Uncomment the lines bellow after inserting the missing parts
     #my.family={
        # B<- (expression for B = \ddot{C}_{\eta\eta} as function of m)
        # A<- (expression for A^{-1}(\phi))
     #},    
     stop("Unsupported family")
  )#end of swith-case
  
  for(m in c(1:N)){ #removing the m-th observation
    Y<-getME(fit,"y")[-m]  #sample
    X<-getME(fit,"X")[-m,] #covariates
    Z<-t(getME(fit,"Zt"))[-m,] #Z matrix
    u<-getME(fit,"u")  #predicted random effects
    E<-Em(m)
    B<-Bm(m)
    
    lbeta<-t(X)%*%E
    lbetabeta<- t(X)%*%B%*%X
    deltabeta<- solve(-lbetabeta)%*%lbeta #approximation for (\hat{beta}-\hat{\beta}_{(m)})
      
    lu<-t(Z)%*%E-D%*%u
    luu<- t(Z)%*%B%*%Z-D
    deltau<- solve(-luu)%*%lu #approximation for (\hat{u}-\hat{u}_{(m)})
      
    CD[m,1]<-as.numeric(t(deltabeta)%*%(lbetabeta)%*%deltabeta)
    CD[m,2]<-as.numeric(t(deltau)%*%(t(Z)%*%B%*%Z)%*%deltau)
  }#end of for(m)
  return(CD)
}
