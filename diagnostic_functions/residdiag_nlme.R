
## *** Diagnostic analysis for linear mixed models fitted via library nlme 
## *** Authors: Francisco Marcelo M. Rocha, Juvencio S. Nobre and Julio M. Singer

#####################################################################################################################
## This function generates the following diagnostic plots for gaussian linear mixed models fitted via function lme ##
## in library nlme.                                                                                                ##
#####################################################################################################################

# plot 1: Modified Lesaffre-Verbeke index versus unit indices
# plot 2: Standardized marginal residuals versus fitted values and corresponding histogram
# plot 3: Mahalanobis distance versus unit indices
# plot 4: Chi-squared QQ plot for Mahalanobis distance
# plot 5: Standardized conditional residuals versus fitted values and corresponding histogram
# plot 6: Normal QQ plot and histogram for standardized least confounded conditional residuals
# plot 7: Cook's Conditional distance versus observation indices
# plot 8: Cook's Conditional distance 1 (D1i) versus observation indices
# plot 9: Cook's Conditional distance 2 (D2i) versus observation indices
# plot 10: Cook's Conditional distance 3 (D3i) versus observation indices
# plot 11: Generalized joint leverage (L) versus unit indices
# plot 12: Generalized marginal leverage (L1) versus unit indices
# plot 13: Generalized random component leverage (L2) versus unit indices
# plot 14: Generalized joint leverage [Li(jj)] versus observation indices
# plot 15: Generalized marginal leverage [L1i(jj)] versus observation indices
# plot 16: Generalized random component leverage [L2i(jj)] versus observation indices

#################################################################################################################
## 1. The data must be arranged in the LONG format, with units indexed by a numerical (not necessarily equally ##
##    spaced) variable expressed as a factor.                                                                  ##
## 2. The horizontal axis contains either the unit indices or the observation indices                          ##
## 3. The outliers are labelled in the format unit.obs (e.g. 13.5 denotes the fifth observation of unit        ##
##    labelled 13)                                                                                             ##
#################################################################################################################

## An example is

## dataset<-groupedData(response ~  time|id, data=dataset_label)
## model1<-lme(response ~ time, random = ~time|id, na.action=na.omit, data=dataset)  
        # use of the function lme in nlme to fit a mixed model 
## resid1<-residdiag.nlme(model1,limit=2,plotid=1:16)
        # the computed quantities will be saved in the object labelled resid1
        # the limit option indicates cutpoints for the residual plots
        # plotid indicates the required residual plots
## names(resid1)  
        # produces the labels of the quantities saved in resid1 (i.e.: mat.Z = Z, mat.X = X, Gam, R)
        # The model is : Y = X \beta + Z b + e
        # Var(Y) = V = Z Gam Z^\top + R
## resid1$least.confounded.residuals   
        # produces the least confounded standardized conditional residuals


###############################################################################################################
## *** References:                                                                                           ##
##                                                                                                           ##
##  - Singer, J.M., Nobre, J.S. and Rocha, F.M.M. (2017). Graphical tools for detecting departures from      ##
##        linear mixed models assumptions and some remedial measures.                                        ##
##        International Statistical Review, 85, 290-324.                                                     ##
##        doi:10.1111/insr.12178                                                                             ##
##  - Nobre, J.S. and Singer, J.M. (2007). Residual Analysis for Linear Mixed Models.                        ##
##        Biometrical Journal, 49, 1-13.                                                                     ##
##  - Nobre, J.S. and Singer, J.M. (2011). Leverage analysis for linear mixed models.                        ##
##        Journal of Applied Statistics, 38, 1063-1072.                                                      ##
##  - Pinheiro, J.C. and Bates, D.M. (2000). Mixed-effects models in S and S-plus. 1st edition.              ##
##        New York: Springer                                                                                 ##
##  - Scheipl, F., Greven, S. and Kuechenhoff, H. (2008) Size and power of tests for a zero random effect    ##
##       variance or polynomial regression in additive and linear mixed models.                              ##
##       Computational Statistics & Data Analysis, 52: 3283--3299.                                           ##
##                                                                                                           ##
###############################################################################################################

residdiag.nlme = function(fit, limit,plotid=NULL) {
require(MASS) 
require(Matrix)
#require(car)

###############################################################################################################
## This function obtains the square root of a matrix                                                         ##
###############################################################################################################
  
sqrt.matrix <- function(mat) {              
  mat <- as.matrix(mat)  
  singular_dec <- svd(mat,LINPACK = F)
  U <- singular_dec$u
  V <- singular_dec$v
  D <- diag(singular_dec$d)
  sqrtmatrix <- U %*% sqrt(D) %*% t(V)
}

###############################################################################################################
## This function extracts various objects of the function lme                                                ##
###############################################################################################################

extract.lmeDesign2 <- function(m){
    start.level = 1
    data <- getData(m)
    grps <- nlme::getGroups(m)
    n <- length(grps)
    X <- list()
    grp.dims <- m$dims$ncol
    Zt <- model.matrix(m$modelStruct$reStruct, data)
    cov <- as.matrix(m$modelStruct$reStruct)
    i.col <- 1
    n.levels <- length(m$groups)
    Z <- matrix(0, n, 0)
    if (start.level <= n.levels) {
      for (i in 1:(n.levels - start.level + 1)) {
        if (length(levels(m$groups[[n.levels - i + 1]])) != 1)
        {
          X[[1]] <- model.matrix(~m$groups[[n.levels - i +
                                              1]] - 1, 
                                 contrasts.arg = c("contr.treatment",
                                                   "contr.treatment"))
        }
        else X[[1]] <- matrix(1, n, 1)
        X[[2]] <- as.matrix(Zt[, i.col:(i.col + grp.dims[i] -
                                          1)])
        i.col <- i.col + grp.dims[i]
        Z <- cbind(mgcv::tensor.prod.model.matrix(X),Z)
      }
      Vr <- matrix(0, ncol(Z), ncol(Z))
      start <- 1
      for (i in 1:(n.levels - start.level + 1)) {
        k <- n.levels - i + 1
        for (j in 1:m$dims$ngrps[i]) {
          stop <- start + ncol(cov[[k]]) - 1
          Vr[ncol(Z) + 1 - (stop:start),ncol(Z) + 1 - (stop:start)] <- cov[[k]]
          start <- stop + 1
        }
      }
    }
    X <- if (class(m$call$fixed) == "name" &&  !is.null(m$data$X)) {
      m$data$X
    } else   {
      model.matrix(formula(eval(m$call$fixed)),data)
    }
    y <- as.vector(matrix(m$residuals, ncol = NCOL(m$residuals))[,NCOL(m$residuals)] + 
                   matrix(m$fitted, ncol = NCOL(m$fitted))[,NCOL(m$fitted)])
    return(list(
      Vr = Vr,                                                                 
      X = X,
      Z = Z,
      sigmasq = m$sigma ^ 2,
      lambda = unique(diag(Vr)),
      y = y,
      k = n.levels
    )
    )
}

##############################################################################################################
## Extracting information from lme fitted model and dataset                                                 ##
##############################################################################################################
  
  data.fit <- extract.lmeDesign2(fit)
  data <-    getData(fit)
  y <- data.fit$y
  X <- data.fit$X
  N <- length(y)                                                               # Number of observations
  id <-  sort(as.numeric(getGroups(fit, level = 1)), index.return = TRUE)$x    # as.numeric(getGroups(fit, level = 1))  
  subject <- as.numeric(unique(id))
  n <- length(as.numeric(names(table(id))))                                    # Number of units
  vecni <- (table(id))                                                         # Vector with number of observations per unit
  p <- ncol(X)                                                                 # Number of fixed parameters
  n.levels <- length(fit$groups)                                               # Number of levels of within subject factors
  start.level <- 1
  Cgrps <- nlme::getGroups(fit, level = start.level)                           # Level = 1 
  CCind <- levels((Cgrps))                                                     # Indices of the observations
  sigma2 <- fit$sigma^2
  obs <- numeric()

for (i in 1:n)
 {
  obs <- append(obs,1:vecni[i])                                                # Labels for observations
 }

##############################################################################################################
## Construction of the Z and Gam matrices                                                                   ##
##############################################################################################################
  
 if (n.levels > 1) { 
   lZi <- list()
   lgi <- list()
   numrow <- numeric()
   
   mgroups <- fit$groups      
   for (n in 1:length(CCind)) {
     dgi <- data.frame(as.matrix(mgroups[mgroups == CCind[n], ]))
     nrowzi <- dim(dgi)[1]
     ncolzi <- 0
    
  # Number of repetitions of the variance components to construct the Gi matrix   
    girep <- as.numeric(length(levels(dgi[,1])))
     for (k in 2:n.levels) {
       girep <- c(girep,as.numeric(length(levels(dgi[,k]))))
     }
  # Number of columns of the Zi matrix   
     for (k in 1:n.levels) {
       ncolzi <- ncolzi + as.numeric(length(levels(dgi[,k])))
     }
   # Numbers of one's by columns of the Zi matrix
     auxi <- as.vector(table(dgi[,1]))
     for (i in 2:n.levels) {
       auxi <- c(auxi,as.vector(table(dgi[,i])))
     }
   # Matrix Zi
     l <- 1
     Zi <- matrix(0,nrowzi,ncolzi)
     # Inserting elements in Zi
     for (j in 1:ncolzi) {
       Zi[l:(l + auxi[j] - 1),j] <- rep(1,auxi[j]) 
       l <- l + auxi[j]
       if (l == (nrowzi + 1)) l <- 1
     }

     lZi[[n]] <- Zi
     
     numrow[n] <- dim(Zi)[1]
     
  # Matrix Gi
     comp.var <- as.matrix(fit1$modelStruct$reStruct)
     auxg <- rep(as.numeric(comp.var[1])*sigma2,girep[1])
     for (i in 2:length(girep)) {
       auxg <- c(auxg,rep(as.numeric(comp.var[i])*sigma2,girep[i]))
     }
     lgi[[n]] <- diag(auxg)
   }
   q <- dim(lgi[[1]])[1]                     # Dimensions of Gi matrices
   for (h in 2:length(CCind)) {
     q <- c(q,dim(lgi[[h]])[1])
   }
   Z <- lZi[[1]]
   for (k in 2:length(CCind)) {
     Z <- bdiag(Z,(lZi[[k]]))
   }
   Z <- as.matrix(Z)
   nrowZi <- lZi[[1]]                        # Dmensions of Zi matrices
   for (h in 2:length(CCind)) {
     nrowZi <- c(nrowZi,dim(lZi[[h]])[1])
   }
   
   Gam <- lgi[[1]]
   for (k in 2:length(CCind)) {
     Gam <- bdiag(Gam,(lgi[[k]]))
   }
   Gam <- as.matrix(Gam)
 }else{
  mataux <- model.matrix(fit$modelStruct$reStruct,data)
  mataux <- as.data.frame(cbind(mataux,id))
  lZi <- list()
  lgi <- list()
  
  for (i in (as.numeric(unique(id)))) { 
     lZi[[i]] <- as.matrix((subset(split(mataux,id == i,
                                          drop = T)$`TRUE`,select = -id)))          
     lgi[[i]] <- getVarCov(fit,type = "random.effects")
  }
  Z <- as.matrix(bdiag(lZi))
  # for (i in 2:7) { 
  #   Z <- as.matrix(bdiag(Z,lZi[[i]]))          
  # }
    g <- getVarCov(fit,type = "random.effects")
    q <- dim(g)[1]                                                           # Total number of random effects
    Gam <- as.matrix(kronecker(diag(length(as.numeric(unique(id)))),g))
 }
##############################################################################################################
## Estimate of the covariance matrix of conditional errors (homoskedastic conditional independence model)   ##
############################################################################################################## 
  
  if (n.levels > 1) {   
    if (!inherits(fit, "lme")) 
      stop("object does not appear to be of class lme")
    grps <- nlme::getGroups(fit)
    n <- length(grps)                                                                      # Number of observations
    n.levels <- length(fit$groups)                                                         # Number of levels
    if (is.null(fit$modelStruct$corStruct)) 
      n.corlevels <- 0
    else n.corlevels <- length(all.vars(nlme::getGroupsFormula(fit$modelStruct$corStruct))) # Levels of the repeated measures
    if (n.levels < n.corlevels) {
      getGroupsFormula(fit$modelStruct$corStruct)
      vnames <- all.vars(nlme::getGroupsFormula(fit$modelStruct$corStruct))
      lab <- paste(eval(parse(text = vnames[1]), envir = fit$data))
      if (length(vnames) > 1) 
        for (i in 2:length(vnames)) {
          lab <- paste(lab, "/", eval(parse(text = vnames[i]), 
                                      envir = fit$data), sep = "")
        }
      grps <- factor(lab)
    }
    if (n.levels >= start.level || n.corlevels >= start.level) {
      if (n.levels >= start.level) 
        Cgrps <- nlme::getGroups(fit, level = start.level)                           # Level = 1
      else Cgrps <- grps
      Cind <- sort(as.numeric(Cgrps), index.return = TRUE)$ix                        # Indices of the observations
      rCind <- 1:n 
      rCind[Cind] <- 1:n
      Clevel <- levels(Cgrps)                                                        # Levels of the first nesting level
      n.cg <- length(Clevel)                                                         
      size.cg <- array(0, n.cg)
      for (i in 1:n.cg) size.cg[i] <- sum(Cgrps == Clevel[i])                        # Number of the observations by subject
    }
    else {
      n.cg <- 1
      Cind <- 1:n
    }
    if (is.null(fit$modelStruct$varStruct)) 
      w <- rep(fit$sigma, n)
    else {
      w <- 1/nlme::varWeights(fit$modelStruct$varStruct)
      group.name <- names(fit$groups)
      order.txt <- paste("ind<-order(data[[\"", group.name[1], 
                         "\"]]", sep = "")
      if (length(fit$groups) > 1) 
        for (i in 2:length(fit$groups)) order.txt <- paste(order.txt, 
                                                         ",data[[\"", group.name[i], "\"]]", sep = "")
      order.txt <- paste(order.txt, ")")
      eval(parse(text = order.txt))
      w[ind] <- w
      w <- w * fit$sigma
    }
    w <- w[Cind]
    if (is.null(fit$modelStruct$corStruct)) 
      lR <- array(1, n)
    else {
      c.m <- nlme::corMatrix(fit$modelStruct$corStruct)
      if (!is.list(c.m)) {
        lR <- c.m
        lR <- lR[Cind, ]
        lR <- lR[, Cind]
      }
      else {
        lR <- list()
        ind <- list()
        for (i in 1:n.cg) {
          lR[[i]] <- matrix(0, size.cg[i], size.cg[i])
          ind[[i]] <- 1:size.cg[i]
        }
        Roff <- cumsum(c(1, size.cg))
        gr.name <- names(c.m)
        n.g <- length(c.m)
        j0 <- rep(1, n.cg)
        ii <- 1:n
        for (i in 1:n.g) {
          Clev <- unique(Cgrps[grps == gr.name[i]])
          if (length(Clev) > 1) 
            stop("inner groupings not nested in outer!!")
          k <- (1:n.cg)[Clevel == Clev]
          j1 <- j0[k] + nrow(c.m[[i]]) - 1
          lR[[k]][j0[k]:j1, j0[k]:j1] <- c.m[[i]]
          ind1 <- ii[grps == gr.name[i]]
          ind2 <- rCind[ind1]
          ind[[k]][j0[k]:j1] <- ind2 - Roff[k] + 1
          j0[k] <- j1 + 1
        }
        for (k in 1:n.cg) {
          lR[[k]][ind[[k]], ] <- lR[[k]]
          lR[[k]][, ind[[k]]] <- lR[[k]]
        }
      }
    }
    if (is.list(lR)) {
      for (i in 1:n.cg) {
        wi <- w[Roff[i]:(Roff[i] + size.cg[i] - 1)]
        lR[[i]] <- as.vector(wi) * t(as.vector(wi) * lR[[i]]) # Matrix lR
      }
    }
    else if (is.matrix(lR)) {
      lR <- as.vector(w) * t(as.vector(w) * lR)
    }
    else {
      lR <- w^2 * lR
    }
    if (is.list(lR)) {
     R <- lR[[1]]
     for (k in 2:n.cg) {
      R <- bdiag(R,lR[[k]])
     }
     R <- as.matrix(R)
    }
    else{
      R <- diag(lR)
      }
  }else{
    R <- getVarCov(fit,type = "conditional",individual = 1)[[1]]
    for (i in 2:length(as.numeric(unique(id)))) {
    R <- as.matrix(bdiag(R,getVarCov(fit,
                                   type = "conditional",individual = i)[[1]] ) )
    }
  }
#############################################################################################################
## Construction of covariance matrix of Y (Here denoted as V; in the paper it is denoted \Omega)           ##
#############################################################################################################
  
  V <- (Z %*% Gam %*% t(Z)) + R
  iV <- solve(V)                                                

#############################################################################################################
## Construction of the Q matrix                                                                            ##
#############################################################################################################  

  varbeta <- solve((t(X) %*% iV %*% X))
  Q <- (iV - iV %*% X %*% (varbeta) %*% t(X) %*% iV ) 
  zq <- t(Z) %*% Q
  norm.frob.ZtQ <- sum(diag(zq %*% t(zq)))

#############################################################################################################
## EBLUE and EBLUP                                                                                         ##
############################################################################################################# 
   
  eblue <- as.vector(fixef(fit))
  eblup <- Gam %*% t(Z) %*% iV %*% (y - X %*% eblue)

############################################################################################################
## Residual analysis                                                                                      ##
############################################################################################################  
  
  predm <- X %*% eblue                       # Predicted values for expected response
  predi <- X %*% eblue + Z %*% eblup         # Predicted values for units
  resm <- (y - predm)                        # Marginal residuals
  resc <- (y - predi)                        # Conditional residuals

############################################################################################################
## Variance of marginal residuals                                                                         ##
############################################################################################################ 
  
  var.resm <- V - X %*% solve(t(X) %*% iV %*% X) %*% t(X) 
 
###########################################################################################################
## Standardized marginal residuals                                                                       ##
###########################################################################################################
  
  resmp <- resm/sqrt(diag(var.resm))

###########################################################################################################
## Variance of conditional residuals                                                                     ##
########################################################################################################### 
  var.resc <- R %*% Q %*% R

##########################################################################################################
## Fraction of confounding                                                                              ##
##########################################################################################################  
 
   ident <- diag(N)
  auxnum <- (R %*% Q %*% Z %*% Gam %*% t(Z) %*% Q %*% R)
  auxden <- R %*% Q %*% R
      CF <- diag(auxnum)/diag(auxden)

##########################################################################################################
## Standardized conditional residuals                                                                   ##
##########################################################################################################    
  
  rescp <- resc/sqrt(diag(var.resc))

##########################################################################################################
## Mahalanobis distance                                                                                 ##
##########################################################################################################
  
  if (n.levels > 1) {
    aux <- Gam %*% t(Z) %*% Q %*% Z %*% Gam
    qm <- q - 1
    dm <- matrix(0,length(CCind),1)
    gbi <- aux[1:(q[1]),(1:q[1])]
    eblupi <- eblup[1:(q[1]),]
    dmi <- t(eblupi) %*% ginv(gbi) %*% eblupi
    dm[1] <- dmi
    for (j in 2:length(CCind)) {
      gbi <- aux[((j - 1)*q[(j - 1)] + 1 ):(q[j] + q[(j - 1)]),((j - 1)*q[(j - 1)] + 1 ):(q[j] + q[(j - 1)])]
      eblupi <- eblup[((j - 1)*q[(j - 1)] + 1 ):(q[j] + q[(j - 1)]),]
      dmi <- t(eblupi) %*% ginv(gbi) %*% eblupi
      dm[j] <- dmi
    }
  }else{
    aux <- Gam %*% t(Z) %*% Q %*% Z %*% Gam
    qm <- q - 1
    dm <- matrix(0,n,1)
    
    for (j in 1:length(CCind)) 
    {
      if (q == 1)
      {
        gbi <- aux[j,j]
        eblupi <- eblup[(q*j - qm):(q*j)]
        dmi <- t(eblupi) %*% ginv(gbi) %*% eblupi
        dm[j] <- dmi
      }
      else
      {
        gbi <- aux[(q*j - qm):(q*j),(q*j - qm):(q*j)]
        cat(gbi,'\n','\t')
        eblupi <- eblup[(q*j - qm):(q*j)]
        dmi <- t(eblupi) %*% ginv(gbi) %*% eblupi
        dm[j] <- dmi
      }
    }
    
  }

  LSdm <- as.numeric(quantile(dm,prob = 0.75) + 1.5*(quantile(dm,prob = 0.75) - quantile(dm,prob = 0.25)))
  LS2dm <- 2*mean(dm)

############################################################################################################
## Modified Lesaffre-Verbeke index                                                                        ##
############################################################################################################
  
  lesverb <- rep(0,length(CCind)) 
    auxni <- as.vector(vecni)
  for (t in 1:length(CCind)) { 
    li <- sum(vecni[1:t - 1]) + 1
    ls <- sum(vecni[1:t])
    if (vecni[t] == 1) {
    
         auxr2 <- solve(sqrt(var.resm[li:ls,li:ls]))
            Ri <- (auxr2) %*% resm[li:ls]
          auxt <- diag(vecni[t]) - Ri %*% t(Ri)
    lesverb[t] <- sqrt(sum(diag(auxt %*% t(auxt))))
  }
  else
  {  
    
         auxr2 <- solve(sqrt.matrix(var.resm[li:ls,li:ls]))
            Ri <- auxr2 %*% resm[li:ls]
          auxt <- diag(vecni[t]) - Ri %*% t(Ri)
    lesverb[t] <- sqrt(sum(diag(auxt %*% t(auxt))))
  } 
}

     lesverb <- lesverb/((as.numeric((table(id)))))
   LSlesverb <- as.numeric(quantile(lesverb,prob = 0.75) + 1.5*(quantile(lesverb, prob = 0.75) - quantile(lesverb,prob = 0.25)))
  LS2lesverb <- 2*mean(lesverb)

############################################################################################################
## Least confounded residuals                                                                             ##
############################################################################################################  

  R.half <- sqrt.matrix(R)
  auxqn <- eigen((R.half %*% Q %*% R.half), symmetric = T, only.values = FALSE) 

  lt <- sqrt(solve(diag((auxqn$values[1:(N-p)])))) %*% t(auxqn$vectors[1:N,1:(N-p)]) %*% solve(sqrt.matrix(R[1:N,1:N]))

  var.resmcp <- lt %*% var.resc[1:N,1:N] %*% t(lt)
  resmcp <- (lt %*% resc[1:N] )/sqrt(diag(var.resmcp))

#############################################################################################################
## Cook's Distance                                                                                        ##
#############################################################################################################
  
      In <- diag(rep(1,N))               
      Dc <- matrix(c(rep(0,3*N)),N,3)
  Dccond <- rep(0,N) 
    Dcor <- rep(0,N)
    k <- (length(CCind) - 1)*max(q) + p
    for (i in 1:N) {
              Ui <- In[,i] 
            auxi <- solve(crossprod(Ui,crossprod(t(Q),Ui)), crossprod(Ui,crossprod(t(Q),y)) )    
           aux2i <- solve( crossprod(X,crossprod(t(iV),X)), crossprod(X, (crossprod(t(iV),crossprod(t(Ui),auxi))) ) )  
           aux3i <- crossprod(t(Gam),crossprod(Z,crossprod(t(Q),crossprod(t(Ui),auxi)))) 
         Dc[i,1] <- crossprod(aux2i,crossprod(X,crossprod(t(X),aux2i))  )/k
         Dc[i,2] <- crossprod(aux3i,crossprod(Z,crossprod(t(Z),aux3i)))/k 
         Dc[i,3] <- crossprod(2*aux2i,crossprod(X,crossprod(t(Z),aux3i)))/k  
        }

    DC1 <- Dc[,1];
  LSDC1 <- as.numeric(quantile(DC1,prob = 0.75) + 1.5*(quantile(DC1,prob = 0.75) - quantile(DC1,prob = 0.25)))
#
    DC2 <- Dc[,2];
  LSDC2 <- as.numeric(quantile(DC2,prob = 0.75) + 1.5*(quantile(DC2,prob = 0.75) - quantile(DC2,prob = 0.25)))
#
    DC3 <- Dc[,3]
  LSDC3 <- as.numeric(quantile(DC3,prob = 0.75) + 1.5*(quantile(DC3,prob = 0.75) - quantile(DC3,prob = 0.25)))
#
  Dccond <- DC1 + DC2 + DC3  
  LSDccond <- as.numeric(quantile(Dccond,prob = 0.75) + 1.5*(quantile(Dccond, prob = 0.75) - quantile(Dccond,prob = 0.25)))

###########################################################################################################
## Leverage                                                                                              ##
###########################################################################################################
  
  Covb <- fit$varFix

  L1 <- X %*% Covb %*% t(X) %*% iV                                                    # Generalized marginal leverage matrix
  L1d <- diag(L1)                                                                     # Marginal leverage for observations
  LSL1d <- as.numeric(quantile(L1d,prob = 0.75) + 1.5*(quantile(L1d,prob = 0.75) - quantile(L1d,prob = 0.25)))
  L1i <- as.numeric(as.matrix(aggregate(L1d, by = list(id), FUN = mean)[2]))
  # trace.L1d<- sum(L1d)
  LSL1i <- as.numeric(quantile(L1i,prob = 0.75) + 1.5*(quantile(L1i,prob = 0.75) - quantile(L1i,prob = 0.25)))
  
  
  L <- L1 + Z %*% Gam %*% t(Z) %*% Q                                                  # Generalized joint leverage matrix
  Ld <- diag(L)                                                                       # Joint leverage for observations
  LSLd <- as.numeric(quantile(Ld,prob = 0.75) + 1.5*(quantile(Ld,prob = 0.75) - quantile(Ld,prob = 0.25)))
  Li <- as.numeric(as.matrix(aggregate(Ld, by = list(id), FUN = mean)[2]))  
  LSLi <- as.numeric(quantile(Li,prob = 0.75) + 1.5*(quantile(Li,prob = 0.75) - quantile(Li,prob = 0.25)))

  L2 <- Z %*% Gam %*% t(Z)                                                          # Generalized random component leverage matrix 
  L2d <- diag(L2)                                                                   # Random component leverage for observations
  LSL2d <- as.numeric(quantile(L2d,prob = 0.75) + 1.5*(quantile(L2d,prob = 0.75) - quantile(L2d,prob = 0.25)))
  L2i <- as.numeric(as.matrix(aggregate(L2d, by = list(id), FUN = mean)[2]))  
  # trace.L2d<- sum(L2d)
  LSL2i <- as.numeric(quantile(L2i,prob = 0.75) + 1.5*(quantile(L2i,prob = 0.75) - quantile(L2i,prob = 0.25)))

##########################################################################################################
## This function constructs QQ plots for normality of random eefects                                    ##
##########################################################################################################
  
  qqPlot2 <- function(x, distribution="norm", ..., ylab=deparse(substitute(x)),
                  xlab=paste(distribution, "quantiles"), main = NULL, 
                  las = par("las"),
                  envelope = .95,  
                  col = palette()[1], 
                  col.lines = palette()[2], lwd = 2, pch = 1, cex = par("cex"),
                  cex.lab = par("cex.lab"), cex.axis = par("cex.axis"), 
                  line = c("quartiles", "robust", "none"), 
                  labels = if (!is.null(names(x))) names(x) else seq(along = x),
                  id.method = "y", 
                  id.n = if (id.method[1] == "identify") Inf else 0,
                  id.cex = 1, id.col=palette()[1], grid = TRUE)
{
  line <- match.arg(line)
  good <- !is.na(x)
  ord <- order(x[good])
  ord.x <- x[good][ord]
  ord.lab <- labels[good][ord]
  q.function <- eval(parse(text = paste("q", distribution, sep = "")))
  d.function <- eval(parse(text = paste("d", distribution, sep = "")))
  n <- length(ord.x)
  P <- ppoints(n)
  z <- q.function(P, ...)
  plot(z, ord.x, type = "n", xlab = xlab, ylab = ylab, main = main, las = las,cex.lab = cex.lab, cex.axis = cex.axis)
  if (grid) {
    grid(lty = 1, equilogs = FALSE)
    box()}
  points(z, ord.x, col = col, pch = pch, cex = cex)
  if (line  == "quartiles" || line == "none") {
    Q.x <- quantile(ord.x, c(.25,.75))
    Q.z <- q.function(c(.25,.75), ...)
    b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
    a <- Q.x[1] - b*Q.z[1]
    abline(a, b, col = col.lines, lwd = lwd)
  }
  if (line == "robust") {
    coef <- coef(rlm(ord.x ~ z))
    a <- coef[1]
    b <- coef[2]
    abline(a, b)
  }
  conf <- if (envelope == FALSE) .95 else envelope
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (b/d.function(z, ...))*sqrt(P*(1 - P)/n)
  fit.value <- a + b*z
  upper <- fit.value + zz*SE
  lower <- fit.value - zz*SE
  if (envelope != FALSE) {
    lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
    lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
  }
}

###########################################################################################################  
## This function constructs the diagnostic plots                                                         ##
###########################################################################################################  

  plotg = function(plotid){
    cat("\n To select the graphic use plotid \n
1- Modified Lesaffre-Verbeke index versus unit indices
2- Standardized marginal residuals versus fitted values and corresponding histogram
3- Mahalanobis distance versus unit indices
4- Chi-squared QQ plot for Mahalanobis distance
5- Standardized conditional residuals versus fitted values and corresponding histogram
6- Normal QQ plot and histogram for standardized least confounded conditional residuals
7-  Cook's conditional distance versus observation indices
8-  Cook's conditional distance 1 (D1i) versus observation indices
9 - Cook's conditional distance 2 (D2i) versus observation indices
10- Cook's conditional distance 3 (D3i) versus observation indices
13- Generalized joint leverage (L) versus unit indices
11- Generalized marginal leverage (L1) versus unit indices
12- Generalized random component leverage (L2) versus unit indices
14- Generalized joint leverage [Li(jj)] versus observation indices
15- Generalized marginal leverage [L1i(jj)] versus observation indices
16- Generalized random component leverage [L2i(jj)] versus observation indices
\n")
    cat("\n Graph plotting", plotid)
    
    if (plotid == 1)
    {
      par(mfrow = c(1,1),mar = c(11, 5, 1, 2))
      plot(lesverb,ylab = expression(paste("Modified Lesaffre-Verbeke index")),
           xlab = "Unit index", cex = 1.2, cex.lab = 1.5, cex.axis = 1.3, 
           pch = 20, ylim = c(0,2*max(abs(range(lesverb)))))
      abline(h = LSlesverb,lty = 2)                            # 3rd quartile + 1.5*interquartile interval
#     abline(h = LS2lesverb,lty = 3)                           # 2*mean(lesverbp)
      index = which(lesverb > LSlesverb)
#      index1<-subject[index]
      if (length(index) > 0)
      {
        text(index, lesverb[index], index, adj = c(1,-.5), cex = 1.0, font = 2)
      }
    }
    
    if (plotid == 2)
    {
      par(mfrow = c(1,2), mar = c(10, 5, 1, 2))
      plot(predm, resmp, xlab = expression(paste("Marginal fitted values")),
           ylab = expression(paste("Standardized marginal residuals")), 
           pch = 20, cex = 1.2, cex.lab = 1.2, cex.axis = 1.3, 
           ylim = c(-1.3*max(abs(range(resmp))),1.3*max(abs(range(resmp)))))
      abline(h = 0, lty = 3)
      abline(h = -limit, lty = 3)
      abline(h = limit, lty = 3)
      index = which(abs(resmp) > limit)
      if (length(index) > 0)
      {
      text(predm[index], resmp[index], paste(id[index], obs[index], sep = "."),
           adj = c(1,-.5), cex = 1.0, font = 2)
      }      
      hist(resmp,breaks = c(-6:6) ,freq = F, main = "", 
           xlab = expression(paste("Standardized marginal residuals")), 
           cex = 0.9, cex.lab = 1.2, cex.axis = 1.3)
     }
    
    if (plotid == 3)
    {
      par(mfrow = c(1,1), mar = c(12, 5, 1, 2))
      quant.chisq <- qqPlot2(dm, distribution = 'chisq', df = q, pch = 20, 
                             cex = 1.2, cex.lab = 1.5, cex.axis = 1.3,
                             ylab = expression(paste("Mahalanobis distance quantiles")),
                             xlab = "Chi-squared quantiles")
    }
    
    if (plotid == 4)
    {
      par(mfrow = c(1,1), mar = c(12, 5, 1, 2))
      plot(dm, ylab = expression(paste("Mahalanobis distance")), 
           xlab = "Unit index", 
           pch = 20, ylim = c(0,2*max(dm)), 
           cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)
      abline(h = LSdm, lty = 2)                                 # 3rd quartile + 1.5*interquartile interval
  #    abline(h = LS2dm, lty = 3) #2*mean(dmp)
      index = which(dm > LSdm)
      index1 <- subject[index]
      if (length(index) > 0)
      {
        text(index,dm[index], index1, adj = c(1,-.5), cex = 1.0, font = 2)
      }
    }
    
    if (plotid == 5)
    {
      par(mfrow = c(1,2), mar = c(11, 5, 1, 2))
      plot(predi, rescp, xlab = expression(paste("Predicted values")), 
           cex = 1.2, cex.lab = 1.3, cex.axis = 1.3, 
           ylab = expression(paste("Standardized conditional residuals")), 
           pch = 20, ylim = c(-1.3*max(abs(range(rescp))),
                              1.3*max(abs(range(rescp)))))
      abline(h = 0,lty = 3)
      abline(h = limit,lty = 3)
      abline(h = -limit,lty = 3)
      index = which(abs(rescp) > limit)
      index1 <- subject[index]
      if (length(index) > 0)
      {
        text(predi[index], rescp[index], 
             paste(id[index], obs[index],sep = "."), adj = c(1,-.5), 
             cex = 1.0, font = 2)
      }
      hist(rescp, freq = F,breaks = c(-7:7), main = "", 
           xlab = expression(paste("Standardized conditional residuals")), 
           cex = 1.0, cex.lab = 1.3, cex.axis = 1.3)
    }
    
    if (plotid == 6)
    {
      par(mfrow = c(1,2), mar = c(11, 5, 3, 2))
      qqPlot2(resmcp, ylab = "Standardizedd least confounded residuals", 
              xlab = "N(0,1) quantiles", pch = 20, cex = 1.2, cex.lab = 1.2,
              cex.axis = 1.3)      
      hist(resmcp, freq = F,breaks = c(-6:6), 
           xlab = "Std least confounded residuals", 
           main = "", cex = 1.0, cex.lab = 1.2, cex.axis = 1.2, pch = 20)
    }
    
    if (plotid == 7)
    {
      par(mfrow = c(1,1))
      plot(Dccond,pch = 16, xlab = "Observation index",
           ylab = "Cook's conditional distance", cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = LSDccond,lty = 2) # 3rd quartile + 1.5*interquartile interval  
      index = which(Dccond > LSDccond)
      index1 <- id[index]
      if (length(index1) > 0)
      {
        text(index, Dccond[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    
    if (plotid == 8)
    {
      par(mfrow = c(1,1))
      plot(DC1,pch = 16, xlab = "Observation index",
           ylab = "Cook's conditional distance D1i", ylim = c(0,max(DC1)), cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = LSDC1,lty = 2)                                          # 3rd quartile + 1.5*interquartile interval
      index = which(DC1 > LSDC1)
      if (length(index) > 0)
      {
        text(index, DC1[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    
    if (plotid == 9)
    {
      par(mfrow = c(1,1))
      plot(DC2,pch = 16,xlab = "Observation index",
           ylab = "Cook's conditional distance D2i",ylim = c(0,max(DC2)), cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = LSDC2,lty = 2)                                          # 3rd quartile + 1.5*interquartile interval
      index = which(DC2 > LSDC2)
      if (length(index) > 0)
      {
        text(index, DC2[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    
    if (plotid == 10)
    {
      
      plot(abs(DC3),pch = 16,xlab = "Observation index",
           ylab = "Cook's conditional distance D3i",ylim = c(0,(1.5*max(DC3))), cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = LSDC3,lty = 2)                                           # 3rd quartile + 1.5*interquartile interval
      index = which(abs(DC3) > LSDC3)
      if (length(index) > 0)
      {
        text(index, abs(DC3)[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    
    if (plotid == 11)
    {
      par(mfrow = c(1,1))
      plot(Li,pch = 16,xlab = "Unit index",
           ylab = "Generalized joint leverage Li",ylim = c(0,(2*max(Li))), cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = ((2*p)/n),lty = 3)
      abline(h = LSLi,lty = 2)                                            # 3rd quartile + 1.5*interquartile interval
      index = which(Li > LSLi) 
      if (length(index) > 0)
      {
        text(index, Li[index], paste(subject[index]), adj = c(1,-.5), 
             cex = .8, font = 2)
      }
    }
   
    if (plotid == 12)
    {
      par(mfrow = c(1,1))
      plot(L1i,pch = 16,xlab = "Unit index",
           ylab = "Generalized marginal leverage L1i", ylim = c(0,2*max(L1i)), cex = 1.2, cex.lab = 1.5,
           cex.axis = 1.3)
      abline(h = ((2*p)/n),lty = 3)
      abline(h = LSL1i,lty = 2)                                          # 3rd quartile + 1.5*interquartile interval
      index = which(L1i > LSL1i) 
 
       if (length(index) > 0)
      {
        text(index, L1i[index], paste(subject[index]), adj = c(1,-.5), 
             cex = .8, font = 2)
      }
    }
    if (plotid == 13)
    {
      par(mfrow = c(1,1))
      plot(L2i,pch = 16,xlab = "Unit index",
           ylab = "Generalized random component leverage L2i",
           ylim = c(0,(2*max(L2i))), cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)
      abline(h = ((2*sum(L2d))/n),lty = 3)
      abline(h = LSL2i,lty = 2)                                          # 3rd quartile + 1.5*interquartile interval
      index = which(L2i > LSL2i)    
      if (length(index) > 0)
      {
        text(index, L2i[index], paste(subject[index]), adj = c(1,-.5), 
             cex = .8, font = 2)
      }
    }
    
    if (plotid == 14)
    {
      par(mfrow = c(1,1))
      plot(Ld,pch = 16,xlab = "Observation index",
           ylab = "Generalized joint leverage Li(jj)",
           ylim = c(0,(1.5*max(Ld))), cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)
      abline(h = LSLd,lty = 2)                                          # 3rd quartile + 1.5*interquartile interval
      index = which(Ld > LSLd) 
      if (length(index) > 0)
      {
        text(index, Ld[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    
    if (plotid == 15)
    {
      par(mfrow = c(1,1))
      plot(L1d,pch = 16,xlab = "Observation index",
           ylab = "Generalized marginal leverage L1i(jj)",
           ylim = c(0,(1.5*max(L1d))), cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)
      abline(h = LSL1d,lty = 2)                                        # 3rd quartile + 1.5*interquartile interval
      index = which(L1d > LSL1d) 
   #   abline(h = ((2*p)/n),lty = 2)
   #   index = which(L1d>((2*p)/n))
      if (length(index) > 0)
      {
        text(index, L1d[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
    if (plotid == 15)
    {
      par(mfrow = c(1,1))
      plot(L2d,pch = 16,xlab = "Observation index",
           ylab = "Generalized random component leverage L2i(jj)",
           ylim = c(0,(1.5*max(L2d))), cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)
      abline(h = LSL2d,lty = 2)                                       # 3rd quartile + 1.5*interquartile interval
      index = which(L2d > LSL2d) 
      if (length(index) > 0)
      {
        text(index, L2d[index], paste(id[index], obs[index],sep = "."), 
             adj = c(1,-.5), cex = .8, font = 2)
      }
    }
 
  }
  if (is.null(plotid)) {
    cat("\n To choose plot, select plotid \n
1- Modified Lesaffre-Verbeke index versus unit indices
2- Standardized marginal residuals versus fitted values and corresponding histogram
3- Mahalanobis distance versus unit indices
4- Chi-squared QQ plot for Mahalanobis distance
5- Standardized conditional residuals versus fitted values and corresponding histogram
6- Normal QQ plot and histogram for standardized least confounded conditional residuals
7-  Cook's conditional distance versus observation indices
8-  Cook's conditional distance 1 (D1i) versus observation indices
9 - Cook's conditional distance 2 (D2i) versus observation indices
10- Cook's conditional distance 3 (D3i) versus observation indices
13- Generalized joint leverage (L) versus unit indices
11- Generalized marginal leverage (L1) versus unit indices
12- Generalized random component leverage (L2) versus unit indices
14- Generalized joint leverage [Li(jj)] versus observation indices
15- Generalized marginal leverage [L1i(jj)] versus observation indices
16- Generalized random component leverage [L2i(jj)] versus observation indices
        \n")
    return(1);  
  }
  
#####################################################################################################
# Generation of diagnostic plots                                                                   ##
#####################################################################################################
  for (g in plotid) {
   plotg(g)
    cat("\n Press ENTER to continue...")
    readline()
  }
  
useful.results <- list(
     std.marginal.residuals = cbind(Subject = id,Predicted = as.numeric(resmp)),
     std.conditional.residuals = cbind(Subject = id,Predicted = as.numeric(rescp)),
     mahalanobis.distance = cbind(Subject = as.numeric(unique(id)),md = as.numeric(dm)),
     lesaffreverbeke.measure = cbind(Subject = as.numeric(unique(id)),LV.m = lesverb),
     least.confounded.residuals = cbind(l.c.r = as.numeric(resmcp)),
     cook.conditional.distance = cbind(Subject = id,DCCond = Dccond),
     DC1 = cbind(Subject = id,DC1 = Dc[,1]),
     DC2 = cbind(Subject = id,DC2 = Dc[,2]),
     DC3 = cbind(Subject = id,DC3 = Dc[,3]),
     L1 = cbind(Subject = subject,L1 = L1i),
     L2 = cbind(Subject = subject,L1 = L2i),
     L = cbind(Subject = subject,L = Li),
     L1ijj = cbind(Subject = id,L1ijj = L1d),
     L2ijj = cbind(Subject = id,L2ijj = L2d),
     L3ijj = cbind(Subject = id,L3ijj = Ld),
     Predi =  cbind(Subject = id,Predi = predi),
     Fraction.Confounding  =  cbind(Subject = id,Frac.Conf = CF),            
     Norm.Frob.ZtQ = norm.frob.ZtQ, 
     mat.X = X,
     lZi = lZi,
     lgi = lgi,
     mat.Z = Z,
     Gam = Gam,
     R = R,
     V = V,
     mat.iV = iV)
     

}

