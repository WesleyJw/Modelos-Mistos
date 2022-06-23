#' ---
#' title: "Modelos Mistos em R"
#' author: "Wesley Lima"
#' date: "05/10/2020"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' ## Problemática
#' 
#' O que se prentende fazer nesta análise é avaliar a **riquezas de espécies** da **macrofauna** em praias da costa da Holanda.  
#' 
#' **Riqueza de espécies** ou riqueza específica é um termo utilizado na ecologia para designar o número de espécies de uma determinada região, sendo a unidade fundamental para a avaliação da homogeneidade de um ambiente.
#' 
#' Em biologia marinha e limnologia, chama-se **macrofauna** ou macrobentos ao conjunto dos animais que vivem no substrato dos ecossistemas aquáticos e que possuem tamanho maior ou igual a 0,5 mm, geralmente são visíveis a olho nu. 
#' 
#' 
#' ### Objetivo
#' 
#' Avaliar se existe diferenças na riqueza de espécies em praias da Holanda.
#' 
#' ## Conjunto de Dados
#' 
#' O conjunto de dados foi obtido em 9 praias da costa da Holanda, para cada uma das praias foram tomadas informações em cinco sítios diferentes. Em cada sítio existe informação sobre a altura da estação de amostragem em relação a altura média da maré (**NAP**), e sobre o índice de exposição da praia (**Exposure**). 
#' 
#' 1. **Variável resposta**: 
#' 
#'     - Riqueza de espécies;
#'     
#' 2. **Variáveis explicativas**:
#' 
#'     - Praias (Categórica);
#'     - Sítios (Categórica);
#'     - NAP (Contínua);
#'     - Exposure (Contínua).
#' 
#' ### O que é um **efeito aleatório** em modelos mistos?
#' 
#' É um efeito que o pesquisador não consegue controlar, não é possível isolar este efeito e colocá-lo em um tratamento ou bloco. Desta forma, a mehor maneira é isolá-lo durante a análise. Desta forma, em modelos mistos o **efeito aleatório** é uma variável que agrupa os dados e que seu efeito sobre a **variável resposta** não interessa diretamente.
#' 
#' ### O que é um **efeito fixo** em modelos mistos?
#' 
#' Os efeitos fixos são **variáveis preditoras** na qual estamos interessados sobre o efeito que ela causa na variável resposta.    
#' 
#' Em geral dados que possuem cortes longitudinais, requerem o uso de modelos mistos para sua análise. Por exemplo, em um experimento com clones de *Eucalyptus* o volume foi medido desde a indade de 18 meses, até a idade de 90 meses, portanto as mesmas árvores foram medidas ao longo do tempo, logo existe uma dependência temporal entre elas.
#' 
#' ### Como definir um modelo com efeitos mistos?
#' 
#' Os efeitos mistos podem ser atribuídos tanto às variáveis preditoras quanto ao intercepto do modelo. É comum que em modelos lineares o efeito aleatório seja atribuído ao intercepto do modelo. Isso implica que para cada nível do fator aleatório teremos uma reta começando de um ponto particular. consideremos o seguinte modelo misto para estudar a riqueza de espécies em função da variável **NAP** em diferentes praias (efeito aleatório):
#' 
#' $$Riq.a_i = \alpha + b_i + \beta \cdot NAP + \epsilon_i$$
#' 
#' Temos que a riqueza pode ser explicada em detalhes para cada praia $i$, logo a riqueza em cada praia é explicada por meio do efeito fixo $\beta \cdot NAP$ e pelo efeito aleatório $b_i$, este valor é somado ao intercepto $\alpha$ para formar o intercepto do modelo. Sendo assim, é possível destacar que o parâmetro $b_i$ permite que cada praia apresente uma explicação diferente a partir da relação $\beta \cdot NAP$. $\epsilon_i$ é a componente de erro aleatório do modelo.
#' 
#' 
#' ### Obtenção e manipulação dos dados
#' 
#' O conjunto de dados pode ser obtido de [Zuur et al., 2009](http://highstat.com/Books/Book3/RBook.zip). Abra o arquivo zip e extraia o conjunto de dados por nome *RIKZ2.txt*, aqui o arquivo foi extraído na pasta chamada *Dataset*.
#' 
## ------------------------------------------------------------
dados <- read.table("Dataset/RIKZ2.txt", h = T, row.names = 1, as.is = T)
str(dados)
head(dados)
#View(dados)

#' 
#' Como foi definido a variável **Exposure** é do tipo categórica, no entanto, é apresentado como contínua com três valores de referência (8, 10 e 11) que representam (baixa, média e alta), porém o fator (8 - baixo) só foi registrado uma única vez, desta forma ele será classificado como 10 e a nova variável será chamada de *fExposure* com as classes (low e high).  
#' 
## ------------------------------------------------------------
#modificar a linha da variavel Exposure que contem o valor 8 para o valor 10
dados$Exposure[dados$Exposure == 8] <- 10

#Criando a nova variavel
dados$NExposure <- as.factor(ifelse(dados$Exposure == 10, "low", "high"))

#visualizando as 10 primeiras linhas do banco de dados
head(dados, 10)

#' 
#' 
#' ## Modelos Mistos em R
#' 
#' Para o ajuste dos modelos mistos utilizaremos o pacote *lme4* proposto por [Bates et al., 2014](https://arxiv.org/abs/1406.5823), com este pacote é possível ajustar modelos lineares, lineares generalizados e não lineares com efeitos mistos.
#' 
## ----message=F-----------------------------------------------
#carregar o pacote
library(lme4)
#Se nao o tiver instalado descomente a linha abaixo para executar o comando 
#install.packages("lme4", dependencies = T)

#' 
#' Vamos criar um modelo no qual possamos verificar o efeito de *NAP* na **riqueza de espécies** considerando a correlação espacial entre as 9 praias onde as amostras foram coletadas. Vamos tomar inicialmente o seguinte  modelo $Riq.a_i = \alpha + b_i + \beta \cdot NAP + \epsilon_i$, vejamos como podemos escreve-lo no R.
#' 
## ------------------------------------------------------------
#ajustando o modelo
rich.model <- lmer(Richness ~ NAP + (1 | Beach), data = dados)
#vendo um resumo das estatísticas do modelo
summary(rich.model)

#' 
#' ### Como interpretar os resultados?
#' 
#' 
#' Inicialmente vamos verificafar o quanto da variabilidade da variável resposta **Riqueza - Richness** está sendo devida pelos nossos **efeitos aleatórios** (*Random effects*): **praias - Beach** e **resíduos - $\epsilon$**. O primeiro grupo é referente ao desvio - padrão do efeito aleatório, ou seja, as diferenças de interceptos entre as prais. No segundo grupo é possivel visualizar o quanto da variabilidade não é prevista pela praia (efeito aleatório) nem pelo *NAP* (efeito fixo), ou seja, estamos nos referindo ao erro aleatório do modelo definido por $\epsilon$ na nossa expressão. 
#' 
#' Os **efeitos fixos** indicados pelo termo *Fixed effects* mostras os valores dos coeficientes do modelo, tanto o valor de $\hat{\alpha}$ (intercepto), que indica qual será a **Riqueza** caso o valor de *NAP* seja 0. E o coeficiente $\hat{\beta}$ responsável pela inclinação da reta de regressão. 
#' 
#' E o coeficiente de determinação $R^2$ onde eu acho? E os p-Valores dos coeficientes sumiram? Bem, inicialmente não precisamos nos preocupar com isso. Veja que o objetivo do estudo é verificar se existem diferenças significativas da riqueza entre as praias. Aqui precisamos nos preocupar com: as variáveis aleatórias são significativos? Deveriamos acrescentar mais variáveis como efeito fixo ou aleatório? Perguntas detes tipo podem ser respondidas ajustando diferentes modelos e depois fazendo uma análise de variância para verificar se o melhor modelo é aquele com mais ou menos variáveis na sua estrutura.
#' 
#' Para facilitar um pouco a nossa vida podemos verificar se os coeficientes dos efeitos fixos são significativos utilizando o pacote *lmerTest*, para calcular o p-Valor com este pacote basta carrega-lo e executar novmente o ajuste do modelo.
#' 
## ----  message=F---------------------------------------------
#carregando o pacote
library(lmerTest)

#reajutando o modelo
rich.model <- lmer(Richness ~ NAP + (1 | Beach), data = dados)
#resumo do modelo
summary(rich.model)

#' 
#' 
#' ## Escolha dos efeitos aleatórios
#' 
#' É comum que se pense em usar como efeitos aleatórios informações correlaconadas temporalmente, geotemporalmente, etc. No entanto, cabe algumas salientações sobre as escolhas corretas destes efeitos. Primeiro, é importante que a variável que será usada como efeito aleatório seja do tipo categórica. Segundo, o número de fatores deve ser superior ou igual a **cinco**, cabe uma resalva neste valor, pois o mesmo ainda não é um consenso, entretanto quanto mais faotres melhor. Questões mais teoricas devem ser levadas em consideração, por exemplo, o intuíto de se empregar um modelo misto está relacionado com a diminuição do número de **graus de liberdade**, já que, se o efeito for considerado aleatório perderemos apenas um grau de liberdade na estimação da variância, já se o efeito for fixo perderemos $k-n$ graus de liberdade, em que $k$ representa o múmero de fatores do efeito aleatório. Porém se o $n$ amostral for grande a perda de um maior número de graus de liberdade é desprezível. 
#' 
#' ## Ajustando Modelos Lineares Mistos com duas Variáveis e Interação
#' 
#' Como nosso conjunto de dados não nos permite incluir mais efeitos aleatórios, ajustaremos novos modelos com mais efeitos fixos e um modelo sem efeito nenhum efeito aleatório, também ajustaremos um modelo com o coeficiente angular aleatório.
#' 
## ------------------------------------------------------------
##Ajustando os novos modelos

m0 <- lmer(Richness ~ NAP + NExposure + (1 | Beach), data = dados)
m1 <- lmer(Richness ~ NAP + NExposure + (1 + NExposure|Beach), data = dados)
m2 <- lmer(Richness ~ NAP * NExposure + (1 | Beach), data = dados)
m3 <- lmer(Richness ~ NAP * NExposure + (1 + NAP*NExposure|Beach), data = dados)


#' 
#' ### O que significa cada modelo? Como interpretar estes modelos?
#' 
#' 1. No modelo **m0**, temos que a **Riqueza de espécies** é explicada pela variável *NAP* e pela variável *NExposure*, considerando que teremos interceptos aleatórios em cada praia. 
#' 
#' 2. Já no modelo **m1**, temos que além da **Riqueza de espécies** ser explicada pelas duas variáveis *NAP* e *NExposure* com intercepto aleatório por praias, também teremos um coeficiente angular aleatório para cada nível de *NExposure* por praia. 
#' 
#' 3. Em relação a interação podemos construir diferentes hipótese a qual queremos testar. Por exemplo considrando o modelo **m2** o qual possui a interação $NAP * NExposure$, não quremos verificar se *NEposure* diferentes influenciam na **Riqueza de espécies**, mas sim, queremos verificar a hipótese de que a variável *NAP* na presença de *NExposure* teria um efeito mais acentuado na **Riqueza de espécies**.   
#' 
#' OBS: O que representa a mensagem no ajuste do modelo **m1** e **m3**? 
#' ```
#' boundary (singular) fit: see ?isSingular
#' ```
#' Devemos proceder com cuidado ao receber esta mensagem, pois ela indica que o modelo foi sobreajustado, ou seja, o modelo tem uma capacidade muito grande de realizar estimativas acertadas sobre a amostra empregada, mas quando empregado em outro conjundo de dados poderá falhar siginificativamente. Sendo assim, o modelo não é bom em fazer generalizações, o que em geral não queremos que aconteça, desta forma, devemos desenvolver modelos com estruturas mais simples em busca da convergência sem restrições.
#' 
#' ## Coeficientes angulares (Slopes) e Efeitos Aleatórios
#' 
#' Já sabemos que quando queremos adicionar um **coeficiente angular** *aleatório* utilizamos a estrutura geral:
#' 
#' ```
#' Richness ~ NAP + NExposure + (1 + NExposure|Beach)
#' ```
#' 
#' Vamos nos concentrar no componente aleatório ```(1 + NExposure|Beach)```, como interpretá-lo?
#' 
#' Bem, o que este componente nos diz é que temos um **intercepto aleatório** para a variável *Beach* mais um **coeficiente angular aleatório** por *NExposure* por *Beach*, ou seja, para cada *Beach* queremos saber o quanto varia a **Riqueza de espécies** na presença de *low* e de *high* da variável *NExposure*. Fazendo uma simples verificação na base de dados podemos ver que o modelo é inconcistente, pois na maioria das praias (*Beach*) não foram coletados dados de *NExposure* com valores *low* e *high*. 
#' 
#' Uma forma mais direta para simplicar a interpretação de  ```(NExposure|Beach)```, é dizer que: para cada *Beach* foram coletados dados de **Riqueza de espécies** na presença de *low* e *high* *NExposure*. 
#' 
#' ## Comparando quais os melhores modelos ajustados.
#' 
## ------------------------------------------------------------
#Comparando todos os modelos via analise de variancia
anova(m0, m1, m2, m3, rich.model)

#' 
#' 
#' Desta forma, podemos verificar que o modelo **m2** é o melhor para explicar a **Riqueza de espécies** em 9 praias da Holanda com diferentes níveis de exposição. Assim, podemos dizer que a riqueza é explicada pelas variáveis *NAP* e *NExposure*, pela interação entre elas e considerando apenas o intecepto aleatório para a variável *Beach*. Veja que, ainda não possuímos uma estatística para saber o quão bem ajustado este modelo estar, temos apenas a medida que nos diz que o modelo *m2* difere dos demais, sendo o melhor entre eles.
#' 
#' Como nossos modelos são aninhados (Hierárquicos) também podemos compara-los via critério de Akaike (AIC), para tal utilizaremos o pacote *bbmle*.
#' 
## ------------------------------------------------------------
library(bbmle)
AICctab(rich.model, m0, m1, m2, m3, base = T, weights = T)

#' 
#' Foi empregado o teste de Akaike corrigido para amostras pequenas. Pelo AICc temos que o melhor modelo é o **m2**, corroborando com o resultado da análise de variância.
#' 
#' ## Análise de Resíduos em Modelos Lineares Mistos
#' 
#' Cabe ressaltar aqui que, os pressupostos da análise de regressão são válidos para este tipo de modelo. Logo, deve-se exister uma relação linear entre *x* e *y*, assim como, os resíduos devem seguir uma distribuição normal, as variâncias dos resíduos devem ser homogeneas, quanto ao pressuposto de autocorrelação dos resíduos este deve ser tratado de forma diferente, pois, estamos adicionando uma componete de correlação no modelo. Em relação a autocorrelação, geralmente se avalia dentro de cada nível do efeito aleatório ou se tenta adicionar uma componete de autocorrelação no modelo (AR1 ou outra). Sendo assim, começaremos por analisar o ajuste do nosso modelo pela análise de resíduos. Para isto, iremos utilizar o pacote *RVAideMemoire*.
#' 
## ----echo = T, message = F, fig.height=6, fig.width=12-------
library(RVAideMemoire)
plotresid(m2)

#' 
#' Outra forma para realizar esta análise de resíduos é fazendo uso do pacote ```robustlmm```, este trás uma rotina com 3 gráficos para análise dos resíduos e um para análise dos perfis.
#' 
## ----echo = T, message = F, fig.height=3, fig.width=6, fig.align="center"----
#Outra forma de analsiar os resíduos
library(robustlmm)
library(ggplot2)
plot.rlmerMod(m2, which = 1:4)

#' 
#' Podemos vizualizar pelo gráfico de resíduos versus valores ajustados que os resíduos não possuem homogeneidade de variâncias. Bem como, não apresentam distribuição normal de acordo com o gráfico quantil quantil. Ainda podemos avaliar a distribuição dos resíduos pelo teste de Shapiro-Wilk, no entanto o afastamento dos resíduos da distribuição normal é de se esperar, já que estamos trabalhando com **Riqueza de espécies** que são dados de contagem, desta forma a melhor maneira de modelar este problema seria utilizando os modelos lineares generalizados mistos com uso da distribuição Poison. Entretanto, continuaremos com o nosso modelo **m2** para concluir-mos esta apresentação. 
#' 
#' 
#' ## Contrastes e Testes Post-Hoc
#' 
#' ### Como interpretar os coeficientes do modelo
#' 
#' Vamos inicialmente analisar a tabela de coeficientes da equação selecionada.
#' 
## ------------------------------------------------------------
summary(m2)

#' 
#' Primeiro analisaremos o o *intercepto* do modelo. Em geral nos modelos de regressão o intercepto está relacionado com um valor médio quando $x = 0$. Em modelos com variáveis categóricas como *NExposure* que tem dois níveis (*low* e *high*) o que acontece é que um dos níveis é modifica o valor do *intecepto* e o outro fica ligado a variável como é o caso de *NExposurelow*. Esta escolha é realizada de acordo com a ordem dos níveis dos fatores, que em geral segue a ordem alfabética. Veja que no nosso modelo o nível *low* está relacionado com a variável *NExposurelow*.
#' 
#' Para facilitar a interpretação vamos verificar os valores das variáveis *NAP* e *NExposure* no nosso conjunto de dados.
#' 
## ----message=F, warning=F------------------------------------
library(dplyr)

#valor médio de NAP
summary(dados$NAP)

#valor médio de Richness
mean(dados$Richness)

#valor médio de Riqueza de espécies por NExposure 
dados %>% 
    group_by(NExposure) %>% 
    summarise(mean = mean(Richness))


#' 
#' Por termos um termo aleatório os valores entre as tabelas descritivas e a tabela dos coeficientes diferem um pouco. Mas podemos ver que o valor do **intercepto** corresponde ao valor médio da **Riqueza** quando **NExposure** é *high*.
#' 
#' A variável *NAP* é contínua e seu entendimento difere um pouco, com um coeficiente angular **negativo** podemos afirmar que quanto maior o valor de *NAP* menor a **Riqueza de espécies**. Como o modelo possui interação não é possível obter uma relação direta com a média, como no exemplo anterior.
#' 
#' Podemos ver com base na tabela da média de **Riqueza** por **NExpouse** que quando a exposição é *low* a **riqueza** média foi de $7.84$ e o valor do coeficiente *NExpouselow* foi de $\sim 5.25$ o que somado ao *intercepto* corresponde a $\sim 5.25 + 3.60 = 8.95$ próximo a média de **riqueza** obtidada na tabela quando **NExpouse** é *low*. Ou seja, este valor representa o valor médio quando o nível é indicado no coeficiente.
#' 
#' O último coeficiente é ainda mais difícil de interpretar. Ele indica a relação negativa quando **NExpouse** é *low* na presença de *NAP*. Ou seja, quando **NExpouse** é *low*, quanto maior o valor de *NAP* menor será a **riqueza**.
#' 
#' Veja que para a variável categórica categórica avaliamos apenas os efeitos simples, mas em geral o que queremos é o efeito principal. Mas o que é o efeito principal? Bem, isto é mais fácil de explicar quando trabalhamos com duas variáveis categóricas, por exemplo, **X com dois níveis ($A_1, A_2$)** e **Z, com dois níveis ($B_1, B_2$)**, quando avaliamos o efeito, sempre fazemos o efeito do nível de uma variável ($A_1$) na presença do nível da outra variável ($B_1$). Sendo assim, testar **efeito principal** de $X$ é a mesma coisa que testar se a hipótese nula de que $\mu B_1 = \mu B_2$, já o **efeito simples** é a mesma coisa que testar a hipótese nula de que $\mu A_1B_1 = \mu A_1B_2$. Veja o resumo dado na tabela abaixo, o que representado pelas interações de $X$ e $Y$ são os **efeitos simples**:
#' 
#' 
#' 
#' Variáveis      | $X (A_1)$      | $X (A_2)$      | Efeito Principal de $Z$
#' ------------------------|----------------|----------------|-----------------
#'               $Z (B_1)$ | $\mu_{A_1B_1}$ | $\mu_{A_2B_1}$ | $\mu (B_1)$   
#'               $Z (B_2)$ | $\mu_{A_1B_2}$ | $\mu_{A_2B_2}$ | $\mu (B_2)$   
#' Efeito Principal de $X$ | $\mu (A_1)$      | $\mu (A_2)$      |
#' 
#' Resumindo a ideia de efeitos simples e principais Dale Bar afirmou que: Em um desenho $XxZ$, o efeito simples de $X$ é o efeito de $X$ controlando $Z$, equanto o efeito principal de $X$ é o efeito de $X$ ignorando $Z$.
#' 
#' ### Contrastes
#' 
#' Quando temos variáveis categóricas por *default* nosso modelo atribuiu a o valor $0$ quando *NExpouse* foi *hiigh* e $1$ quando foi *low*, isto é conhecido como *variáveis dummy*. Já sabemos que quando um nível recebe o valor $0$ ele passa a ser o valor de refrência do **intercepto**. Sempre que temos uma variável categórica a tabela de coeficientes mostra os valores dos **efeitos simples**. Quando temos apenas uma variável categórica isso não gera problemas, pois é fácil contrastar os diferentes néveis dentro daquela variável. No entanto, quando temos duas ou mais variáveis categóricas isto se torna um problema, pois queremos em geral os efeitos principais.  
#' 
#' Para exemplificar esta análise, vamos utilizar como base o exemplo dado anteriormente, poderiamos proceder com o seguinte código, imagine que temos um modelo full para as variáveis *X* e *Z*.
#' 
#' ```
#' Como por default o modelo atribui 0 e 1 para variáveis "dummy", isto nos leva a ficarmos apenas com os efeitos simples (a média de apenas um dos níveis é expressa) na tabela de coeficientes, o que queremos é encontrar o valor médio da variável no coeficiente. Para que isso ocorro vamos modificiar o valor das nossas variáveis dummy. Assim, estaremos mudando o contraste para podemos aplicar o sum coding a cada uma das variáveis, o que irmos fazer é opor seus níveis usando os valores positivos e negativos, quando temos apenas dois níveis dentro da variável usamos os valores 0.5 e -0.5, isto fará com que o valor do coeficiente b0 mude. Assim, o coeficiente b0 deixa de corresponder a apenas um dos níveis da variável e passa a corresponder ao valor médio geral das observações.
#' 
#' ## ajustar o sum coding para variável X, ou seja, mudando o valor do contraste para X
#' 
#' contrasts(dataset$X) = c(-0.5, 0.5)
#' 
#' ## visualizando os contrastes
#' contrasts(dataset$X)
#' #>              [,1]
#' #> A1 -0.5
#' #> A2  0.5
#' 
#' ## ajustar o sum coding para variável Z, ou seja, mudando o valor do contraste para Z
#' 
#' contrasts(dataset$Z) = c(-0.5,0.5)
#' 
#' ## visualizando os contrastes
#' contrasts(dataset$Z)
#' #>       [,1]
#' #> B1 -0.5
#' #> B2  0.5
#' 
#' ## reajustando o modelo com os contrastes empregados 
#' 
#' model.full <- lmer(resposta ~ X*Z + (1|VarAleatoria), 
#'                    data = dataset)
#' 
#' ## resultado
#' 
#' summary(model.full)
#' 
#' Agora temos que, o intercepto agora corresponde a média da variável resposta ignorando as condições experimentais. Os coeficientes angulares agora correspondem aos contrastes definidos (efeito principal), e não a comparação direta com o intercepto, como anteriormente. Para fazermos uma comparação semelhante basta pegar o valor médio da variável X quando Z ocorre e subtraílos, isto corresponderá ao valor do coeficiente angular relacionado aquela situação.
#' 
#' ```
#' 
#' ### Testes de Hipótese (post Hoc)
#' 
#' Como obtivemos que houve diferenças significativas entre a interação *NAP* e *NExpouse* cabe agora sabermos quem se diferencia de quem. Lógico que, como temos uma variável contínua parte dessa diferença é obtida direto como foi realizado na análise da tabela de coeficiente para a variável *NAP*. Como nosso variável categórica só possui dois níveis a comparação é de forma direta também. 
#' 
#' Para casos mais gerais podemos empregar o teste de **Tukey** para comparar os níveis, podemos fazer isto de forma direta com a rotina contida no pacote *emmeans*. Na realização do cálculo temos que especificar o *modelo* ajustado e as variáveis que queremos fazer a comparação, desta forma podemos usar os seguintes códigos para comparação:
#' 
#' 1. ``` emmeans(modelo ~ X*Z)```, aqui estamos querendo que as comparações sejam realizadas com base na interação das duas variáveis, ou seja, uma na presença da outra;
#' 
#' 2. ``` emmeans(modelo ~ X)```, a comparação é feita em relação apenas aos níveis da variável *X*; 
#' 
#' 2. ``` emmeans(modelo ~ Z)```, a comparação é feita em relação apenas aos níveis da variável *Z*; 
#' 
#' Agora vamos proceder com os cálculos para o nosso experiemnto.
#' 
## ------------------------------------------------------------
library(emmeans)

post.hoc.test <- emmeans(m2, ~ NAP*NExposure)
post.hoc.test

#' 
#' Foram cáculadas as diferenças entre os níveis da variável *NExposure* na presença de *NAP*, como já foi falado como *NAP* é contínua as comparações aos pares são pouco informativas, mas assim, é como devemos proceder específicando o teste de médeis que queremos empregar, aqui vamos empregar o teste de **Tukey**.
#' 
## ------------------------------------------------------------
options(digits = 1)
pairs(post.hoc.test, adjust = "tukey")

#' 
#' Por meio do teste de *Tukey* podemos afirmar que existe diferenças significativas entre os níveis *low* e *high* da variável **NExposure**, ao nível de 5% de significância.
#' 
#' ## Modelos Lineares Generalizados Mistos
#' 
#' ### Distribuições de Probabilidade 
#' 
#' Quando estamos realizando um experimento podemos trabalhar com variáveis do tipo discreto (quando o seu resultado envolve contagens, categorias, níveis, etc.), a definição precisa de uma variável ser discreta ou contínua pode variar bastante dependendo do objeto medido, de como é medido, do campo de visão do problema entre outros. Em geral pela natureza aleatória do experimento que é realizado podemos chamar esta variável de aleatória, assim em experimentos sempre trabalhamos com **variáveis aleatŕoias discretas ou contínuas**. 
#' 
#' Com este conhecimento então podemos definir **distribuições** ou **modelos** de **probabilidade** do tipo *discreto* ou *contínuo*.
#' 
#' ### Distribuições discretas
#' 
#' #### Distribuição de Bernoulli
#' 
#' Uma variável aleatória $X$ segue a distribuição de *Bernoulli* se atribui $0$ ou $1$ à ocorrência de fracaso ou sucesso (presença e ausência), respectivamente. A função discreta de probabilidade para esta distribuição é dada por:
#' 
#' $$P(X=x) = p^x(1-p)^{1-x}, \;\;\;\;\;\;\; x=0, 1$$
#' 
#' Em que, $p$ representa a probabilidade de sucesso (presença), $0 \leq p \leq 1$.
#' 
#' Esta distribuição está relacionada a um único ensaio de Bernoulli, quando temos a repetição de ensaios de Bernoulli independentes chegamos a famosa distribuição *Binomial*.
#' 
#' #### Distribuição Binomial
#' 
#' Para definirmos a da distribuição Binomial vamos considear a repetição de $n$ ensaios de Bernoulli independentes e em todos os ensaios temos o mesmo valor $p$ de probabilidade para o sucesso (presença). Assim, temos que a variárel aleatória que conta o número de sucessos (presença) nos ensaios com os parâmetros $n$ e $p$ tem a seguinte função de probabilidade:
#' 
#' $$ P(X=k) = \binom{n}{k} p^k (1-p)^{n-k},\;\;\;\;\;\;\; k = 0,1,2,...,n,$$
#' 
#' em que, $\binom{n}{k}$ é o coeficiente binomial e é calculado por:
#' 
#' $$\binom{n}{k} = \frac{n!}{k!(n-k)!}$$
#' 
#' Para indicarmos que uma variável tem distribuição binomial com os parâmetros $n$ e $p$ podemos utilizar a notação $X \sim b(n,p)$.
#' 
#' Esta é uma das distibuições mais utilizadas para calcular probabildiades quando temos variáveis dicotômicas, ou seja, com apenas dois resultados possíveis. Simplificando o que foi falado anteriormente nas definições utilizamos a distribuição de binomial quando:
#' 
#' 1. temos apenas dois níveis na sso variável categórica (duas possibilidades);
#' 
#' 2. todos os ensaios são independentes (a ocorrência de um dado não intefere na probabilidade de ocorrência do outro);
#' 
#' 3. A probabilidade $p$ não se altera (em todos o ensaios a probabilidade de sucesso (presença) não muda).
#' 
#' Quando trabalhamos com a **regressão logística** que é um caso dos **modelos lineares generalizados mistos**, estamos trabalhando em geral com dados com distribuição binomial. 
#' 
#' Quando temos dados de variáveis categóricas por muitas vezes recorremos aos testes de tabela de contigência ou qui-quadrado, no entanto, quando temos respostas repetidadas, o melhor caminho é empregar a **regressão logística**. Para o completo entendimento dessa metodologia temos que está apar das definições de probabilidade e chances (sua versão em inglês é bastante empregada, *odds* e *odds ratio*).
#' 
#' ### Probabilidades e chances (odds e odds ratio)
#' 
#' A *probabilidade* de maneira bem informal nada mais é do que, a possibilidade de um evento occorer dado um espaço de eventos possíveis. Um dos exemplos mais clássicos da distribuição binomial é o da moeda, cara ou coroa. Então digamos que em um experimento onde foi realizado o lançamento de 100 moedas, saíram 50 caras (sucesso) e 50 coroas (fracasso), assim, podemos expressar estes resultado em forma de **probabilidade** ou de **chances**, vejamos:
#' 
#' 1. A probabilidade de sair cara é de 0.5 ($\frac{50}{100}$).
#' 
#' 2. A chance de sair cara é de 50%.
#' 
#' 3. A chance de sair cara é de 1:1 (que pode ser lido como: um para um ou uma cara para uma coroa).
#' 
#' Agora vamos tentar compreender as diferenças entre *probabilidade* e *chance*. Vejamos outro exemplo, digamos que outro experimento com moedas foi realizado e os resultados foram 10 caras (sucessos) e 100 coroas (fracassos), podemos extrair as seguintes informações:  
#' 
#' 1. A probabilidade de sair cara é de 0.09 ($\frac{10}{110}$).
#' 
#' 2. A chance de sair cara é de 9%.
#' 
#' 3. A chance de sair cara é de 10:100 (que pode ser lido como: dez para cem ou 10 caras para cem coroas).
#' 
#' Vamos montar uma tabela com diferentes exemplos sobre porcentagem, probabilidade e chances (odds) para auxiliar nossa aprendizagem sobre estes termos.
#' 
#' 
#' Porcentagem | Probabilidade | Chance (odds) | Chance (odds)
#' ------------|---------------|---------------|---------------------
#' 50%         | 0,5           | 1             | 1:1  sucesso:fracasso
#' 90%         | 0,9           | 9             | 9:1  sucesso:fracasso
#' 10%         | 0,1           | 0,11          | 1:9  sucesso:fracasso
#' 30%         | 0,3           | 0,42          | 3:7  sucesso:fracasso
#' 60%         | 0,6           | 1,5           | 3:2  sucesso:fracasso
#' 75%         | 0,75          | 3             | 3:1  sucesso:fracasso
#' 80%         | 0,8           | 4             | 4:1  sucesso:fracasso
#' 
#' Com esta noção concretizamos nosso entendimento sobre canche (odds), e temos por entendimento que *chance (odds)* é nada mais do que uma outra maneira de se espressar a probabilidade. Mas, os termos são diferentes *probabilidade* varia de $0$ a $1$, já *chance (odds)* pode assumir qualquer valor entre $0$ e infinito positivo.
#' 
#' ### Regressão logística
#' 
#' Para definimos formalmente a regressão logística consideremos o modelo logístico linear em que $P(x)$, a probabilidade de *sucesso* para um valor de *x* de uma dada variável independente pode ser calculada por:
#' 
#' $$log\left(\frac{P(x)}{1-P(x)}\right) = \alpha + \beta x$$
#' em que, $\alpha$ e $\beta$ são parâmetros que serão estimados. O termo $log\left(\frac{P(x)}{1-P(x)}\right)$ é chamado de *logit* e o termo $\frac{P(x)}{1-P(x)}$ representa a chance (odds) de ocorrência do evento de interesse.
#' 
#' Com um pouco de recursos matemáticos podemos chegar na seguinte parametrização do modelo (forma mais comum):
#' 
#' $$P(x) = \frac{e^{\alpha+\beta x}}{1 + e^{\alpha+\beta x}}$$
#' 
#' Veja que, o lado direito superior da equação $e^{\alpha+\beta x}$ é a parte sigmoide de $x$, é ela quem mapeia a linha real para o intervalo $(0, 1)$. Podemos verificar também que a derivada desta função nos leva a:
#' 
#' $$P(x) = \frac{e^{\alpha+\beta x}}{1 + e^{\alpha+\beta x}} = (e^{\alpha+\beta x}) (1+e^{\alpha+\beta x})^{-1}$$
#' $$P'(x) = (e^{\alpha+\beta x})(1+e^{\alpha+\beta x})^{-1} + (e^{\alpha+\beta x})(-1)(1+e^{\alpha+\beta x})^{-2}(e^{\alpha+\beta x})$$
#' $$P'(x) = \frac{(e^{\alpha+\beta x})(1+e^{\alpha+\beta x})}{(1+e^{\alpha+\beta x})^2}-\frac{(e^{\alpha+\beta x})^2}{(1+e^{\alpha+\beta x})^2}$$
#' $$P1(x) = \frac{e^{\alpha+\beta x}}{(1+e^{\alpha+\beta x})^2}$$
#' $$P'(x) = \frac{e^{\alpha+\beta x}}{1+e^{\alpha+\beta x}}\cdot \frac{1}{1+e^{\alpha+\beta x}}$$
#' $$P'(x) = P(x)(1-P(x))$$
#' 
#' #### Razão de chance ou Risk odds Ratio
#' 
#' A razão de chance é calcula pela divisão de chances de dois fatores, temos que:
#' 
#' $$odds \;R = \frac{P(x_1)/(1-P(x_1))}{P(x_2)/(1-P(x_2))}$$
#' 
#' #### Pressupostos do modelo
#' 
#' O modelo logístico possui os seguintes pressupostos:
#' 
#' 1. Deve-se existir relação linear entre as variáveis explicativas $X$ e a variável resposta $Y$;
#' 2. A esperança dos resíduos é igual a $0$;
#' 3. As variâncias dos resíduos devem ser homogeneas;
#' 4. As variáveis explicativas não são correlacionadas (ausência de multicolinearidade).
#' 
#' #### O experimento
#' 
#' O obetivo da pesquisa foi avaliar se a distibuição da espécie *Araucaria angustifolia* e *Podocarpus lambertii* é afetada por fatores ambientais. 
#' 
#' Para tal procedimento foi realizado um experimento em cinco área de vegetação não explorada onde foram alocadas 1000 parcelas de tamanho $10 \;m\; x\; 10\; m$. As variáveis medidas foram: 
#' 
#' 1. *profundidade do solo* com dois níveis profundo e raso;
#' 
#' 2. *exposição do terreno* quando existia a presença de sub-bosque denso foi consirado protegido o contrario exposto; 
#' 
#' 3. *área basal* medida em $m^2 ha^{-1}$. 
#' 
#' 4. *área* pontos onde foram instaladas as parcelas.
#' 
#' Os dados do experimento foram simulados conforme parâmetros encontrados no artigo de [Longhi et al., 2010](https://www.redalyc.org/pdf/331/33118929002.pdf).
#' 
#' #### Distribuição da espécie *Araucaria angustifolia*
#' 
#' Inicialmente iremos analisar o conjunto de dados, temos que existe uma relação espacial entre áreas e possui cinco níveis, logo esta variável pode ser empregada como **fator aleatório**. 
#' 
## ------------------------------------------------------------
#importando o conjunto de dados

araucaria <- read.csv("Dataset/Distribuicao_Araucaria.csv", h = T)
head(araucaria)
str(araucaria)

#' 
#' Veja que em nosso conjunto de dados só temos uma variável continua, as demais são categoricas, no entanto como estas são representadas por números inteiros, todas foram atribuidas como númericas. Vamos corrigir isto, transformando nossas variáveis categóricas para fator.
#' 
## ------------------------------------------------------------
#Transformar a variável distribuição para fator 
araucaria$Distribuicao <- as.factor(araucaria$Distribuicao)

#Transformar a variável profundidade para fator 
araucaria$Profundidade <- as.factor(araucaria$Profundidade)

#Transformar a variável exposição para fator 
araucaria$Exposicao <- as.factor(araucaria$Exposicao)

#Transformar a variável area para fator 
araucaria$Area <- as.factor(araucaria$Area)

#' 
#' Vamos analisar a distribuição da espécie para cada uma das variáveis *Profundidade*, *Exposicao* e *Area*. 
#' 
## ------------------------------------------------------------
#Tabela de contagem e frequência relativa da Distribuicao por Profundidade do solo
araucaria %>% 
    group_by(Profundidade, Distribuicao) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n))

#Tabela de contagem e frequência relativa da Distribuicao por Exposicao do solo
araucaria %>% 
    group_by(Exposicao, Distribuicao) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n))

#Tabela de contagem e frequência relativa da Distribuicao por Áreas
araucaria %>% 
    group_by(Area, Distribuicao) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n))

#' 
#' Vamos comparar estes valores com as **chance (odds)** nas tabelas abaixo:
#' 
#' Distribuição da Espécie | Odds (profundo "1":raso "0")
#' ------------------------|------------------------
#' 0                       | 119:345 0,34
#' 1                       | 430:106 4,06
#' 
#' Os resultados indicam que a ocorrência da espécie tem menor chance de acontecer em um solo raso do que em um solo profundo.
#' 
#' Distribuição da Espécie | Odds (menos exposto "1":mais exposto "0")
#' ------------------------|-------------------------------------------
#' 0                       | 153:322 0,47
#' 1                       | 396:129 3,07
#' 
#' Os resultados apontam que a inexistência da espécie tem maior chance de ocorrer em um solo mais exposto do que em um solo menos exposto.
#' 
#' Distribuição da Espécie | Odds (área em evidência :demais áreas)
#' ------------------------|----------------------------------------
#' 0                       | área 1:áreas[2,3,4,5] 150:301 0,50
#' 0                       | área 2:áreas[1,3,4,5] 136:315 0,43
#' 0                       | área 3:áreas[1,2,4,5] 92:359  0,27
#' 0                       | área 4:áreas[1,2,3,5] 47:404  0,12
#' 0                       | área 5:áreas[1,2,3,4] 26:425  0,06
#' 1                       | área 1:áreas[2,3,4,5] 50:499  0,10
#' 1                       | área 2:áreas[1,3,4,5] 64:485  0,13
#' 1                       | área 3:áreas[1,2,4,5] 108:441 0,25
#' 1                       | área 4:áreas[1,2,3,5] 153:396 0,39
#' 1                       | área 5:áreas[1,2,3,4] 174:375 0,46
#' 
#' Para as áreas temos que existe uma maior chance de não existir a espécie na área 1 do que nas demais. Bem como existe maior chance de ocorrer a espécie na área 5 do que nas demais.
#' 
#' Depois da ideia inicial que temos sobre os nossos dados vamos ajustar alguns modelos de Regressão Logística. Inicialmente vamos ajustar um modelo considerando a distribuição da espécie *Araucaria angustifolia* em relação a profundidade do solo. A sintaxe para escrita do modelo é semelhante a que já estudamos. 
#' 
#' Como nossa variável resposta **Distribuição** é dicotômica  (ausência:presença) e estamos modelando considerando apenas a variável independente **Profundidade** (raso:profundo) com dois néveis, utilizaremos o contraste *dummy*. Desta forma, poderemos fazer a comparação direta entre os níveis dos efeitos que iremos analisar. Como já foi apresentado a regressão logística possui uma resposta com distribuição binomial e para indicarmos utilizamos o parâmetro *family*, o ajuste do modelo é realizado com a função *glm* (Generalized Linear Models), como sabemos a regressão logística é um tipo de modelo linear generalizado.
#' 
## ------------------------------------------------------------
#ajustando o modelo

modelo.arau <- glm(Distribuicao ~ Profundidade, data = araucaria, family = binomial)

#apresentando o resumo do modelo

summary(modelo.arau)

#' 
#' A saída do modelo ajustado traz uma série de resultados, inicialmente temos a chamada do modelo *call*, uma análise descritiva dos resíduos, a tabela com os coeficientes e suas estatísticas, alguns parâmetros de dispersão para a família binomial considerando o efeito 1, o valor do teste de Akaike (AIC) e o número de iterações do Score de Fisher para convergência do modelo.
#' 
#' Logo de ínicio temos que o efeito de solo raso está no intercepto, isto indica que o modelo está comparando, na segunda linha, a distribuição da espécie no solo profundo com o solo raso.
#' 
#' O que está acontecendo nesse modelo? Bem, primeiramente sabemos que o modelo testa a distribuição da espécie. Tenha ideia de que nosso modelo toma como valor de referência o valor *0* (inexistência da espécie), pois ele ordena de forma alfabética ou númerica crescente. Sendo assim, podemos definir que nosso modelo está comparando a proporção  da distribuição do nível de não referência com o nível de referência. 
#' 
#' Agora podemos afirmar que o intercepto do nosso modelo está comparando, a ocorrência de espécie (*1*)  com a não ocorrência da espécie (*0*, valor de referência), para o solo raso (da variável profundidade). Veja que isso representa a chance *odds* (sucesso:fracasso). Olhando para o valor do intercepto temos que ele é negativo $-1,064$, isto significa que, no intercepto que está associado ao solo raso, existe menor ocorrência de espécie do que não ocorrência, ou seja, no solo raso existe predominância de não existência da espécie, mas essa informação já sabiamos pela tabela de frequência que fizemos. Mas pela análise do teste de hipótese para o coeficiente (intecepto) vemos que essa diferença é estatísticamente significativa.
#' 
#' Mas ainda existe um pergunta por que o valor de $-1,064$, se na tabela de *odds* temos que a chance de ocorrência:não ocorrência da espécie em solo raso é de  $119:345$ ou $0,34$. Bem, isto é explicado olhando para a definição do nosso modelo, vemos que ele não calcula a *chance* diretamente, mas sim, o logaritmo da chance. Sendo assim, vamos verificar qual o logaritmo da chnace de ocorrência:não ocorrência da espécie em solo raso:  
#' 
#' $$log(odds \;ratio) = log\left(\frac{119}{345}\right) = 0,34$$
#' 
#' Logo, temos que o modelo está realizando o mesmo cálculo de forma um pouco diferente, nosso modelo utiliza o logaritmo da chance justamente para que ele possa assumir valores negativos, e assim, de forma simples, podemos avaliar se houve mais sucesso ou fracasso ou virce versa simplesmente verificando o sinal do intercepto. Agora, se quisermos encontrar a razão de chance (*odds ratio*) a partir da *log odds ratio*, basta calculamos o exponencial.
#' 
## ------------------------------------------------------------
options(digits=3) #Modificando o número de casas decimais
exp(-1.064)

#' 
#' Em relção ao coeficiente $\hat{\beta}$, não podemos concluir diretamente que ele corresponde diretamente ao log da razão de chance de ocorrência da espécie em solo profundo (A razão de chance para ocorrência de espécie no solo "profundo:raso" é de *4,06*), aqui o valor de $\hat{\beta}$ é de *2,465*. Como já vimos para o modelo linear, pada dados categóricos o valor do intecepto corresponde sempre a um dos efeitos, neste caso como analisamos o valor do intercepto está relacionado com o efeito do solo raso. Se quisermos encontrar o log da razão de chance só para o efeito de solo profundo, temos que fazer a diferença. Assim temos:
#' 
## ------------------------------------------------------------
options(digits=3)
#Log da Razão de Chance
Log.oddsRatio.1 <-  2.465-1.064
Log.oddsRatio.1

#Razão de Chance
oddsRatio.1 <- exp(Log.oddsRatio.1)
oddsRatio.1

#' 
#' Assim, conseguimos encontrar a Razão de Chance para distribuição da espécie (*ocorrencia*) em razão do solo **"profundo:raso"**. Mas o que podemos extrair mais do coeficiente  $\hat{\beta}$.
#' 
#' Também podemos analisar o valor do coeficiente $\hat{\beta}$ com respeito ao que ele está expressando na equação de fato. Assim, podemos afirmar, a equação nos informa que, em relação ao solo raso, a razão de chance de ocorrência da espécie em solo profundo é maior que no solo raso, ainda mais, com base no teste de hipótese e no p-Valor do coeficiente a diferença entre esses efeitos é significativa ao nível de confiança $\alpha$ de 0,05. Agora podemos nos perguntar, qual a razão de chance de ocorrência da espécie no solo profundo em comparação ao intercepto (solo raso)? Para sabermos este valor, basta extraírmos o exponencial do coeficiente $\hat{\beta}$.
#' 
## ------------------------------------------------------------
#Vamos calcular a chance de ocorrência:não ocorrência em solo profundo em relação ao solo raso (intercepto)
exp(2.465)

#Vamos calcular a chance de não ocorrênciao:corrência em solo profundo em relação ao solo raso (intercepto)
exp(-2.465)

#' 
#' o que isso nos diz, temos que a chance de ocorrência da espécie em solo profundo é *11,8* vezes maior que no solo raso.
#' 
#' A segunda parte do resultado possui uma interpretação menos direta ou menos necessária. Ele nos diz que que a chance da espécie não ocorrer em solo profundo é de 0,085 em relação ao solo raso. 
#' 
#' Vamos também calcular o intervalo de confiança para a razão de chances encontradas na equação. Para tal vamos extrair os coeficientes da equação e e chamar a função que calcula o intervalo de confiança, lembre que este resultado retorna o *log odds ratio*, para chegarmos a **razão de chances** precisamos calcular o exponencial da saída:
#' 
## ------------------------------------------------------------
exp(cbind(OR=coef(modelo.arau), confint(modelo.arau)))

#' 
#' 
#' Bem uma vez que somos capazes de interpretar a saída do nosso modelo, temos agora que avaliar o ajuste do nosso modelo.
#' 
#' #### Técnicas de adequação do ajuste
#' 
#' A análise de resíduos tradicional (graficamente) para a regressão logística quando as duas variáveis dependente e independente são dicotômicas é pouco informativa. Dessa forma, vamos analisisar inicialmente se existe relação linear da variável **Profundidade** com a variável **Distribuição** da espécie. Vamos testar as seguintes hipóteses:
#' 
#' - $H_0:$ A relação linear entre as variáveis é nula.
#' - $H_1:$ Existe relação linear entre as variáveis.
#' 
#' Vamos calcular o teste Qui-quadrado conforme:
#' 
## ------------------------------------------------------------
options(digits = 2)

drop1(modelo.arau, test = "Chisq")

#' 
#' O teste indica que existe uma forte relação linear entre as variáveis.
#' 
#' #### Diagnóstico do resíduos
#' 
#' De forma geral, um **resíduo ($r_i$)** é uma medida que expressa o afastamento de um elemento ($y_i$) da variável observada em relção a estimativa deste elemento ($\hat{y}_i$).
#' 
#' **Resíduo ordinário**
#' 
#' O resíduo ordinário nada mais é do que a diferença entre um elemento $y_i$ de valores observados com o elemento ($\hat{y}_i$) estimado correspondente. Sua obtenção é direta:
#' 
#' $$r_i = (y_i - \hat{y}_i)$$
#' 
#' Cabe ressaltar aqui que os resíduos ordinários não são homogeneos, ou seja, não possuem variânica constante, por esta razão estes não são utilizados no diagnóstico de modelos lineares generalizados.
#' 
#' **Resíduo de Pearson**
#' 
#' O resíduo de Perason também obtido pela diferentre entre observado e estimado, porém ele é corrigio pelo componente da estatística $x^2$ de Pearson. Seu cálculo é dado por:
#' 
#' $$r_i^P = \frac{(y_i - \hat{y}_i)}{\sqrt{V(\hat{y}_i)}}$$
#' 
#' Em que: $V(\hat{y}_i)$ é a função de variância do modelo. Este resíduo tem a desvantagem de possuírem distribuição assimétrica quando utilizamos modelos com resposta diferente da normal. 
#' 
#' **Resíduo de Pearson Padronizado**
#' 
#' O resíduo de Pearson Padronizado é definido da seguinte forma:
#' 
#' $$r_i^{Ppad} = \frac{(y_i - \hat{y}_i)}{\sqrt{\hat{\theta}V(\hat{y}_i)(1-h_{ii})}}$$
#' Em que: $h_{ii}$ é o $i-ésimo$ elemento da diagonal da matriz **H** (matriz de projeção da solução de mínimos quadrados).
#' 
#' **Resíduo Componente da Deviance ou Medida de Deviance**
#' 
#' A deviance também é conhecida como *desvio*. A medida de deviance para um elemento da variável observada correponde à contribuição desse elemento observado para a deviance do modelo. Resumindo a deviance é uma medida referente a distância de $y_i$ em relação a $\hat{y}_i$ levando em consideração a escala do logaritmo da verossimilhança. A deviance de um modelo (MLG) é dada por:
#' 
#' $$D(y_i:\hat{y}_i) = 2(\hat{l}_y - \hat{l}_{\hat{y}_i}) = \sum_{i=1}^n d_i$$
#' 
#' Logo, o resíduo componente da deviance é calculado por:
#' 
#' $$ r_i^D = sinal(y_i - \hat{y}_i)\cdot \sqrt{d_i}$$
#' 
#' Em que: o componente sinal é obtido por, $sinal(\lambda) = -1$, se $\lambda < 0$ e $sinal(\lambda) = +1$, se $\lambda>0$.
#' 
#' **Resíduo Componente da Deviance Padronizado**
#' 
#' O resíduo Componente da Deviance Padronizado é expresso por:
#' 
#' $$r_i^{Dpad} = \frac{r_i^D}{\sqrt{\hat{\theta (1-h_{ii})}}}$$
#' 
#' Agora vamos realizar estes cálculos no R e depois apresentar os resultados na forma gráfica.
#' 
## ----echo=T--------------------------------------------------
#Obtenção dos resíduos ordinários
r_ord <- residuals(modelo.arau, type = "response")
head(r_ord)

#Obtenção dos resíduos de Pearson
r_pear <- residuals(modelo.arau, type = "pearson")
head(r_pear)

#Obtenção dos resíduos de Pearson
r_pear.pad <- residuals(modelo.arau, type = "pearson")/sqrt(1 - hatvalues(modelo.arau))
head(r_pear.pad)

#Obtenção dos resíduos Componente da Deviance
r_dev <- residuals(modelo.arau, type = "deviance")
head(r_dev)

#Obtenção dos resíduos Componente da Deviance
r_dev.pad <- residuals(modelo.arau, type = "deviance")/sqrt(1 - hatvalues(modelo.arau))
head(r_dev.pad)

par(mfrow=c(2, 2))
plot(density(r_pear), ylab = "Densidade", main='Deviance x Resíduo de Pearson')
lines(density(r_dev), col='red')

plot(density(r_pear.pad), ylab = "Densidade", main='Deviance Padronizada x Resíduo de Pearson Padronixado')
lines(density(r_dev.pad), col='red')

plot(r_dev.pad, 1:length(r_dev.pad), xlab = "Índice", ylab = "Resíduo Componente da Deviance", ylim = c(min(r_dev.pad)-1 , max(r_dev.pad)+1), pch = 16)
abline(2,0,lty=2)
abline(-2,0,lty=2)

plot(fitted(modelo.arau), r_dev.pad, xlab="Valor Ajustado", 
ylab="Resíduo Componente da Deviance", pch=16)
#identify(fitted(fit.model), td, n=1)
par(mfrow=c(1,1))


#' 
#' 
#' **Testes sobre os resíduos**
#' 
#' Para verificar se o modelo foi bem ajustado podemos recorrer a alguns testes, tais como, a estatística de **Pearson generalizada** e do **Desvio (Deviance)**, entretanto para usar estes testes devemos seguir a seguinte recomendação $N < n$.
#' 
#' - N: número de combinações das variáveis dependentes;
#' - n: tamanho amostral;
#' 
#' Para casos em que esta recomendação é violada devemos utilizar o teste de  **Hosmer-Lemeshow**.
#' 
#' Vamos considerar as seguintes hipóteses:
#' 
#' - $H_0:$ modelo adequado;
#' - $H_1:$ modelo inadequado.
#' 
#' **Estatística de Pearson generalizada**
#' 
#' Uma forma de medir a discrepância do ajuste de um modelo (MLG) com relação a um conjunto de dados é utilizando a estatística de Pearson generalizada ($\chi^2_p$), a qual podemos calcular por:
#' 
#' $$\chi^2_p = \sum_{i=1}^{n} \dfrac{y_i-\widehat{P(x_i)}^2}{V\widehat{P(x_i)}}$$
#' 
#' Em que  $V\widehat{P(x_i)}$ é a função de variância estimada com respeito ao modelo estimado ao conjunto de dados. $\widehat{P(x_i)}$ corresponde a $\hat{\mu}$ como é mais conhecido em modelos desta natureza.
#' 
#' **Estatística do Desvio (Deviance)**
#' 
#' $$Q_D = 2\sum_{i=1}^{n}\left[(y_i log \left(\dfrac{y_i}{\widehat{P(x_i)}}\right)+(m_i-y_i)log \left( \dfrac{m_i-y_i}{m_i-\widehat{P(x_i)}}\right)\right] $$
#' 
#' Em que, $y_i$ é a nossa variável independente.
#' 
#' Para amostras grandes podemos inferir que $\chi^2_p$ e $Q_D$ seguem uma distribuição Qui-quadrado com $N-p-1$ graus de liberdade. Vale notar que se extrairmos a raiz quadrado dos componentes $\chi^2_p$ e $Q_D$ temos os resíduos de Pearson e dos Desvios.
#' 
## ------------------------------------------------------------
#Estatística de Pearson
Q_p <- sum(r_pear)
Q_p

#Estatística Desvio
Q_d <- 2*sum(r_dev)
Q_d

#Qui quadrado tabelado
qchisq(p = 0.95, df = 2) #alpha = 0.05 devido p = 0.95 (p = probabilidade)

#' 
#' Como podemos a estatística e Pearson não foi informativa para o modelo ajustada e a estatística da Deviance mostrou que o modelo não é adequado com base no Qui-qudrado tabelado.
#' 
#' **Estatística de Hosmer e Lemeshow**
#' 
#' 
#' Os autores desenvovleveram uma estatística em que os dados são agrupados com base nas probabilidades estimadas. Em geral são criados no máximo 10 grupos ($g=10$), em que primeiramente as probabilidades são ordenadas e posteriormentes os grupos são criados com base nos percentis. A estatística deste teste é do tipo Pearson:
#' 
#' $$HL = \sum_{i=1}^{g} \dfrac{o_i-n_i\bar{P(x_i)^2}}{n_i\bar{P(x_i)}(1-\bar{P(x_i)})}$$
#' 
#' A estatística $HL$ também segue uma distribuição Qui-quadrado com $g-2$ graus de liberdade.
#' 
#' O procedimento para cálculo deste teste é dado abaixo, no entanto para este exemplo devemos utilizar as duas primeiras estatísticas. Se o valor p for maior que $0,05$ não se rejeita a hipótese nula.
#' 
## ------------------------------------------------------------
library(ResourceSelection)
hoslem.test(as.numeric(araucaria$Distribuicao), 
            ifelse(fitted(modelo.arau)>0.5,1,0))

#' 
#' #### Estatísticas de qualidade do ajuste
#' 
#' Existe diferentes estatísticas para avaliar a qualidade de ajuste de um modelo logístico, vamos estudar algumas aqui. Mas, primeiro de tudo, se verificarmos os valores ajustados pela nossa equação ela retorna uma variável continua (de probabilidade que variam de $0\; a\; 1$), entretanto, nosso valor real é binário ($0\; ou\; 1$), dessa forma precisamos transformar estas probabilidades em classes ($0\; ou\; 1$), a maneira mais fácil e mais empregada para isso é definir um ponto de corte para o modelo, por exemplo $0,5$ e probabilidades abaixo disto são classificadas como $0$ e acima como $1$. Este valor pode ser melhor definido, mas inicialmente vamos considerá-lo depois verificando a qualidade do ajuste o definiremos melhor.
#' 
## ------------------------------------------------------------
#Criando as classes com base no valor de corte de 0.5
dist.estimada <- ifelse(fitted(modelo.arau)>0.5,1,0)

#' 
#' Para entendermos os termos de Verdadeiro Positivo, Verdadeiro Negativo, Falso Positivo e Faso Negativo considere a **Matriz de Confusão** teórica dada abaixo:
#' 
#' Estimado/Observado | $y=1$                         | $y=0$
#' -------------------|-------------------------------|-----------------------------
#' $\hat{y}=1$        | Verdadeiro Positivo           |Falso Positivo (Erro Tipo I)
#' $\hat{y}=0$        | Falso Negativo (Erro Tipo II) |Verdadeiro Negativo
#' 
#' **Prevalência**
#' 
#' O quanto do efito positivo (*1*) está presente na população.
#' 
#' $$Prevalência = \frac{\sum_{i=1}^{N}Condição\;Positiva}{N}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
Prevalencia <- length(araucaria[araucaria$Distribuicao == 1,1])/length(araucaria$Distribuicao)
Prevalencia

#' 
#' 
#' **Acurácia**
#' 
#' A métrica mais utilizada em problemas de classificação, refere-se a quanto o modelo acerta em relação ao total de predições.
#' 
#' $$Acc = \frac{Verdadeiros\; Positivos + Verdadeiros\; Negativos}{N}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
#calcular a acuracia
acc <- sum(araucaria$Distribuicao == dist.estimada)/length(dist.estimada)
acc

#' 
#' 
#' **Error**
#' 
#' Este é complemento da acurácia, dado por:
#' 
#' $$Error = \frac{Predições\; Incorretas}{N}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
#calcular o erro
erro <- sum(araucaria$Distribuicao != dist.estimada)/length(dist.estimada)
erro

#' 
#' **Sensibilidade ou Recall**
#' 
#' A sensibilidade se refere a proporção de verdadeiros positivos, isto é, o poder de predição de quando o modelo classifica um evento como sendo positivo dado que ele realmente é positivo. Esta medida resume o quão bem a classe positiva é estimada. 
#' 
#' $$Sensibilidade = \frac{Verdadeiro\; Positivo}{Verdadeiro \; Positivo + Falso \; Negativo}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
#Juntando os valores reais e os preditos
df <- data.frame(real=araucaria$Distribuicao, est=as.factor(dist.estimada))

df1 <- df[df$est == 1,]  #Corte na classe estimada 1
df0 <- df[df$est == 0,]  #Corte na classe estimada 0

sens <- sum(df1$real == df1$est)/(sum(df1$real == df1$est) + 
                                      sum(df0$real != df0$est))
sens

#' 
#' 
#' **Especificidade**
#' 
#' É a proporção de verdadeiro negativo, ou seja, a capacidade do modelo classificar um evento negativo quando ele realmente é negativo. Esta medida resume o quão bem o modelo classifica a classe verdadeiro negativo.
#' 
#' $$Especificidade = \frac{Verdadeiro\;Negativo}{Verdadeiro\;Negativo+Falso\;Positivo}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
espe <- sum(df0$real == df0$est)/(sum(df0$real == df0$est) + 
                                      sum(df1$real != df1$est))
espe

#' 
#' **Verdadeiro Predito Positivo (VPP) ou Precisão**
#' 
#' É a taxa de Verdadeiro Positivo com relação ao total de predições positivas, ou seja, é a capacidade do modelo prever um evento como positivo (*1*), dado que, ele realmente é positivo (*1*).
#' 
#' $$VPP = \frac{Verdadeiro\; Positivo}{Verdadeiro \; Positivo + Falso \; Positivo}$$
#' 
## ------------------------------------------------------------
options(digits = 3)
VPP <- sum(df1$real == df1$est)/(sum(df1$real == df1$est) + sum(df1$real != df1$est))
VPP

#' 
#' **Verdadeiro Predito Negativo (VPN)**
#' 
#' É a taxa de Verdadeiro Negativo com relação ao total de predições negativas, ou seja, é a capacidade do modelo prever um evento como negativo (*0*), dado que, ele realmente é negativo (*0*).
#' 
#' $$VPN = \frac{Verdadeiro\;Negativo}{Verdadeiro\;Negativo+Falso\;Negativo}$$
#' 
## ------------------------------------------------------------
options(digits = 3)

VPN <- sum(df0$real == df0$est)/(sum(df0$real == df0$est) + 
                                     sum(df0$real != df0$est) )
VPN

#' 
#' **G-mean**
#' 
#' Uma forma de resumir as infomações obtidas por meio da *sensibilidade* e da *especificidade* é utilizar a média geométrica (**G-mean**) destas métricas, logo, **G-mean** pode ser interpretado como uma meidada que balanceia a *sensibilidade* e a *especificidade*.
#' 
#' $$G-mean = \sqrt{Sensibilidade + Especificidade}$$
#' 
## ------------------------------------------------------------
G_mean <- sqrt(sens + espe)
G_mean

#' 
#' **Medidada F ou F score**
#' 
#' A medida F é a combinação entre a **Sensibilidade (Recall)** e a **Precisão (VPP)**, esta representa o balanço entre as duas métricas. Quando não temos hipóteses definidas sobre o que queremos com o nosso modelo (Maior precisão ou maior sensibilidade), então a medida F é a melhor métrica para balancear estas duas informações. É mais utilizada quando queremos comparar dois modelos ou quando temos o desbalanceamento entre as classes.
#' 
#' $$F-score = \frac{(2\cdot Precisão\cdot Sensibilidade)}{( Precisão + Sensibilidade)}$$
#' 
## ------------------------------------------------------------
F_score <- (2*VPP*sens)/(sens+VPP)
F_score

#' 
#' **Medidada F Beta ou F Beta score**
#' 
#' A medida F Beta é uma abstração da medida F, em que, o balanço entre a **sensibilidade** e a **precisão** no cálculo da *média harmônica* é controlado pelo parâmetro $\beta$.
#' 
#' $$Fbeta-score = \frac{(1+\beta)^2\cdot(2\cdot Precisão\cdot Sensibilidade)}{\beta^2 \cdot(Precisão + Sensibilidade)}$$
#' 
## ------------------------------------------------------------
beta <- 1
Fbeta_score <-  (1+beta)^2*(VPP*sens)/((beta)*(sens+VPP))

#' 
#' **Índice Kappa**
#' 
#' Proposto por Jacob Cohen em 1960, o teste de concordância Kappa ou índice Kappa (*K*) é empregado para avaliar a concordância entre duas amostras dependentes. Este teste é utilizada para avaliar a qualidade da classificação realizada pelo modelo. O valor do teste varia entre *0* e *1*, sendo que, quanto mais perto de *1* melhor a qualidade do classificador.
#' 
#' Vamos introduzir a fórmula do **índice Kappa**:
#' 
#' $$Kappa = \frac{PO - PE}{1-PE}$$
#' 
#' Em que, *PO* é a proporção de classificações corretas pelo tamanho amostral.
#' 
#' $$PO = \frac{Verdadeiro\;Positivo + Verdadeiro\;Negativo}{N}$$
#' 
#' *PE* é a proporção esperada de classificações corretas. Para calcularmos *PE* temos que particionar o cálculo, primeiro vamos encontrar a probabilidade de estimar uma classe como positiva "*1*" (PECP).
#' 
#' $$PECP = \frac{Verdadeiro\;Positivo + Falso\;Positivo}{N}$$
#' 
#' A probabilidade de ocorrência positiva "*1*" nos dados observados nós já conhecemos como **prevalência**. Agora calcularemos a interação da probabilidade, ou seja, a probabilidade conjunta de encontrarmos a classe positiva "*1*" nas estimativas e nos observados (PCPEO).
#' 
#' $$PCPEO = PECP \cdot Prevalência$$
#' 
#' Agora temos que calcular a probabilidade conjunta de encontrarmos a classe negativa "*0*" nas estimativas e nos observados (PCNEO), para tal vamos utiliar o complementar das métricas que já calculamos.
#' 
#' $$PCNEO = (1-PECP) \cdot (1-Prevalência)$$
#' 
#' Desta forma *PE* é cálculado por:
#' 
#' $$PE = PCPEO + PCNEO$$
#' 
## ------------------------------------------------------------
#Calculo do PO
PO <- (sum(df1$real == df1$est) + sum(df0$real == df0$est))/length(df$real)

#Calculo do PE
PECP <- (sum(df1$real == df1$est) + sum(df1$real != df1$est))/length(df$real)

PCPEO <- PECP * Prevalencia 
    
PCNEO <- (1-PECP) * (1-Prevalencia)

PE <- PCPEO + PCNEO

#Cálculo do índice Kappa
indice.kappa <- (PO - PE)/(1-PE)
indice.kappa

#' 
#' 
#' **Matriz de Confusão**
#' 
#' A matriz de confusão teórica já foi definida inicialmente para facilitar os cálculos, agora segue a implementação dela.
## ------------------------------------------------------------
#Matriz de confusão

matriz.confusao <- data.frame(OcorrenciaNegativa.0 = c(sum(df0$real == df0$est),
                                                       sum(df1$real != df1$est)),
                              OcorrenciaPositiva.1 = c(sum(df0$real != df0$est),
                                                       sum(df1$real == df1$est)),
                              row.names = c("EstimadoNegativo.0", "EstimadoPositivo.1"))
matriz.confusao

#' 
#' 
#' Uma forma mais simples e direta para realizarmos todos estes cálculos é empregar o que já temos implementados em pacotes prontos, aqui realizamos todos os cálculos para aprendizagem dos termos corretos e como são deduzidos os resultados. Vamos utilizar a função *confusionMatrix* do pacote **Caret** para realizarmos os cálculos.
#' 
## ----message=F-----------------------------------------------
library(caret)

#matriz de confusão
#OBS: Atente para a entrada dos parâmetros a inversão de entrada dos dados #estimados pelo real, inverte toda a matriz de confusão, levando a interpretação
#dos resultados de forma diferente.
confusionMatrix(df$est, df$real, positive = "1")

#' 
#' **Curva ROC**
#' 
#' A métrica mais utilizada em problemas de classificação é a **curva ROC** (Receiver Operating Characteristic) e resume o a abilidade do classificador binário em discriminar as classes. 
#' 
#' A curva ROC é um gráfico de diagnóstico que resume a qualidade de um modelo. Ela é calculada pela relação entre da **taxa de falso positivo** e a **taxa de verdadeiro positivo** de um conjunto de predições. Olhando para os eixos *x* e *y* do gráfico da curva ROC, podemos dizer que:
#' 
#' - Um ponto em (*0,0*): o modelo não é capaz de predizer uma classe positiva;
#' - Um ponto em (*1,1*): o modelo prediz todas as classes como positivas ou não prediz classes negativas;
#' - Um ponto em (*0,1*): o modelo é um excelente classificador.
#' 
#' Para produzirmos a curva ROC temos que calcular a **taxa de falso positivo (TFP)** e a **taxa de verdadeiro positivo (TVP)**, veja que, a **TVP** nós conhecemos como *sensibilidade* ou *recall*. A **TFP** é calculada por:
#' 
#' $$TFP = \frac{Falso\;Positivo}{(Falso\;Positivo+Verdadeiro\;Negativo)}$$
#' 
#' Vejamos uma figura, extraída de [Brownlee, 2020](https://machinelearningmastery.com/tour-of-evaluation-metrics-for-imbalanced-classification/), que resumo o que foi apresentado aqui.
#' 
#' ![Conhecendo a Curva ROC](/home/wesley/MEGAsync/CursosOnline/Modelos_Mistos_R/mixedModelsinR/Figuras/Depiction-of-a-ROC-Curve.jpg)
#' 
#' A área sob a curva ROC (**ROC AUC**) pode ser calculada e expressa uma importante métrica que reporta a qualidade do classificador, que pode ser empregada para comparar classificadores. 
#' 
#' $$ROCAUC = Área \; sob; a \; curva\; ROC$$
#' 
#' A curva ROC e a **ROC AUC** tendem a aprsentar a ser otimistas em problemas desbalanceados, principalmente quando a clase negativa apresenta o menor número de ocorrẽncia. Geralmente uma alternativa para classes desbalanceadas é utilizar a curva de sensibilidade e precisão que foca na classe com menor ocorrência. Vamos apresentar a curva ROC com base nos pacotes já implementados no R.
#' 
## ----echo=T, message=F---------------------------------------
#Curva ROC pacote ROCit
library(ROCit)
roc <- rocit(as.numeric(df$est), as.numeric(df$real))
plot(roc, col = c("#00BA37", "#F8766D"))

#Curva ROC pacote pROC
library(pROC)
plot.roc(as.numeric(df$real), as.numeric(df$est),
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

#' 
#' ## Regressão Logística com Efeitos Mistos
#' 
#' Como foi definido a **Regressão logística** é um tipo de modelo linear generalizado, sabemos que a variável resposta segue a distribuição binomial. Adicionando um pouco mais de complexidade a nossa análise, as parcelas foram alocadas em cinco áreas diferentes e estas introduzem uma variabilidade aleatória que deve ser prevista pelo modelo. Temos que a **variável área** possui cinco fatores e estss possuem correlação espacial entre si. 
#' 
#' A sintaxe para implementação de um **modelo linear generaizado misto** no *R* é semelhante a que já estudamos para os **modelos lineares mistos**, utilizaremos o mesmo pacote com poucos comandos diferentes. Para ajustarmos o modelo usaremos a função ```glmer()``` do pacote ```lme4```, além dos parâmetros que já adiconamos, iremos adicionar o comando ```control``` especifianco o algoritmo que irá otimizar os coeficientes do modelo. Vamos ajustar o modelo considerando sua forma mais parametrizada com as variáveis explicativas *Profundidade*, *Exposição* e *Área Basal* e com a variável *Área* especificando efeito aleatório.
#' 
## ------------------------------------------------------------
#modelo completo
mlgm.full <- glmer(Distribuicao ~ AreaBasal + Profundidade + Exposicao + 
                       (1 + AreaBasal + Profundidade + Exposicao|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))

#retirando variáveis (problemas de convergência)
mlgm.m1 <- glmer(Distribuicao ~ AreaBasal + Profundidade + Exposicao + 
                       (1 + Profundidade + Exposicao|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))

#retirando variáveis (problemas de convergência)
mlgm.m2 <- glmer(Distribuicao ~ AreaBasal + Profundidade + Exposicao + 
                       (1|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))

#retirando variáveis (problemas de convergência)
mlgm.m3 <- glmer(Distribuicao ~ Profundidade * Exposicao + (1|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))

#retirando variáveis (problemas de convergência)
mlgm.m4 <- glmer(Distribuicao ~ Profundidade * Exposicao  + 
                     (1+Profundidade + Exposicao|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))

#retirando variáveis (problemas de convergência)
mlgm.m5 <- glmer(Distribuicao ~ Profundidade + 
                     (1|Area),
                   data = araucaria, family = binomial(link = "logit"),
                   control = glmerControl(optimizer = "bobyqa"))


#' 
#' Como era de se esperar os modelos *full*, *m1* e *m2* não convergiram e modelo *m4* apresenta problemas na sua convergência, como já foi discutido aqui, a análise deste modelo, fica restria ao conjunto de dados empregado no ajsute.
#' 
#' Vamos vericar qual o melhor modelo destes que apresentaram convrgência, para depois podermos analisar os demais aspectos da modelagem.
#' 
## ------------------------------------------------------------
anova(mlgm.m3, mlgm.m4, mlgm.m5)

#' 
#' Com base na ANOVA temos que o melhor modelo foi o *mlgm.m3* com menor AIC e significativo pelo teste Qui-quadrado (*p Valor  < 2e-16*). Agora vamos analisar o resumo de estatísticas para o modelo selecionado.
#' 
## ------------------------------------------------------------
summary(mlgm.m3)

#' 
#' Temos que a interação entre a *Profundidade* e a *Exposição* não é significativa, os demais coeficientes apresentam estatísticas significantes. A interpretação destes parâmetros não é tão direta quanto as apresentadas. 
#' 
#' Para efeito de comparação vamos verificar o modelo *mlgm.m5* que é semelhante ao *modelo logístico* inicial.
#' 
## ------------------------------------------------------------
summary(mlgm.m5)

#' 
#' A discussão sobre os coeficientes é semelhante as que já foram apresentadas nos tópicos anteriores. 
#' 
#' ### Análise de resíudos para Modelos Lineares Generalizados Mistos com resposta dicotômica
#' 
#' O procedimento de análise dos resíduos não se compara aos modelos lineares ou lineares generalizados. Para seguir com a análise utilizaremos a proposta apresentada por [Hartig, 2016](https://theoreticalecology.wordpress.com/2016/08/28/dharma-an-r-package-for-residual-diagnostics-of-glmms/) por meio do pacote [DHARMa](https://cran.r-project.org/web/packages/DHARMa/index.html). Este procedimento simula novos resíduos com base no modelo ajsutado e nos dados.
#' 
## ----echo=T, fig.width=12, fig.height=6----------------------
#lembre que o pacote deve ser instalado inicialmente
library(DHARMa)

#simulando os residuos
dha.resid1 <- simulateResiduals(fittedModel = mlgm.m3, n = 1000, seed = 123)

#Grafico dos residuos
plot(dha.resid1)

#' 
#' Diante do exposto podemos afirmar que o modelo não apresenta desvios quanto a distribuição sugerida, tampouco, apresenta problemas relacionados aos valores estimados.
#' 
#' ### Estimar novos valores
#' 
#' Como usar a equação obtida por meio do modelo? No resumo (**summary**) estatístico só aparece os coeficientes do efeito fixo, então só precisamos dele para realizar as estimativas? A resposta desta questão é não, quando trabalhamos com **Modelos Lineares Mistos ou (Generalizados Mistos)** o efeito aleatório é um fator que possui diferentes níveis e a sintaxe de modelos mistos acaba ajustando uma equação para cada um desses níveis do fator. Logo, teremos sim a equação com os efeitos fixos, mas também estaremos interessados nas equações de cada nível do efeito aleatório. Vamos desenvolver nossa equação e depois criar um gráfico com as previsões para cada nível do fator correspondente ao efeito aleatório. 
#' 
## ------------------------------------------------------------
#armazenar na memória os coeficientes do modelo relacionados ao efeito fixo
fixCoef <- fixef(mlgm.m3)
fixCoef

#armazenar na memória os coeficientes do modelo relacionados ao efeito aleatorio
ranCoef <- coef(mlgm.m3)$Area
ranCoef

#Vamos selecionar uma parte do nosso banco de dados para servir como teste (o correto é ter um novo conjunto)

araucaria.test <- araucaria %>% 
    slice_sample(prop = .2)

araucaria.test$Profundidade <- as.numeric(araucaria.test$Profundidade)
araucaria.test$Exposicao <- as.numeric(araucaria.test$Exposicao)

#Estimativas para cada area
#area 1
area1 <- ranCoef[1,1] + ranCoef[1,2]*araucaria.test$Profundidade + 
    ranCoef[1,3]*araucaria.test$Exposicao + 
    ranCoef[1,4]*araucaria.test$Profundidade*araucaria.test$Exposicao
#area 2
area2 <- ranCoef[2,1] + ranCoef[2,2]*araucaria.test$Profundidade + 
    ranCoef[2,3]*araucaria.test$Exposicao + 
    ranCoef[2,4]*araucaria.test$Profundidade*araucaria.test$Exposicao
#area 3
area3 <- ranCoef[3,1] + ranCoef[3,2]*araucaria.test$Profundidade + 
    ranCoef[3,3]*araucaria.test$Exposicao + 
    ranCoef[3,4]*araucaria.test$Profundidade*araucaria.test$Exposicao

#area 4
area4 <- ranCoef[4,1] + ranCoef[4,2]*araucaria.test$Profundidade + 
    ranCoef[4,3]*araucaria.test$Exposicao + 
    ranCoef[4,4]*araucaria.test$Profundidade*araucaria.test$Exposicao

#area 5
area5 <- ranCoef[5,1] + ranCoef[5,2]*araucaria.test$Profundidade + 
    ranCoef[5,3]*araucaria.test$Exposicao + 
    ranCoef[5,4]*araucaria.test$Profundidade*araucaria.test$Exposicao

#Predizendo para os efeitos fixos
areaFix <- fixCoef[1] + fixCoef[2]*araucaria.test$Profundidade + 
    fixCoef[3]*araucaria.test$Exposicao + 
    fixCoef[4]*araucaria.test$Profundidade*araucaria.test$Exposicao


#Voltando para escala original
area1 <- ifelse(area1 > 0.5, 1, 0)
area2 <- ifelse(area2 > 0.5, 1, 0)
area3 <- ifelse(area3 > 0.5, 1, 0)
area4 <- ifelse(area4 > 0.5, 1, 0)
area5 <- ifelse(area5 > 0.5, 1, 0)
areaFix <- ifelse(areaFix > 0.5, 1, 0)

#Dataframe com Estimativas
df.est <- data.frame(areaFix, area1, area2, area3, area4, area5)
names(df.est) <- c("areaFix", "area1", "area2", "area3", "area4", "area5")
head(df.est)

#' 
#' 
#' ## Modelos Lineares Generalizados Mistos Outras Distribuições
#' 
#' ### A distribuição Poisson
#' 
#' Uma variável aleatória $X$ segue uma distribuição de *Poisson* se sua função de probabilidade é definida por:
#' 
#' $$P(X=\kappa) = \dfrac{e^{-\lambda} \lambda^{\kappa}}{\kappa}$$
#' 
#' em que: $\kappa = 0, 1, ...$, $\lambda > 0$ e refere-se a taxa de ocorrência. Assim podemos afirmar que $X \sim Po(\lambda)$. Esta distribuição (modelo) é bastante utilizada em experimentos biológicos em que a variável resposta está relacionada com a contagem de ocorrência de determinda evento. 
#' 
#' ### MLGM com resposta Poison
#' 
#' No nosso primeiro experimento vimos que a riqueza **Richness** se refere a contagem de espécies que ocorreram em diferentes praias da Holando, também verificamos que o modelo *m2* ajsutado  apresentou diagnóstico do resíduo que se desviava dos pressupostos do modelo. Assim faremos um novo ajuste considerando a *distribuição Poisson*.
#' 
## ------------------------------------------------------------
mlgm.po1 <- glmer(Richness ~ NExposure*NAP + (1|Beach),
                   data = dados, family = poisson)
summary(mlgm.po1)

mlgm.po2 <- glmer(Richness ~ NExposure*NAP + (NAP|Beach),
                   data = dados, family = poisson)
summary(mlgm.po2)



#' 
#' Agora vamos comparar com o modelo *m2* ajsutado anteriomente.
#' 
## ------------------------------------------------------------
anova(m2, mlgm.po1, mlgm.po2, refit = F)

#' 
#' Os modelos diferem pelo teste Qui-quadrado, sendo assim inicialmente podemos escolher o modelo *mlgm.po2*. A partir da escolha correta do efeito aleatório e de saber como ele está relacionado com as variáveis explicativas (aqui definimos o efeito aleatório **Beach** com *slopes* aleatórios para a variável **NAP**) passamos a verificar como as variáveis explicativas estão relacionadas. O passo é verificar se elas possuem relação aditiva (```NExposure+NAP```), ou se existe realmente interação como foi definido (```NExposure*NAP```) e até mesmo se podemos excluir algumas dessas variáveis. 
#' 
#' ### Análise de Resíduos em Modelos Lineares Generalizados Mistos
#' 
#' A análise de resíduos para modelos **Modelos Lineares Generalizados Mistos** não pode ser realizada da mesma forma que para os modelos lineares, já que os pressupostos de normalidade e homogeneidade de variâncias não necessáriamente precisam ser atendidos. Com relação aos resíduos de Pearson ou a Deviance, estes não podem garantir que o modelo está bem especificado como é esperado para os modelos lineares generalizados. A forma apresentada por [Labtrop](http://labtrop.ib.usp.br/doku.php?id=cursos:planeco:roteiro:12-glmm) e implementada por [Hartig, 2016](https://theoreticalecology.wordpress.com/2016/08/28/dharma-an-r-package-for-residual-diagnostics-of-glmms/) é uma importante ferramente para diagnóstico dos resíduos desses modelos.  Para utilizarmos esta ferramenta faremos uso do pacote [DHARMa](https://cran.r-project.org/web/packages/DHARMa/index.html). Este procedimento cria novos resíduos escalondados entre $0$ e $1$, para tal ele, simula novos conjuntos de dados com as pressuposições do modelo e ajusta os parâmetros, posteriormente os resíduos são definidos para cada observação como a proporção das observações simuladas (estimadas) que são menores que o valor observado.
#' 
#' Vamos criar um vetor com estes resíudos simulados e posteriormente gerar os gráficos dos resíduos, que será um gráfico qqplot para avaliar se os resíduos seguem a distribuição proposta e o gráfico dos resíduos versus os valores ajustados. Para gerarmos a simulação precisamos informar o número de simulações que faremos, [DHARMa](https://cran.r-project.org/web/packages/DHARMa/index.html) utiliza $250$ como padrão, mas  [Labtrop](http://labtrop.ib.usp.br/doku.php?id=cursos:planeco:roteiro:12-glmm) recomenda ao menos $1000$ a depender da capacidade computacional.
#' 
## ----echo=T, fig.width=12, fig.height=6----------------------
#lembre que o pacote deve ser instalado inicialmente
library(DHARMa)

#simulando os residuos
dha.resid <- simulateResiduals(fittedModel = mlgm.po2, n = 1000, seed = 123)

#Grafico dos residuos
plot(dha.resid)

#' 
#' Pelo gráfico **QQplot** não verificamos desvio quanto a distribuição sugerida no modelo, bem como, verificamos que estes resíduos também são normais pelo teste de Shapiro-Wilk a dispersão também não é significativa. No gráfico de **resíudos vs preditos** temos três linhas horizontais, estas deviam ser *retas* paralelas, já que as linhas representam a regressão quantílica para cada quantil (0,25; 0,50; 0,75). Estas linhas detectam se existe desvio da uniformidade em relação a $y$. Nossa linhas não são paralelas, mas não possuem desvios considerados graves. Logo, podemos concluir que o modelo está bem especificado.
