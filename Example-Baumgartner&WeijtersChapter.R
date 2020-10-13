
## Empirical application of CFA and SEM based on open access data 
## source: Diop, E. B., Zhao, S., & Duy, T. V. (2019). An extension of the technology acceptance model 
##         for understanding travelers' adoption of variable message signs. PLOS ONE, 14(4), e0216007.


########################################################################
## SECTION 1:
## Install (if necessary) and load required packages and read the data
## Conduct an exploratory data analysis
########################################################################

#install.packages("lavaan")
#install.packages("openxlsx")
#install.packages("semTools")
#install.packages("semPlot")
#install.packages("lavaanPlot")
#install.packages("psych")
#install.packages("car")
#install.packages("corrplot")
#install.packages("gplots")
#install.packages("ggpubr")
#install.packages("ggplot2")
library(lavaan)      # for CFA and SEM
library(openxlsx)    # to read the data
library(semTools)    # to get additional fit indices
library(semPlot)     # to do plots
library(lavaanPlot)  # to do plots
library(psych)       # to use the describe function
library(car)         # to produce a scatterplot matrix
library(corrplot)    # to do a correlation plot
library(gplots)      # needed for color interpolation
library(ggplot2)     # needed for histogram
library(ggpubr)      # needed for histogram

#read the data directly from the PLOS ONE website
vmsdata<-read.xlsx("https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0216007.s001",1)
str(vmsdata)
#pick the relevant variables
head(vmsdata[,c(2:9,16:18)])
SampleSize<-nrow(vmsdata)
#summary statistics for relevant variables
summary(vmsdata[,c(2:9,16:18)])
describe(vmsdata[,c(2:9,16:18)]) 
#histograms
hist.pu1<-ggplot(data=vmsdata,aes(x=PU1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.pu2<-ggplot(data=vmsdata,aes(x=PU2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.pu3<-ggplot(data=vmsdata,aes(x=PU3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.pu4<-ggplot(data=vmsdata,aes(x=PU4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
figure1 <- ggarrange(hist.pu1,hist.pu2,hist.pu3,hist.pu4, ncol = 2, nrow = 2)
figure1

hist.peou1<-ggplot(data=vmsdata,aes(x=PEOU1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.peou2<-ggplot(data=vmsdata,aes(x=PEOU2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.peou3<-ggplot(data=vmsdata,aes(x=PEOU3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.peou4<-ggplot(data=vmsdata,aes(x=PEOU4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
figure2 <- ggarrange(hist.peou1,hist.peou2,hist.peou3,hist.peou4, ncol = 2, nrow = 2)
figure2

hist.bi1<-ggplot(data=vmsdata,aes(x=BI1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.bi2<-ggplot(data=vmsdata,aes(x=BI2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist.bi3<-ggplot(data=vmsdata,aes(x=BI3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
figure3 <- ggarrange(hist.bi1,hist.bi2,hist.bi3, ncol = 2, nrow = 2)
figure3

#skewness and kurtosis
skew(vmsdata[,c(2:9,16:18)], na.rm = TRUE,type=3)
kurtosi(vmsdata[,c(2:9,16:18)], na.rm = TRUE,type=3)
mardia(vmsdata[,c(2:9,16:18)],na.rm = TRUE,plot=TRUE)

#scatterplotMatrix(formula = ~ PU1 + PU2 + PU3 + PU4 + PEOU1 + PEOU2 + PEOU3 + PEOU4 + BI1 + BI2 + BI3, data=vmsdata)
scatterplotMatrix(formula = ~ jitter(PU1) + jitter(PU2) + jitter(PU3) + jitter(PU4) + 
                              jitter(PEOU1) + jitter(PEOU2) + jitter(PEOU3) + jitter(PEOU4) + 
                              jitter(BI1) + jitter(BI2) + jitter(BI3),data=vmsdata)

#cor(vmsdata[,c(2:9,16:18)])
corrplot.mixed(corr=cor(vmsdata[ , c(2:9,16:18)], use="complete.obs"),
  upper="ellipse", tl.pos="lt", upper.col = colorpanel(50, "red", "gray60", "blue4"))


#########################
## SECTION 2:
## Measurement analysis
#########################

#Specify the initial CFA model for the TAM variables (PU, PEOU, BI)
cfa1<- '
 PU=~NA*PU1+PU2+PU3+PU4
 PEOU=~NA*PEOU1+PEOU2+PEOU3+PEOU4
 BI=~NA*BI1+BI2+BI3
 PU ~~ 1*PU
 PEOU ~~ 1*PEOU
 BI ~~ 1*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
'
#Fit cfa1 to the data via ML and report the results
fit_cfa1_ML<-cfa(model=cfa1,data=vmsdata)
fitMeasures(fit_cfa1_ML, fit.measures = "all")
summary(fit_cfa1_ML, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Fit cfa1 to the data via MLM and report the results
#this provides the Satorra-Bentler mean-corrected Tmlm, although it differs from the one given in Mplus
fit_cfa1_MLM<-cfa(model=cfa1,data=vmsdata,estimator="MLM",test="satorra.bentler")
FitStats<-fitMeasures(fit_cfa1_MLM, fit.measures = "all")
FitStats
summary(fit_cfa1_MLM,  fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Barlett correction of regular chi-square for p=11 and m=3
p<-11 # number of variables
m<-3  # number of factors
BartlettTML<-(SampleSize-p/3-2*m/3-11/6)*(FitStats[[3]]/(SampleSize-1))
#Barlett correction of Satorra-Bentler scaled chi-square for p=11 and m=3
BartlettTMLM<-(SampleSize-p/3-2*m/3-11/6)*(FitStats[[6]]/(SampleSize-1))
c(BartlettTML,BartlettTMLM) 
#using semTools to compute the Bartlett correction for regular ch-square (plus three other corrections)
chisqSmallN(fit_cfa1_ML,smallN.method = c("all"))

#using the F distribution (McNeish 2018)
Tml<-FitStats[3]
Tmlm<-FitStats[6]
df<-FitStats[4]
pfTml<-pf(Tml/df, df, SampleSize-1, lower.tail = FALSE)
pfTmlm<-pf(Tmlm/df, df, SampleSize-1, lower.tail = FALSE)
names(pfTml) <- 'pFTml'
names(pfTmlm) <- 'pFTmlm'
c(df,Tml,pfTml,Tmlm,pfTmlm)

#check residuals
lavResiduals(fit_cfa1_ML, type = "cor", zstat = TRUE, summary = TRUE)

#Check modification indices and EPCs
mi_cfa1_ML<-as.data.frame(modindices(fit_cfa1_ML))
mi_cfa1_ML

#respecify the measurement model by dropping BI3
cfa2<- '
 PU=~NA*PU1+PU2+PU3+PU4
 PEOU=~NA*PEOU1+PEOU2+PEOU3+PEOU4
 BI=~NA*BI1+BI2
 PU ~~ 1*PU
 PEOU ~~ 1*PEOU
 BI ~~ 1*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
'
#fit revised CFA to the data using ML
#report results and modification indices
fit_cfa2_ML<-cfa(model=cfa2,data=vmsdata)
summary(fit_cfa2_ML, fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
mi_cfa2_ML<-as.data.frame(modindices(fit_cfa2_ML))
mi_cfa2_ML

#fit revised CFA to the data using MLM
fit_cfa2_MLM<-cfa(model=cfa2,data=vmsdata, test="Satorra-Bentler")
summary(fit_cfa2_MLM, fit.measures=TRUE,standardized=TRUE)

#draw path diagram
semPaths(fit_cfa2_ML,title=FALSE)
semPaths(fit_cfa2_ML,"std", edge.label.cex = 0.5, exoVar = FALSE,
         exoCov = FALSE)

#display model graph
lavaanPlot(model = fit_cfa2_ML, coefs = TRUE, stand = TRUE,  covs = TRUE)

#compute iir, ave, cr and sv
#store standardized estimates
std_cfa2_ML<-standardizedSolution(fit_cfa2_ML) 
std_cfa2_ML
#compute iir
iir_cfa2_ML<-std_cfa2_ML[1:10,4]^2
iir_cfa2_ML
#compute AVE
ave_pu_cfa2_ML<-mean(iir_cfa2_ML[1:4])
ave_peou_cfa2_ML<-mean(iir_cfa2_ML[5:8])
ave_bi_cfa2_ML<-mean(iir_cfa2_ML[9:10])
c(ave_pu_cfa2_ML,ave_peou_cfa2_ML,ave_bi_cfa2_ML)
#intermediate steps for CR (see equation 4 in the chapter)
sql_pu_cfa2_ML<-sum(std_cfa2_ML[1:4,4])^2
sql_peou_cfa2_ML<-sum(std_cfa2_ML[5:8,4])^2
sql_bi_cfa2_ML<-sum(std_cfa2_ML[9:10,4])^2
res_pu_cfa2_ML<-sum(std_cfa2_ML[17:20,4])
res_peou_cfa2_ML<-sum(std_cfa2_ML[21:24,4])
res_bi_cfa2_ML<-sum(std_cfa2_ML[25:26,4])
#compute CR
cr_pu_cfa2_ML<-sql_pu_cfa2_ML/(sql_pu_cfa2_ML+res_pu_cfa2_ML)
cr_peou_cfa2_ML<-sql_peou_cfa2_ML/(sql_peou_cfa2_ML+res_peou_cfa2_ML)
cr_bi_cfa2_ML<-sql_bi_cfa2_ML/(sql_bi_cfa2_ML+res_bi_cfa2_ML)
c(cr_pu_cfa2_ML,cr_peou_cfa2_ML,cr_bi_cfa2_ML)
#compute SV
sv_pu_peou_cfa2_ML<-std_cfa2_ML[14,4]^2
sv_pu_bi_cfa2_ML<-std_cfa2_ML[15,4]^2
sv_peou_bi_cfa2_ML<-std_cfa2_ML[16,4]^2
c(sv_pu_peou_cfa2_ML,sv_pu_bi_cfa2_ML,sv_peou_bi_cfa2_ML)

#analysis with parceled indicators
vmsdata$PEOUparcel<-(vmsdata$PEOU1+vmsdata$PEOU2+vmsdata$PEOU3)/3
vmsdata$PUparcel<-(vmsdata$PU1+vmsdata$PU2+vmsdata$PU3)/3

cfa3<- '
 PU=~NA*PUparcel+PU4
 PEOU=~NA*PEOUparcel+PEOU4
 BI=~NA*BI1+BI2
 PU ~~ 1*PU
 PEOU ~~ 1*PEOU
 BI ~~ 1*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
'
#fit parceled CFA to the data using ML
#report results and modification indices
fit_cfa3_ML<-cfa(model=cfa3,data=vmsdata)
summary(fit_cfa3_ML, fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
mi_cfa3_ML<-as.data.frame(modindices(fit_cfa3_ML))
mi_cfa3_ML


#########################################
## SECTION 3:
## Analysis of the latent variable model
#########################################

#specify SEM with mediation and indirect effect
sem<- '
 #measurement model
 PU=~NA*PU1+PU2+PU3+1*PU4
 PEOU=~NA*PEOU1+PEOU2+PEOU3+1*PEOU4
 BI=~1*BI1+BI2
 PEOU ~~ NA*PEOU
 #direct effect
 BI~c*PEOU
 #mediating effects
 PU~a*PEOU
 BI~b*PU
 #indirect effect
 ab:=a*b
 #total effect
 total:=c+(a*b)
'

#fit SEM to the data with ML se's and report results
fit_sem_ML<-cfa(model=sem,data=vmsdata, estimator="ML")
summary(fit_sem_ML,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
parameterEstimates(fit_sem_ML, ci = TRUE, level = 0.95, standardized = T)
standardizedSolution(fit_sem_ML,ci = TRUE)

#fit SEM to the data with MLM se's and report results
fit_sem_MLM<-cfa(model=sem,data=vmsdata, estimator="MLM")
summary(fit_sem_MLM,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
parameterEstimates(fit_sem_MLM, ci = TRUE, level = 0.95, standardized = T)
standardizedSolution(fit_sem_MLM,ci = TRUE)

#fit SEM to the data with bootstrapped se's and report results
fit_sem_boot<-cfa(model=sem,data=vmsdata, se="bootstrap")
summary(fit_sem_boot, fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
standardizedSolution(fit_sem_boot,ci = TRUE)

#optional: plot model with standardized estimates -- this takes a while
lavaanPlot(model = fit_sem_boot, coefs = TRUE, stand = TRUE)


##########################
## SECTION 4:
## Multi-sample analysis
##########################

#specify unconstrained CFA model
cfa2_2g_uc<- '
 PU=~NA*PU1+PU2+PU3+1*PU4
 PEOU=~NA*PEOU1+PEOU2+PEOU3+1*PEOU4
 BI=~1*BI1+BI2
 PU ~~ NA*PU
 PEOU ~~ NA*PEOU
 BI ~~ NA*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
 PU1 ~ 1
 PU2 ~ 1
 PU3 ~ 1
 PU4 ~ 0*1
 PEOU1 ~ 1
 PEOU2 ~ 1
 PEOU3 ~ 1
 PEOU4 ~ 0*1
 BI1 ~ 0*1
 BI2 ~ 1
 PU ~ 1
 PEOU ~ 1
 BI ~ 1
'
fit_cfa2_2g_uc <-cfa(model=cfa2_2g_uc,data=vmsdata, group = "GENDER")
summary(fit_cfa2_2g_uc)
fitMeasures(fit_cfa2_2g_uc, fit.measures = "all")

#specify full metric invariance CFA model
cfa2_2g_mi <- '
 PU=~NA*PU1+c(l1,l1)*PU1+c(l2,l2)*PU2+c(l3,l3)*PU3+1*PU4
 PEOU=~NA*PEOU1+c(l4,l4)*NA*PEOU1+c(l5,l5)*PEOU2+c(l6,l6)*PEOU3+1*PEOU4
 BI=~1*BI1+c(l7,l7)*BI2
 PU ~~ NA*PU
 PEOU ~~ NA*PEOU
 BI ~~ NA*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
 PU1 ~ 1
 PU2 ~ 1
 PU3 ~ 1
 PU4 ~ 0*1
 PEOU1 ~ 1
 PEOU2 ~ 1
 PEOU3 ~ 1
 PEOU4 ~ 0*1
 BI1 ~ 0*1
 BI2 ~ 1
 PU ~ 1
 PEOU ~ 1
 BI ~ 1
'
fit_cfa2_2g_mi <-cfa(model=cfa2_2g_mi,data=vmsdata, group = "GENDER")
summary(fit_cfa2_2g_mi)
fitMeasures(fit_cfa2_2g_mi, fit.measures = "all")
mi_cfa2_2g_mi<-as.data.frame(modindices(fit_cfa2_2g_mi))
mi_cfa2_2g_mi[mi_cfa2_2g_mi$op == "=~",]

#to get MI's for the invariant parameters 
lavTestScore(fit_cfa2_2g_mi, cumulative = TRUE)

#specify partial metric invariance CFA model
cfa2_2g_pmi <- '
 PU=~NA*PU1+c(l1,l1)*PU1+c(l2,l2)*PU2+c(l3,l3)*PU3+1*PU4
 PEOU=~NA*PEOU1+c(l4,l4)*NA*PEOU1+c(l5,l5)*PEOU2+c(l61,l62)*PEOU3+1*PEOU4
 BI=~1*BI1+c(l7,l7)*BI2
 PU ~~ NA*PU
 PEOU ~~ NA*PEOU
 BI ~~ NA*BI
 PU ~~ NA*PEOU
 PU ~~ NA*BI
 PEOU ~~ NA*BI
 PU1 ~ 1
 PU2 ~ 1
 PU3 ~ 1
 PU4 ~ 0*1
 PEOU1 ~ 1
 PEOU2 ~ 1
 PEOU3 ~ 1
 PEOU4 ~ 0*1
 BI1 ~ 0*1
 BI2 ~ 1
 PU ~ 1
 PEOU ~ 1
 BI ~ 1
'
fit_cfa2_2g_pmi <-cfa(model=cfa2_2g_pmi,data=vmsdata, group = "GENDER")
summary(fit_cfa2_2g_pmi)
fitMeasures(fit_cfa2_2g_pmi, fit.measures = "all")

#Specify SEM with partially metrically invariant loadings
sem_2g<- '
 PU=~NA*PU1+c(l1,l1)*PU1+c(l2,l2)*PU2+c(l3,l3)*PU3+1*PU4
 PEOU=~NA*PEOU1+c(l4,l4)*NA*PEOU1+c(l5,l5)*PEOU2+c(l61,l62)*PEOU3+1*PEOU4
 BI=~1*BI1+c(l7,l7)*BI2
 PEOU ~~ NA*PEOU
 PU ~~ NA*PU
 BI ~~ NA*BI
 PU1 ~ 1
 PU2 ~ 1
 PU3 ~ 1
 PU4 ~ 0*1
 PEOU1 ~ 1
 PEOU2 ~ 1
 PEOU3 ~ 1
 PEOU4 ~ 0*1
 BI1 ~ 0*1
 BI2 ~ 1
 PU ~ 1
 PEOU ~ 1
 BI ~ 1
 PU~c(a1,a2)*PEOU
 BI~c(b1,b2)*PU
 BI~c(c1,c2)*PEOU
 d_a:=a1-a2
 d_b:=b1-b2
 d_c:=c1-c2
'
fit_sem_2g <-cfa(model=sem_2g,data=vmsdata, group = "GENDER")
summary(fit_sem_2g)
fitMeasures(fit_sem_2g, fit.measures = "all")

#initialize a summary table with fit indices 
fittable_mg <- data.frame(matrix(ncol = 7,nrow= 4))
colnames(fittable_mg)<-c("model","chisq","df", "tli","cfi", "rmsea", "srmr")

#store fit indices in table
fittable_mg[1,1] <- 'uc'
fittable_mg[2,1] <- 'fullmi'
fittable_mg[3,1] <- 'partialmi'
fittable_mg[4,1] <- 'sem'
fittable_mg[1,2:7] <- fitMeasures(fit_cfa2_2g_uc,c("chisq","df", "tli","cfi", "rmsea", "srmr"))
fittable_mg[2,2:7] <- fitMeasures(fit_cfa2_2g_mi,c("chisq","df", "tli","cfi", "rmsea", "srmr"))
fittable_mg[3,2:7] <- fitMeasures(fit_cfa2_2g_pmi,c("chisq","df", "tli","cfi", "rmsea", "srmr"))
fittable_mg[4,2:7] <- fitMeasures(fit_sem_2g,c("chisq","df", "tli","cfi", "rmsea", "srmr"))
fittable_mg

#using semTools to perform invariance tests
measurementInvariance(model=cfa2_2g_uc, data = vmsdata, group = "GENDER")
#using semTools to compare the various models
compareFit(fit_cfa2_2g_uc, fit_cfa2_2g_mi, nested = TRUE)
compareFit(fit_cfa2_2g_mi, fit_cfa2_2g_pmi, nested = TRUE)
compareFit(fit_cfa2_2g_uc, fit_cfa2_2g_pmi, nested = TRUE)

#report results of the multi-group SEM
summary(fit_sem_2g,standardized=TRUE)
standardizedSolution(fit_sem_2g,ci = TRUE)
