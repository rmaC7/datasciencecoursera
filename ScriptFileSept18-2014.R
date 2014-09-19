library(relimp, pos=4)
load("/home/ubuntu/Documents/August 29-2014Workspace.RData")
load("/home/ubuntu/Documents/June22014FLOWDataset.RData")
cor.test(Dataset$SCHOOL, Dataset$stdINTRINSIC, alternative="greater", method="kendall")
cor.test(Dataset$STAND, Dataset$stdINTRINSIC, alternative="greater", method="kendall")
library(relimp, pos=4)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
         maxheight=30, suppress.X11.warnings=FALSE)
Dataset$MLU <- with(Dataset, LEAST_MLU+ MAIN_MLU+ MOST_MLU+ EMOTIONS_MLU)
cor.test(Dataset$MLU, Dataset$stdINTRINSIC, alternative="greater", method="kendall")
cor.test(Dataset$MLU, Dataset$stdGPA, alternative="greater", method="kendall")
GLM.1 <- glm(school ~ stdINTRINSIC +stdGPA, family=binomial(logit), data=Dataset)
summary(GLM.1)
GLM.2 <- glm(SCHOOL ~ stdGPA + stdINTRINSIC, family=binomial(logit), data=Dataset)
summary(GLM.2)
LinearModel.3 <- lm(SCHOOL ~ stdGPA + stdINTRINSIC, data=Dataset)
summary(LinearModel.3)
cor.test(Dataset$ATTEND, Dataset$stdINTRINSIC, alternative="greater", method="kendall")
LinearModel.4 <- lm(stdTRANSFORMTIME ~ stdGPA + stdINTRINSIC, data=Dataset)
summary(LinearModel.4)
LinearModel.5 <- lm(stdClrGOALSFDBK ~ stdGPA + stdINTRINSIC, data=Dataset)
summary(LinearModel.5)
LinearModel.6 <- lm(stdMergeACTAWRE ~ stdGPA + stdINTRINSIC, data=Dataset)
summary(LinearModel.6)
load("/home/ubuntu/Documents/JUNE30-2014-FLOW-HAPPINESS STUDYREWORKED DATA SHEET -2-StandardizedVarsCSV.csv")
Dataset <- 
  read.table("/home/ubuntu/Documents/JUNE30-2014-FLOW-HAPPINESS STUDYREWORKED DATA SHEET -2-StandardizedVarsCSV.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
AnovaModel.7 <- (lm(varAUTOTELIC ~ ORIENTATION, data=Dataset))
Anova(AnovaModel.7)
tapply(Dataset$varAUTOTELIC, list(ORIENTATION=Dataset$ORIENTATION), mean, na.rm=TRUE) 
# means
tapply(Dataset$varAUTOTELIC, list(ORIENTATION=Dataset$ORIENTATION), sd, na.rm=TRUE) 
# std. deviations
tapply(Dataset$varAUTOTELIC, list(ORIENTATION=Dataset$ORIENTATION), function(x) 
  sum(!is.na(x))) # counts
library(multcomp, pos=4)
library(abind, pos=4)
AnovaModel.8 <- aov(stdTRANSFORMTIME ~ ORIENTATION, data=Dataset)
summary(AnovaModel.8)
numSummary(Dataset$stdTRANSFORMTIME , groups=Dataset$ORIENTATION, statistics=c("mean", 
                                                                               "sd"))
.Pairs <- glht(AnovaModel.8, linfct = mcp(ORIENTATION = "Tukey"))
summary(.Pairs) # pairwise tests
confint(.Pairs) # confidence intervals
cld(.Pairs) # compact letter display
old.oma <- par(oma=c(0,5,0,0))
plot(confint(.Pairs))
par(old.oma)
remove(.Pairs)
library(MASS, pos=4)
cor.test(Dataset$stdGPA, Dataset$stdTRANSFORMTIME, alternative="two.sided", 
         method="kendall")
cor.test(Dataset$HON, Dataset$stdGPA, alternative="two.sided", method="kendall")
cor.test(Dataset$stdINTRINSIC, Dataset$YEAR, alternative="two.sided", method="kendall")
Hist(Dataset$AGE, scale="frequency", breaks="Sturges", col="darkgray")
matplot(Dataset$AGE, Dataset[, c("stdINTRINSIC")], type="b", lty=1, ylab="", pch=1)
AnovaModel.9 <- (lm(AGE ~ ORIENTATION, data=Dataset))
Anova(AnovaModel.9)
tapply(Dataset$AGE, list(ORIENTATION=Dataset$ORIENTATION), mean, na.rm=TRUE) # means
tapply(Dataset$AGE, list(ORIENTATION=Dataset$ORIENTATION), sd, na.rm=TRUE) 
# std. deviations
tapply(Dataset$AGE, list(ORIENTATION=Dataset$ORIENTATION), function(x) sum(!is.na(x))) 
# counts
GLM.10 <- glm(ACTION ~ AGE, family=poisson(identity), data=Dataset)
summary(GLM.10)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
         maxheight=30, suppress.X11.warnings=FALSE)
GLM.11 <- glm(SCHOOL ~ AGE +GPA +SUM_INTRINSIC, family=binomial(logit), data=Dataset)
summary(GLM.11)
GLM.12 <- glm(SCHOOL ~ stdGPA +stdINTRINSIC, family=binomial(logit), data=Dataset)
summary(GLM.12)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
         maxheight=30, suppress.X11.warnings=FALSE)
.Z <- scale(Dataset[,c("ACTION")])
Dataset$Z.ACTION <- .Z[,1]
remove(.Z)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
         maxheight=30, suppress.X11.warnings=FALSE)
.Z <- scale(Dataset[,c("AGE")])
Dataset$Z.AGE <- .Z[,1]
remove(.Z)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
         maxheight=30, suppress.X11.warnings=FALSE)
.Z <- scale(Dataset[,c("GPA","SUM_INTRINSIC","SUM_SATISFACTION","varAUTOTELIC",
                       "varCHALLNGSKILL","varCONCENT","varLossofSELF","varMergeACTAWRE","varPRDXCONTROL",
                       "varTRANSFORMTIME")])
Dataset$Z.GPA <- .Z[,1]
Dataset$Z.SUM_INTRINSIC <- .Z[,2]
Dataset$Z.SUM_SATISFACTION <- .Z[,3]
Dataset$Z.varAUTOTELIC <- .Z[,4]
Dataset$Z.varCHALLNGSKILL <- .Z[,5]
Dataset$Z.varCONCENT <- .Z[,6]
Dataset$Z.varLossofSELF <- .Z[,7]
Dataset$Z.varMergeACTAWRE <- .Z[,8]
Dataset$Z.varPRDXCONTROL <- .Z[,9]
Dataset$Z.varTRANSFORMTIME <- .Z[,10]
remove(.Z)
.Z <- scale(Dataset[,c("AWARENESS")])
Dataset$Z.AWARENESS <- .Z[,1]
remove(.Z)
Dataset$Fact_Z.ACTION <- recode(Dataset$Z.ACTION, '-5:0=0; 0:5=1', 
                                as.factor.result=TRUE)
Dataset$Fact_Z.AGE <- recode(Dataset$Z.AGE, '-5:0=0; 0:5=1', as.factor.result=TRUE)
Dataset$Fact_Z.AWARENESS <- recode(Dataset$Z.AWARENESS, '-5:0=0; 0:5=1', 
                                   as.factor.result=TRUE)
Dataset$Fact_Z.GPA <- recode(Dataset$Z.GPA, '-5:0=0; 0:5=1', as.factor.result=TRUE)
Dataset$Fact_Z.SUM_INTRINSIC <- recode(Dataset$Z.SUM_INTRINSIC, '-5:0=0; 0:5=1', 
                                       as.factor.result=TRUE)
Dataset$Fact_Z.SUM_SATISFACTION <- recode(Dataset$Z.SUM_SATISFACTION, '-5:0=0; 0:5=1', 
                                          as.factor.result=TRUE)
Dataset$Fact_Z.varAUTOTELIC <- recode(Dataset$Z.varAUTOTELIC, '-5:0=0; 0:5=1', 
                                      as.factor.result=TRUE)
Dataset$Fact_Z.varCHALLNGSKILL <- recode(Dataset$Z.varCHALLNGSKILL, '-5:0=0; 0:5=1', 
                                         as.factor.result=TRUE)
Dataset$Fact_Z.varCONCENT <- recode(Dataset$Z.varCONCENT, '-5:0=0; 0:5=1', 
                                    as.factor.result=TRUE)
Dataset$Fact_Z.varLossofSELF <- recode(Dataset$Z.varLossofSELF, '-5:0=0; 0:5=1', 
                                       as.factor.result=TRUE)
Dataset$Fact_Z.varMergeACTAWRE <- recode(Dataset$Z.varMergeACTAWRE, '-5:0=0; 0:5=1', 
                                         as.factor.result=TRUE)
Dataset$Fact_Z.varPRDXCONTROL <- recode(Dataset$Z.varPRDXCONTROL, '-5:0=0; 0:5=1', 
                                        as.factor.result=TRUE)
Dataset$Fact_Z.varTRANSFORMTIME <- recode(Dataset$Z.varTRANSFORMTIME, '-5:0=0; 0:5=1', 
                                          as.factor.result=TRUE)
View(Dataset)
View(Dataset)
.Z <- scale(Dataset[,c("SCHOOL")])
Dataset$Z.SCHOOL <- .Z[,1]
remove(.Z)
LinearModel.13 <- lm(Z.SCHOOL ~ Z.GPA +Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.13)
View(Dataset)
Dataset$Fact_ <- recode(Dataset$Z.SCHOOL, '-5:0=0; 0:5=1;', as.factor.result=TRUE)
Dataset$Fact_Z.SCHOOL <- recode(Dataset$Z.SCHOOL, '-5:0=0; 0:5=1; ;', 
                                as.factor.result=TRUE)
Dataset$Fact_Z.SUM_SATISFACTION <- recode(Dataset$Z.SUM_SATISFACTION, 
                                          '-5:0=0; 0:5=1; ;', as.factor.result=TRUE)
View(Dataset)
LinearModel.14 <- lm(Fact_Z.SCHOOL ~ Fact_Z.GPA +Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.14)
LinearModel.15 <- lm(Fact_Z.SCHOOL ~ Fact_Z.SUM_INTRINSIC +Fact_Z.GPA, data=Dataset)
summary(LinearModel.15)
GLM.16 <- glm(Fact_Z.SCHOOL ~ Fact_Z.GPA +Fact_Z.SUM_INTRINSIC, family=binomial(logit), 
              data=Dataset)
summary(GLM.16)
GLM.17 <- glm(Fact_Z.SCHOOL ~ Fact_Z.GPA + Fact_Z.SUM_INTRINSIC + Fact_Z.AGE, 
              family=binomial(logit), data=Dataset)
summary(GLM.17)
library(nnet, pos=4)
MLM.18 <- multinom(Fact_Z.SCHOOL ~ Fact_Z.GPA +Fact_Z.AGE + Fact_Z.SUM_INTRINSIC, 
                   data=Dataset, trace=FALSE)
summary(MLM.18, cor=FALSE, Wald=TRUE)
OrdRegModel.19 <- polr(Fact_Z.SCHOOL ~ Fact_Z.SUM_INTRINSIC + Fact_Z.GPA + Fact_Z.AGE, 
                       method="probit", data=Dataset, Hess=TRUE)
summary(OrdRegModel.19)
LinearModel.20 <- lm(Fact_Z.SCHOOL ~ Fact_Z.GPA + Fact_Z.AGE + Fact_Z.SUM_INTRINSIC, 
                     data=Dataset)
summary(LinearModel.20)
LinearModel.21 <- lm(stdSCHOOLSAT ~ GPAExact + AGE + SUM_INTRINSIC, data=Dataset)
summary(LinearModel.21)
LinearModel.22 <- lm(stdSCHOOLSAT ~ GPAExact +  SUM_INTRINSIC  + RecAGE, data=Dataset)
summary(LinearModel.22)
{AnovaModel.23 <- (lm(A ~ Fact_Z.AGE*Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
} {AnovaModel.23 <- (lm(AGE ~ Fact_Z.AGE*Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
} {AnovaModel.23 <- (lm(SCHOOL ~ Fact_Z.AGE*Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
}
Anova(AnovaModel.23)
{tapply(Dataset$A, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
} {tapply(Dataset$AGE, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA
                            , Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
} {tapply(Dataset$SCHOOL, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.
                               GPA, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
}
{tapply(Dataset$A, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
} {tapply(Dataset$AGE, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA
                            , Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
} {tapply(Dataset$SCHOOL, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.
                               GPA, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
}
{tapply(Dataset$A, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
} {tapply(Dataset$AGE, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.GPA
                            , Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
} {tapply(Dataset$SCHOOL, list(Fact_Z.AGE=Dataset$Fact_Z.AGE, Fact_Z.GPA=Dataset$Fact_Z.
                               GPA, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
}
LinearModel.24 <- lm(SCHOOL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.24)
LinearModel.25 <- lm(Fact_Z.SCHOOL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.25)
LinearModel.26 <- lm(Fact_Z.SCHOOL ~ Fact_Z.GPA + Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.26)
OrdRegModel.27 <- polr(Fact_Z.SCHOOL ~ Fact_Z.GPA + Fact_Z.SUM_INTRINSIC + Fact_Z.AGE, 
                       method="probit", data=Dataset, Hess=TRUE)
summary(OrdRegModel.27)
View(Dataset)
LinearModel.28 <- lm(Fact_Z.SCHOOL ~ Fact_Z.GPA + Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.28)
LinearModel.29 <- lm(Fact_Z.SCHOOL ~ Fact_Z.SUM_INTRINSIC + Fact_Z.GPA, data=Dataset)
summary(LinearModel.29)
AnovaModel.30 <- (lm(SCHOOL ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.30)
tapply(Dataset$SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                            Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                            Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                            Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.32 <- (lm(stdAUTOTELIC ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.32)
tapply(Dataset$stdAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$stdAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$stdAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.33 <- (lm(Z.varCHALLNGSKILL ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.33)
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.34 <- (lm(Z.varMergeACTAWRE ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.34)
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
LinearModel.35 <- lm(Z.varCHALLNGSKILL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, 
                     data=Dataset)
summary(LinearModel.35)
AnovaModel.36 <- (lm(varMergeACTAWRE ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.36)
tapply(Dataset$varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$varMergeACTAWRE, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.37 <- (lm(Z.varCONCENT ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.37)
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.38 <- (lm(Z.varPRDXCONTROL ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.38)
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.39 <- (lm(Z.varLossofSELF ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.39)
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
save("Dataset", file="/home/ubuntu/Documents/June27-2014Dataset.RData")
LinearModel.40 <- lm(Z.varLossofSELF ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.40)
LinearModel.41 <- lm(Z.varCHALLNGSKILL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, 
                     data=Dataset)
summary(LinearModel.41)
LinearModel.42 <- lm(Z.varCONCENT ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.42)
RegModel.43 <- lm(Z.varAUTOTELIC~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.43)
RegModel.44 <- lm(Z.varCHALLNGSKILL~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.44)
RegModel.45 <- lm(Z.varCONCENT~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.45)
RegModel.46 <- lm(Z.varLossofSELF~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.46)
RegModel.47 <- lm(Z.varMergeACTAWRE~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.47)
RegModel.48 <- lm(Z.varPRDXCONTROL~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.48)
RegModel.49 <- lm(Z.varTRANSFORMTIME~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.49)
RegModel.50 <- lm(Z.SCHOOL~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.50)
RegModel.51 <- lm(stdClrGOALSFDBK~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.51)
cor.test(Dataset$AGE, Dataset$Z.SUM_INTRINSIC, alternative="two.sided", 
         method="kendall")
RegModel.52 <- lm(Z.SCHOOL~Z.GPA+Z.SUM_INTRINSIC, data=Dataset)
summary(RegModel.52)
.PC <- 
  princomp(~Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
View(Dataset)
.Z <- scale(Dataset[,c("ClrGOALSFDBK")])
Dataset$Z.ClrGOALSFDBK <- .Z[,1]
remove(.Z)
LinearModel.53 <- lm(Z.ClrGOALSFDBK ~ Z.GPA + Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.53)
.PC <- 
  princomp(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
LinearModel.54 <- lm(SCHOOL ~ +SKILLS +CHALLEN, data=Dataset)
summary(LinearModel.54)
.PC <- 
  princomp(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
Dataset$PC1 <- .PC$scores[,1]
Dataset$PC2 <- .PC$scores[,2]
Dataset$PC3 <- .PC$scores[,3]
remove(.PC)
View(Dataset)
# Spearman rank-order correlations
cor(Dataset[,c("PC1","PC2","PC3","Z.ClrGOALSFDBK","Z.GPA","Z.SCHOOL","Z.SUM_INTRINSIC",
               "Z.SUM_SATISFACTION","Z.varAUTOTELIC","Z.varCHALLNGSKILL","Z.varCONCENT",
               "Z.varLossofSELF","Z.varMergeACTAWRE","Z.varPRDXCONTROL","Z.varTRANSFORMTIME")], 
    use="complete.obs", method="spearman")
HClust.1 <- hclust(dist(model.matrix(~-1 + 
                                       Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
                                     Dataset)) , method= "ward")
plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab= "Observation 
     Number in Data Set Dataset", sub="Method=ward; Distance=euclidian")
.cluster <- KMeans(model.matrix(~-1 + Z.ClrGOALSFDBK + Z.varAUTOTELIC + 
                                  Z.varCHALLNGSKILL + Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + 
                                  Z.varPRDXCONTROL + Z.varTRANSFORMTIME, Dataset), centers = 3, iter.max = 10, num.seeds 
                   = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ClrGOALSFDBK + Z.varAUTOTELIC + Z.varCHALLNGSKILL 
                             + Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + 
                               Z.varTRANSFORMTIME, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
.cluster <- KMeans(model.matrix(~-1 + Z.ClrGOALSFDBK + Z.varAUTOTELIC + 
                                  Z.varCHALLNGSKILL + Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + 
                                  Z.varPRDXCONTROL + Z.varTRANSFORMTIME, Dataset), centers = 2, iter.max = 10, num.seeds 
                   = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ClrGOALSFDBK + Z.varAUTOTELIC + Z.varCHALLNGSKILL 
                             + Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + 
                               Z.varTRANSFORMTIME, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
LinearModel.55 <- lm(Z.SCHOOL ~ Z.ClrGOALSFDBK + Z.varAUTOTELIC + Z.varCHALLNGSKILL + 
                       Z.varCONCENT + Z.varLossofSELF %in%Z.varTRANSFORMTIME + Z.varPRDXCONTROL + 
                       Z.varMergeACTAWRE, data=Dataset)
summary(LinearModel.55)
LinearModel.56 <- lm(Z.SCHOOL ~ Z.ClrGOALSFDBK + Z.varAUTOTELIC + Z.varCHALLNGSKILL + 
                       Z.varCONCENT + Z.varLossofSELF + Z.varTRANSFORMTIME + Z.varPRDXCONTROL + 
                       Z.varMergeACTAWRE, data=Dataset)
summary(LinearModel.56)
LinearModel.57 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS, data=Dataset)
summary(LinearModel.57)
LinearModel.58 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS + Z.GPA, data=Dataset)
summary(LinearModel.58)
LinearModel.59 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS + Z.GPA, data=Dataset)
summary(LinearModel.59)
.Z <- scale(Dataset[,c("GPAExact")])
Dataset$Z.GPAExact <- .Z[,1]
remove(.Z)
LinearModel.60 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS +Z.GPAExact, data=Dataset)
summary(LinearModel.60)
LinearModel.61 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS + Z.GPAExact + Z.SUM_INTRINSIC, 
                     data=Dataset)
summary(LinearModel.61)
LinearModel.62 <- lm(Z.varTRANSFORMTIME ~ Z.ACTION + Z.AWARENESS + Z.GPAExact + 
                       Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.62)
LinearModel.63 <- lm(Z.varPRDXCONTROL ~ Z.ACTION + Z.AWARENESS + Z.GPAExact + 
                       Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.63)
AnovaModel.64 <- (lm(Z.SCHOOL ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.64)
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
LinearModel.65 <- lm(Z.SCHOOL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.65)
LinearModel.66 <- lm(Z.SCHOOL ~ Z.GPAExact + Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.66)
# Spearman rank-order correlations
cor(Dataset[,c("PC1","PC2","PC3","Z.ACTION","Z.AWARENESS","Z.ClrGOALSFDBK","Z.GPA",
               "Z.GPAExact","Z.SCHOOL","Z.SUM_INTRINSIC","Z.SUM_SATISFACTION","Z.varAUTOTELIC",
               "Z.varCHALLNGSKILL","Z.varCONCENT","Z.varLossofSELF","Z.varMergeACTAWRE",
               "Z.varPRDXCONTROL","Z.varTRANSFORMTIME")], use="complete.obs", method="spearman")
.PC <- 
  princomp(~Z.ACTION+Z.AGE+Z.AWARENESS+Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
LinearModel.67 <- lm(Z.SCHOOL ~ Z.GPAExact + Z.SUM_INTRINSIC + Z.varTRANSFORMTIME + 
                       Z.varPRDXCONTROL + Z.varMergeACTAWRE + Z.varLossofSELF + Z.varCONCENT + 
                       Z.varCHALLNGSKILL + Z.varAUTOTELIC + Z.ClrGOALSFDBK, data=Dataset)
summary(LinearModel.67)
LinearModel.68 <- lm(Z.SCHOOL ~ Z.GPAExact + Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.68)
LinearModel.69 <- lm(GPAExact ~ Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.69)
LinearModel.70 <- lm(GPAExact ~ Z.ACTION + Z.AWARENESS, data=Dataset)
summary(LinearModel.70)
.cluster <- KMeans(model.matrix(~-1 + Z.ACTION + Z.AGE + Z.AWARENESS + Z.ClrGOALSFDBK + 
                                  Z.GPAExact + Z.SCHOOL + Z.SUM_INTRINSIC + Z.varAUTOTELIC + Z.varCHALLNGSKILL + 
                                  Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + 
                                  Z.varTRANSFORMTIME, Dataset), centers = 4, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ACTION + Z.AGE + Z.AWARENESS + Z.ClrGOALSFDBK + 
                               Z.GPAExact + Z.SCHOOL + Z.SUM_INTRINSIC + Z.varAUTOTELIC + Z.varCHALLNGSKILL + 
                               Z.varCONCENT + Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + 
                               Z.varTRANSFORMTIME, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
scatterplot(Z.GPA~GPAExact, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots='xy', 
            span=0.5, data=Dataset)
scatterplot(Z.SUM_INTRINSIC~Z.GPAExact, reg.line=lm, smooth=TRUE, spread=TRUE, 
            boxplots='xy', span=0.5, data=Dataset)
.PC <- 
  princomp(~Z.ACTION+Z.AGE+Z.AWARENESS+Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
library(lattice, pos=4)
stepwise(LinearModel.70, direction='backward/forward', criterion='BIC')
LinearModel.71 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.71)
LinearModel.72 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC +Z.GPAExact, data=Dataset)
summary(LinearModel.72)
LinearModel.73 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact +Z.AGE, data=Dataset)
summary(LinearModel.73)
LinearModel.74 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact +Z.ACTION, data=Dataset)
summary(LinearModel.74)
LinearModel.75 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact + Z.ACTION +Z.AWARENESS, 
                     data=Dataset)
summary(LinearModel.75)
LinearModel.76 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact +, data=Dataset)
summary(LinearModel.76)
LinearModel.77 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact, data=Dataset)
summary(LinearModel.77)
LinearModel.78 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS, data=Dataset)
summary(LinearModel.78)
LinearModel.79 <- lm(Z.SCHOOL ~ Z.ACTION + Z.AWARENESS + Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.79)
LinearModel.80 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.80)
.PC <- princomp(~Z.ACTION+Z.AWARENESS+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC, cor=TRUE, 
                data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
GLM.81 <- glm(Z.SCHOOL ~ Z.GPAExact + Z.SUM_INTRINSIC, family=binomial(logit), 
              data=Dataset)
summary(GLM.81)
LinearModel.82 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact, data=Dataset)
summary(LinearModel.82)
LinearModel.83 <- lm(Z.SCHOOL ~ Z.SUM_INTRINSIC + Z.GPAExact 
                     +Z.SUM_INTRINSIC:Z.GPAExact, data=Dataset)
summary(LinearModel.83)
library(sem, pos=4)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
AnovaModel.84 <- (lm(Z.SCHOOL ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.84)
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.SCHOOL, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                              Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
.PC <- princomp(~Z.ACTION+Z.AWARENESS+Z.GPAExact+Z.SCHOOL, cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- princomp(~Z.ACTION+Z.AWARENESS+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC, cor=TRUE, 
                data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- princomp(~Z.ACTION+Z.AGE+Z.AWARENESS+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC, 
                cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- princomp(~Z.ACTION+Z.AWARENESS+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC, cor=TRUE, 
                data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
dev.print(png, filename="/home/ubuntu/Documents/RGraph-ScreePlot-9Components.png", 
          width=500, height=500)
.PC <- princomp(~Z.ACTION+Z.AWARENESS+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC, cor=TRUE, 
                data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
dev.print(png, filename="/home/ubuntu/Documents/RGraph-ScreePlot-5Components-.png", 
          width=500, height=500)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.SCHOOL+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ClrGOALSFDBK+Z.GPAExact+Z.SCHOOL+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ACAH+ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+HON+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL+Z.GPAExact+Z.SCHOOL,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ACAH+ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+HON+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL+Z.GPAExact+Z.SCHOOL,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ACAH+ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+HON+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ACAH+ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+HON+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL+Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.cluster <- KMeans(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset), centers = 3, iter.max 
                   = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset)), xlabs = 
         as.character(.cluster$cluster))
remove(.cluster)
.cluster <- KMeans(model.matrix(~-1 + PC1 + PC2 + PC3 + Z.ClrGOALSFDBK + Z.GPAExact + 
                                  Z.SCHOOL + Z.SUM_INTRINSIC + Z.varAUTOTELIC + Z.varCHALLNGSKILL + Z.varCONCENT + 
                                  Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + Z.varTRANSFORMTIME, Dataset), 
                   centers = 3, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + PC1 + PC2 + PC3 + Z.ClrGOALSFDBK + Z.GPAExact + 
                               Z.SCHOOL + Z.SUM_INTRINSIC + Z.varAUTOTELIC + Z.varCHALLNGSKILL + Z.varCONCENT + 
                               Z.varLossofSELF + Z.varMergeACTAWRE + Z.varPRDXCONTROL + Z.varTRANSFORMTIME, Dataset)), 
       xlabs = as.character(.cluster$cluster))
remove(.cluster)
.cluster <- KMeans(model.matrix(~-1 + Z.ACTION + Z.AWARENESS + Z.GPAExact + Z.SCHOOL, 
                                Dataset), centers = 2, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ACTION + Z.AWARENESS + Z.GPAExact + Z.SCHOOL, 
                             Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
.cluster <- KMeans(model.matrix(~-1 + Z.ACTION + Z.AWARENESS + Z.GPAExact + Z.SCHOOL + 
                                  Z.SUM_INTRINSIC, Dataset), centers = 2, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ACTION + Z.AWARENESS + Z.GPAExact + Z.SCHOOL + 
                               Z.SUM_INTRINSIC, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
.PC <- 
  princomp(~Z.GPAExact+Z.SCHOOL+Z.SUM_INTRINSIC+Z.varAUTOTELIC+Z.varCHALLNGSKILL+Z.varCONCENT+Z.varLossofSELF+Z.varMergeACTAWRE+Z.varPRDXCONTROL+Z.varTRANSFORMTIME,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
LinearModel.85 <- lm(Z.varTRANSFORMTIME ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, 
                     data=Dataset)
summary(LinearModel.85)
LinearModel.86 <- lm(Z.varPRDXCONTROL ~ Fact_Z.GPA * Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.86)
LinearModel.87 <- lm(Z.varPRDXCONTROL ~ Fact_Z.GPA + Fact_Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.87)
LinearModel.88 <- lm(Z.varPRDXCONTROL ~ Z.GPAExact + Z.SUM_INTRINSIC 
                     +Z.SUM_INTRINSIC:Z.GPAExact, data=Dataset)
summary(LinearModel.88)
LinearModel.89 <- lm(Z.varPRDXCONTROL ~ Z.GPAExact + Z.SUM_INTRINSIC, data=Dataset)
summary(LinearModel.89)
AnovaModel.90 <- (lm(Z.varAUTOTELIC ~ Fact_Z.GPA*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.90)
tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPA=Dataset$Fact_Z.GPA, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
Dataset$Fact_Z.GPAExact <- recode(Dataset$Z.GPAExact, '-5:0=0; 0:5=1; ; ; ;', 
                                  as.factor.result=TRUE)
save("Dataset", file="/home/ubuntu/Documents/Dataset-131COLUMNS-August192014.RData")
AnovaModel.91 <- (lm(Z.varCHALLNGSKILL ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.91)
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
{AnovaModel.92 <- (lm(A ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, data=Dataset))
} {AnovaModel.92 <- (lm(Z.varAUTOTELIC ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, data=Dataset))
}
Anova(AnovaModel.92)
{tapply(Dataset$A, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
} {tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPAExact=Dataset$Fact
                                       _Z.GPAExact, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
}
{tapply(Dataset$A, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
} {tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPAExact=Dataset$Fact
                                       _Z.GPAExact, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
}
{tapply(Dataset$A, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
} {tapply(Dataset$Z.varAUTOTELIC, list(Fact_Z.GPAExact=Dataset$Fact
                                       _Z.GPAExact, Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
}
AnovaModel.93 <- (lm(Z.varCHALLNGSKILL ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.93)
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varCHALLNGSKILL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.94 <- (lm(Z.ClrGOALSFDBK ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.94)
tapply(Dataset$Z.ClrGOALSFDBK, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.ClrGOALSFDBK, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.ClrGOALSFDBK, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                    Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.95 <- (lm(Z.varCONCENT ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, data=Dataset))
Anova(AnovaModel.95)
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varCONCENT, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                  Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.96 <- (lm(Z.varLossofSELF ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.96)
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varLossofSELF, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                     Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.97 <- (lm(Z.varMergeACTAWRE ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.97)
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varMergeACTAWRE, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                       Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.98 <- (lm(Z.varPRDXCONTROL ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.98)
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varPRDXCONTROL, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                      Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
AnovaModel.99 <- (lm(Z.varTRANSFORMTIME ~ Fact_Z.GPAExact*Fact_Z.SUM_INTRINSIC, 
                     data=Dataset))
Anova(AnovaModel.99)
tapply(Dataset$Z.varTRANSFORMTIME, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), mean, na.rm=TRUE) # means
tapply(Dataset$Z.varTRANSFORMTIME, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), sd, na.rm=TRUE) # std. deviations
tapply(Dataset$Z.varTRANSFORMTIME, list(Fact_Z.GPAExact=Dataset$Fact_Z.GPAExact, 
                                        Fact_Z.SUM_INTRINSIC=Dataset$Fact_Z.SUM_INTRINSIC), function(x) sum(!is.na(x))) # counts
View(Dataset)
.Z <- scale(Dataset[,c("ACAH","ASSIGN","ATTEND","BOOKS","CHALLEN","CLASS","CONT",
                       "CREATE","DISTRACT","DROP","DUE","ELSE","ESCAPE","FOLLOW","FUN","HON","INVOLVE",
                       "MENTAL","MISTAKE","NEW","OFFICE","OPPORT","PART","PHONE","PRESSURE","RELATION",
                       "REVIEW","SELF","SKILLS","STAND","STUDY","SUCCESS","TRAVEL","YEAR")])
Dataset$Z.ACAH <- .Z[,1]
Dataset$Z.ASSIGN <- .Z[,2]
Dataset$Z.ATTEND <- .Z[,3]
Dataset$Z.BOOKS <- .Z[,4]
Dataset$Z.CHALLEN <- .Z[,5]
Dataset$Z.CLASS <- .Z[,6]
Dataset$Z.CONT <- .Z[,7]
Dataset$Z.CREATE <- .Z[,8]
Dataset$Z.DISTRACT <- .Z[,9]
Dataset$Z.DROP <- .Z[,10]
Dataset$Z.DUE <- .Z[,11]
Dataset$Z.ELSE <- .Z[,12]
Dataset$Z.ESCAPE <- .Z[,13]
Dataset$Z.FOLLOW <- .Z[,14]
Dataset$Z.FUN <- .Z[,15]
Dataset$Z.HON <- .Z[,16]
Dataset$Z.INVOLVE <- .Z[,17]
Dataset$Z.MENTAL <- .Z[,18]
Dataset$Z.MISTAKE <- .Z[,19]
Dataset$Z.NEW <- .Z[,20]
Dataset$Z.OFFICE <- .Z[,21]
Dataset$Z.OPPORT <- .Z[,22]
Dataset$Z.PART <- .Z[,23]
Dataset$Z.PHONE <- .Z[,24]
Dataset$Z.PRESSURE <- .Z[,25]
Dataset$Z.RELATION <- .Z[,26]
Dataset$Z.REVIEW <- .Z[,27]
Dataset$Z.SELF <- .Z[,28]
Dataset$Z.SKILLS <- .Z[,29]
Dataset$Z.STAND <- .Z[,30]
Dataset$Z.STUDY <- .Z[,31]
Dataset$Z.SUCCESS <- .Z[,32]
Dataset$Z.TRAVEL <- .Z[,33]
Dataset$Z.YEAR <- .Z[,34]
remove(.Z)
save("Dataset", file="/home/ubuntu/Documents/Dataset-131COLUMNS-August192014.RData")
.PC <- 
  princomp(~ACAH+Z.ACAH+Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DROP+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SCHOOL+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~ACAH+Z.ACAH+Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DROP+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ACAH+Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DROP+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.cluster <- KMeans(model.matrix(~-1 + Z.ACAH + Z.ASSIGN + Z.ATTEND + Z.BOOKS + 
                                  Z.CHALLEN + Z.CLASS + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DROP + Z.DUE + Z.ELSE + 
                                  Z.ESCAPE + Z.FOLLOW + Z.FUN + Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + 
                                  Z.OPPORT + Z.PART + Z.PHONE + Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + 
                                  Z.STAND + Z.STUDY + Z.SUCCESS + Z.TRAVEL + Z.YEAR, Dataset), centers = 3, iter.max = 10,
                   num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ACAH + Z.ASSIGN + Z.ATTEND + Z.BOOKS + Z.CHALLEN + 
                               Z.CLASS + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DROP + Z.DUE + Z.ELSE + Z.ESCAPE + 
                               Z.FOLLOW + Z.FUN + Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + Z.OPPORT + 
                               Z.PART + Z.PHONE + Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + Z.STAND + 
                               Z.STUDY + Z.SUCCESS + Z.TRAVEL + Z.YEAR, Dataset)), xlabs = 
         as.character(.cluster$cluster))
remove(.cluster)
.cluster <- KMeans(model.matrix(~-1 + Z.ACAH + Z.ASSIGN + Z.ATTEND + Z.BOOKS + 
                                  Z.CHALLEN + Z.CLASS + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DROP + Z.DUE + Z.ELSE + 
                                  Z.ESCAPE + Z.FOLLOW + Z.FUN + Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + 
                                  Z.OPPORT + Z.PART + Z.PHONE + Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + 
                                  Z.STAND + Z.STUDY + Z.SUCCESS + Z.TRAVEL + Z.YEAR, Dataset), centers = 2, iter.max = 10,
                   num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ACAH + Z.ASSIGN + Z.ATTEND + Z.BOOKS + Z.CHALLEN + 
                               Z.CLASS + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DROP + Z.DUE + Z.ELSE + Z.ESCAPE + 
                               Z.FOLLOW + Z.FUN + Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + Z.OPPORT + 
                               Z.PART + Z.PHONE + Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + Z.STAND + 
                               Z.STUDY + Z.SUCCESS + Z.TRAVEL + Z.YEAR, Dataset)), xlabs = 
         as.character(.cluster$cluster))
remove(.cluster)
save("Dataset", file="/home/ubuntu/Documents/Dataset-131COLUMNS-August202014.RData")
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.cluster <- KMeans(model.matrix(~-1 + Z.ASSIGN + Z.ATTEND + Z.BOOKS + Z.CHALLEN + 
                                  Z.CLASS + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DUE + Z.ELSE + Z.ESCAPE + Z.FOLLOW + Z.FUN 
                                + Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + Z.OPPORT + Z.PART + Z.PHONE + 
                                  Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + Z.STAND + Z.STUDY + Z.SUCCESS 
                                + Z.TRAVEL, Dataset), centers = 3, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + Z.ASSIGN + Z.ATTEND + Z.BOOKS + Z.CHALLEN + Z.CLASS 
                             + Z.CONT + Z.CREATE + Z.DISTRACT + Z.DUE + Z.ELSE + Z.ESCAPE + Z.FOLLOW + Z.FUN + 
                               Z.INVOLVE + Z.MENTAL + Z.MISTAKE + Z.NEW + Z.OFFICE + Z.OPPORT + Z.PART + Z.PHONE + 
                               Z.PRESSURE + Z.RELATION + Z.REVIEW + Z.SELF + Z.SKILLS + Z.STAND + Z.STUDY + Z.SUCCESS 
                             + Z.TRAVEL, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
.FA <- 
  factanal(~ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~ASSIGN+ATTEND+BOOKS+CHALLEN+CLASS+CONT+CREATE+DISTRACT+DROP+DUE+ELSE+ESCAPE+FOLLOW+FUN+INVOLVE+MENTAL+MISTAKE+NEW+OFFICE+OPPORT+PART+PHONE+PRESSURE+RELATION+REVIEW+SELF+SKILLS+STAND+STUDY+SUCCESS+TRAVEL,
           factors=4, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
reliability(cov(Dataset[,c("ASSIGN","ATTEND","BOOKS","CHALLEN","CLASS","CONT","CREATE",
                           "DISTRACT","DROP","DUE","ELSE","ESCAPE","FOLLOW","FUN","INVOLVE","MENTAL","MISTAKE",
                           "NEW","OFFICE","OPPORT","PART","PHONE","PRESSURE","RELATION","REVIEW","SELF","SKILLS",
                           "STAND","STUDY","SUCCESS","TRAVEL")], use="complete.obs"))
reliability(cov(Dataset[,c("ASSIGN","ATTEND","BOOKS","CHALLEN","CLASS","CONT","CREATE",
                           "DISTRACT","DROP","DUE","ELSE","ESCAPE","FOLLOW","FUN","INVOLVE","MENTAL","MISTAKE",
                           "NEW","OFFICE","OPPORT","PART","PHONE","PRESSURE","RELATION","REVIEW","SELF","SKILLS",
                           "STAND","STUDY","SUCCESS","TRAVEL","Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CHALLEN",
                           "Z.CLASS","Z.CONT","Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW",
                           "Z.FUN","Z.INVOLVE","Z.MENTAL","Z.MISTAKE","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE",
                           "Z.PRESSURE","Z.RELATION","Z.REVIEW","Z.SELF","Z.SKILLS","Z.STAND","Z.STUDY",
                           "Z.SUCCESS","Z.TRAVEL")], use="complete.obs"))
reliability(cov(Dataset[,c("Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CHALLEN","Z.CLASS",
                           "Z.CONT","Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW","Z.FUN",
                           "Z.INVOLVE","Z.MENTAL","Z.MISTAKE","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE",
                           "Z.PRESSURE","Z.RELATION","Z.REVIEW","Z.SELF","Z.SKILLS","Z.STAND","Z.STUDY",
                           "Z.SUCCESS","Z.TRAVEL")], use="complete.obs"))
reliability(cov(Dataset[,c("Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CLASS","Z.CONT",
                           "Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW","Z.FUN","Z.INVOLVE",
                           "Z.MENTAL","Z.MISTAKE","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE","Z.PRESSURE",
                           "Z.RELATION","Z.REVIEW","Z.SELF","Z.STAND","Z.STUDY","Z.SUCCESS","Z.TRAVEL")], 
                use="complete.obs"))
reliability(cov(Dataset[,c("Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CLASS","Z.CONT",
                           "Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW","Z.FUN","Z.INVOLVE",
                           "Z.MENTAL","Z.MISTAKE","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE","Z.PRESSURE",
                           "Z.RELATION","Z.REVIEW","Z.SKILLS","Z.STAND","Z.STUDY","Z.SUCCESS","Z.TRAVEL")], 
                use="complete.obs"))
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
summary(AnovaModel.99, cor=FALSE)
Confint(AnovaModel.99, level=0.95)
stepwise(AnovaModel.99, direction='backward/forward', criterion='BIC')
xyplot(Z.SCHOOL ~ Z.GPAExact + Z.SUM_INTRINSIC, pch=16,
       auto.key=list(border=TRUE), 
       par.settings = simpleTheme(pch=16), scales=list(x=list(relation='same'), 
                                                       y=list(relation='same')),
       data=Dataset)
.Table <- xtabs(~Fact_Z.SUM_INTRINSIC+Fact_Z.SCHOOL, data=Dataset)
.Table
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
.Table <- xtabs(~Fact_Z.GPAExact+Fact_Z.SCHOOL, data=Dataset)
.Table
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
.Table <- xtabs(~Fact_Z.varPRDXCONTROL+Fact_Z.SCHOOL, data=Dataset)
.Table
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
.Table <- xtabs(~Fact_Z.AGE+Fact_Z.SUM_INTRINSIC, data=Dataset)
.Table
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
Hist(Dataset$Z.CONT, scale="frequency", breaks="Sturges", col="darkgray")
Hist(Dataset$Z.CONT, scale="frequency", breaks="Sturges", col="darkgray")
cor.test(Dataset$Z.CONT, Dataset$Z.SUM_INTRINSIC, alternative="greater", 
         method="kendall")
cor.test(Dataset$Z.SUM_INTRINSIC, Dataset$Z.varPRDXCONTROL, alternative="greater", 
         method="kendall")
reliability(cov(Dataset[,c("Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CLASS","Z.CONT",
                           "Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW","Z.FUN","Z.INVOLVE",
                           "Z.MENTAL","Z.MISTAKE","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE","Z.PRESSURE",
                           "Z.RELATION","Z.REVIEW","Z.SKILLS","Z.STAND","Z.STUDY","Z.SUCCESS","Z.TRAVEL")], 
                use="complete.obs"))

library(psych)
reliability(polychoric(Dataset[,c("Z.ASSIGN","Z.ATTEND","Z.BOOKS","Z.CHALLEN","Z.CLASS",
                                  "Z.CONT","Z.CREATE","Z.DISTRACT","Z.DUE","Z.ELSE","Z.ESCAPE","Z.FOLLOW","Z.FUN",
                                  "Z.INVOLVE","Z.MENTAL","Z.MISTAKE","Z.NEW","Z.OFFICE","Z.OPPORT","Z.PART","Z.PHONE",
                                  "Z.PRESSURE","Z.RELATION","Z.REVIEW","Z.SELF","Z.SKILLS","Z.STAND","Z.STUDY",
                                  "Z.SUCCESS","Z.TRAVEL")], use="complete.obs", method=c("pearson")))


save("Dataset", 
     file="/home/ubuntu/Documents/DatasetAUGUST24-2014TransformedScoresIncluded.RData")
load("/home/ubuntu/Documents/DatasetAUGUST24-2014TransformedScoresIncluded.RData")
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
remove(.PC)
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
remove(.PC)
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.PC <- 
  princomp(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
View(Dataset)
.cluster <-  KMeans(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset), centers = 2, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
.cluster <-  KMeans(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset), centers = 3, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
HClust.1 <- hclust(dist(model.matrix(~-1 + PC1+PC2+PC3, Dataset)) , method= "ward")
plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab= "Observation Number in Data Set Dataset", sub="Method=ward; Distance=euclidian")
.cluster <-  KMeans(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset), centers = 3, iter.max = 10, num.seeds = 10)
.cluster$size # Cluster Sizes
.cluster$centers # Cluster Centroids
.cluster$withinss # Within Cluster Sum of Squares
.cluster$tot.withinss # Total Within Sum of Squares
.cluster$betweenss # Between Cluster Sum of Squares
biplot(princomp(model.matrix(~-1 + PC1 + PC2 + PC3, Dataset)), xlabs = as.character(.cluster$cluster))
remove(.cluster)
HClust.2 <- hclust(dist(model.matrix(~-1 + PC1+PC2+PC3, Dataset)) , method= "ward")
plot(HClust.2, main= "Cluster Dendrogram for Solution HClust.2", xlab= "Observation Number in Data Set Dataset", sub="Method=ward; Distance=euclidian")
dev.print(png, filename="/home/ubuntu/Documents/RGraph-ClusterDendrogramActionAwarenessItems.png", width=7, height=7, pointsize=12, units="in", res=72)
library(sem, pos=4)
.model <- c('AutotelicExperience: ASSIGN, Z.NEW')
.model <- cfa(file=textConnection(.model), reference.indicators=FALSE)
.Data <- Dataset[, c('ASSIGN', 'Z.NEW')]
summary(sem(.model, data=.Data), robust=FALSE)
remove('.model', '.Data')
.model <- c('ChallengeSkill: Z.BOOKS, Z.CHALLEN, Z.CREATE, Z.OPPORT, Z.SKILLS', 'MergingActionAwareness: Z.ATTEND, Z.CLASS, Z.INVOLVE, Z.PART, Z.REVIEW', 
            'ClearGoalsFeedback: Z.DUE, Z.OFFICE, Z.RELATION, Z.STAND, Z.SUCCESS', 'Concentration: Z.MENTAL, Z.STUDY', 'Control: Z.CONT, Z.DISTRACT, Z.PHONE', 
            'LossOfSelfConsciousness: Z.ELSE, Z.ESCAPE, Z.FUN, Z.MISTAKE, Z.SELF', 'AutotelicExperience: Z.ASSIGN, Z.NEW')
.model <- cfa(file=textConnection(.model), reference.indicators=FALSE)
.Data <- Dataset[, c('Z.BOOKS', 'Z.CHALLEN', 'Z.CREATE', 'Z.OPPORT', 'Z.SKILLS', 'Z.ATTEND', 'Z.CLASS', 'Z.INVOLVE', 'Z.PART', 'Z.REVIEW', 'Z.DUE', 'Z.OFFICE', 
                     'Z.RELATION', 'Z.STAND', 'Z.SUCCESS', 'Z.MENTAL', 'Z.STUDY', 'Z.CONT', 'Z.DISTRACT', 'Z.PHONE', 'Z.ELSE', 'Z.ESCAPE', 'Z.FUN', 'Z.MISTAKE', 'Z.SELF', 'Z.ASSIGN', 
                     'Z.NEW')]
summary(sem(.model, data=.Data), robust=FALSE)
remove('.model', '.Data')
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.TRAVEL,
           factors=8, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=10, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=7, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=6, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=5, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=2, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=3, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
.FA <- 
  factanal(~Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL,
           factors=12, rotation="varimax", scores="none", data=Dataset)
.FA
remove(.FA)
load("/home/ubuntu/Documents/DatasetAUGUST24-2014TransformedScoresIncluded.RData")
library(abind, pos=4)
.Table <- xtabs(~Fact_Z.AWARENESS+Fact_Z.ACTION+Fact_Z.GPA, data=Dataset)
.Table
remove(.Table)
.Table <- xtabs(~Fact_Z.AWARENESS+Fact_Z.ACTION+Fact_Z.SUM_INTRINSIC, data=Dataset)
.Table
remove(.Table)
.Table <- xtabs(~Fact_Z.AWARENESS+Fact_Z.ACTION+Fact_Z.AGE, data=Dataset)
.Table
remove(.Table)
.PC <- 
  princomp(~Z.ACAH+Z.AGE+Z.ASSIGN+Z.ATTEND+Z.BOOKS+Z.CHALLEN+Z.CLASS+Z.CONT+Z.CREATE+Z.DISTRACT+Z.DROP+Z.DUE+Z.ELSE+Z.ESCAPE+Z.FOLLOW+Z.FUN+Z.GPAExact+Z.HON+Z.INVOLVE+Z.MENTAL+Z.MISTAKE+Z.NEW+Z.OFFICE+Z.OPPORT+Z.PART+Z.PHONE+Z.PRESSURE+Z.RELATION+Z.REVIEW+Z.SCHOOL+Z.SELF+Z.SKILLS+Z.STAND+Z.STUDY+Z.SUCCESS+Z.SUM_INTRINSIC+Z.TRAVEL+Z.YEAR,
           cor=TRUE, data=Dataset)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
remove(.PC)
.Z <- scale(Dataset[,c("ALL_QUESTIONS")])
Dataset$Z.ALL_QUESTIONS <- .Z[,1]
remove(.Z)
cor.test(Dataset$Z.ASSIGN, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ACAH, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ACTION, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.AGE, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ASSIGN, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ATTEND, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.AWARENESS, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.BOOKS, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.CHALLEN, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.CLASS, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ClrGOALSFDBK, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.CONT, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.CREATE, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.DISTRACT, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.DROP, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.DUE, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ELSE, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.ESCAPE, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.FOLLOW, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.FUN, Dataset$Z.GPAExact, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.HON, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.INVOLVE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.MENTAL, alternative="greater", method="kendall")
cor.test(Dataset$Z.HON, Dataset$Z.MENTAL, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.HON, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.INVOLVE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.MENTAL, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.MISTAKE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.NEW, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.OFFICE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.OPPORT, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.PART, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.PHONE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.PRESSURE, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.RELATION, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.REVIEW, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.SCHOOL, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.SELF, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.SKILLS, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.STAND, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.STUDY, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.SUCCESS, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.TRAVEL, alternative="greater", method="kendall")
cor.test(Dataset$Z.GPAExact, Dataset$Z.YEAR, alternative="greater", method="kendall")
