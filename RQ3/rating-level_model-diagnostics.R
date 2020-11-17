# get the data
rating <- read.csv("~/Desktop/TEMP/DISS/rating.csv") # PCA soutions really similar to those of score-level analysis
stats_scoring_RPlat_original <- read.csv("~/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv")
rating <- merge(rating, stats_scoring_RPlat_original, by="soundname")
names(rating)
# to be consistent with the human scoring/rater effects analyses, except for reverse coding
table(rating$rater)
levels(rating$rater)[levels(rating$rater)=="Simpsone"] <- "simpsone"
rating <- subset(rating, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili" & rater != "phuongn" & rater != "gcakinci")
rating$rater <- droplevels(rating$rater)
table(rating$rater)
rating$rater_num <- as.factor(as.numeric(rating$rater))
# classify the raters: *** human score analysis should inform this *****
# by expert-novice
#rating$rater2 <- recode(rating$rater,'alhammad'=0,'apriltan'=0,'eftodey'=0,'egoodale'=0,'ilucic'=0,'kterrill'=0,'lcompton'=0,'nanda'=0,'rhani'=0,'secil'=0,'shuffman'=0,'simpsone'=0,'ykwon'=0,'ziweizh '=0,
#                        'gccorones'=1,'gcdessie'=1,'gcdouglas'=1,'gcewald'=1,'gcpeterson'=1,'gcreedy'=1,'sandyp'=1)
# by effects vs non-effects
# severity: rater19, 7
# leniency: rater 13, 2
# overfit: Rater 9, 2
# misfit: Rater 15, 12
library(tidyverse)
# recoding the effects raters: color coded in fit statistics table in IRT analysis
# tried to split half-half
rating$rater_num2 <- as.factor(recode(rating$rater_num,'19'=1,'7'=1,'13'=1,'2'=1,'9'=1,'15'=1,'12'=1,
                                      '1'=0,'3'=0,'4'=0,'5'=0,'6'=0,'8'=0,'10'=0,'11'=0,'14'=0,'16'=0,'17'=0,'18'=0,'20'=0,'21'=0))

table(rating$teach_total)
rating <- rating[rating$teach_total!=4,]
table(rating$teach_total)
names(rating)
rating[,c(9:20)] <- scale(rating[,c(9:20)], center=T,scale=T)

## 1. LM
step.mod1.1 <- lm(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3, data = rating)
library(car)
# assessing outliers
outlierTest(step.mod1.1) # assess outliers
leveragePlots(step.mod1.1) # leverage plots: some high leverage points can be observed: case 403 and 202 appeared in all plots 
rating2 <- rating[-c(403, 202),]
# ****** refit and re-evaluate the model ***

# influential obs
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(rating)-length(step.mod1.1$coefficients)-2))
plot(step.mod1.1, which=4, cook.levels=cutoff)
# Influence Plot
avPlots(step.mod1.1) # added variable plot: same as the leverage plot
influencePlot(step.mod1.1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# ** remove 13, 371, and 40 and refit the model ****

# non-normality
# Normality of Residuals
# qq plot for studentized resid
qqPlot(step.mod1.1, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(step.mod1.1)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# non-constant error variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(step.mod1.1)
# plot studentized residuals vs. fitted values
spreadLevelPlot(step.mod1.1) # *** problem?
# multicollinearity
vif(step.mod1.1) # variance inflation factors
sqrt(vif(step.mod1.1)) > 2 # no problem

# non-linearity
# Evaluate Nonlinearity
# component + residual plot
crPlots(step.mod1.1)
# Ceres plots
ceresPlots(step.mod1.1)
# non-independence of errors
# Test for Autocorrelated Errors
durbinWatsonTest(step.mod1.1)
# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(step.mod1.1)
summary(gvmodel) # issue with heteroscedastiity

# homoskedasticity: https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
library(olsrr)
ols_test_breusch_pagan(step.mod1.1)
ols_test_f(step.mod1.1)
plot(rating$fluency_PC1,resid(step.mod1.1))
plot(rating$nrep_per_token,resid(step.mod1.1)) # potential megaphone shpe
plot(rating$gcp_confidence,resid(step.mod1.1))
plot(rating$taales_PC1,resid(step.mod1.1))
# Linearity
plot(rating$fluency_PC1,step.mod1.1$fitted.values)
plot(rating$nrep_per_token,step.mod1.1$fitted.values)
plot(rating$gcp_confidence,step.mod1.1$fitted.values)
plot(rating$taales_PC1,step.mod1.1$fitted.values)

## LMM
library(lme4)
library(lmerTest)
reduce_mod2.1 <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1++taasc_PC3+(1|ratee), REML = FALSE, data = rating)
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
# *** continue with interpretations
plot(reduce_mod2.1) # ** interpretations
# linearity
ggplot(data.frame(x1=rating$date3,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=rating$nrep_per_token,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=rating$L7,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=rating$gcp_confidence,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=rating$taales_PC1,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=rating$taasc_PC3,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()

# normality of residuals
qqnorm(residuals(reduce_mod2.1)) # does not raise any significant concern with normality of the weighted residuals
shapiro.test(residuals(reduce_mod2.1))

# homogeneity of variance: https://ademos.people.uic.edu/Chapter18.html
# This procedure is a variation of “Levene’s Test”. 
# Essentially, we’ll extract the residuals from the model, take their absolute value, and then square them (for a more robust analysis with respect to issues of normality, see Glaser 2006).
# Finally we’ll take a look at the ANOVA of the between subjects residuals.
anova(lm(abs(residuals(reduce_mod2.1))^2 ~ ratee, data = rating)) # equal variance assumption met
# Sensitivity to data (case dianostics): help with generalizability *******
ggplot(data.frame(lev=hatvalues(reduce_mod2.1),pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

# independence ***
means <- aggregate(rating[,c("date3","nrep_per_token","L7", "gcp_confidence","taales_PC1","taasc_PC3")],by=list(rating$ratee),FUN=mean)
lmcoefs <- summary(lm(teach_total ~ date3 + nrep_per_token + L7 + gcp_confidence + taales_PC1 + taasc_PC3+ratee, data=rating))$coefficients[,"Estimate"]
means$effects <- c(0,lmcoefs[substr(names(lmcoefs),1,5) == "ratee"])
means$effects <- means$effects - mean(means$effects)
cor(means[,c("date3","nrep_per_token","L7","gcp_confidence","taales_PC1","taasc_PC3","effects")]) # predictors are only weakly correlated with the random effect
# only nfiller correlated moderately with random effects
fixef(reduce_mod2.1)
lmcoefs[1:7] # some differences

#The following code determines which of the observations have the highest leverage and displays these observations. 
#The code also generates a new model without these observations and then compares the coefficients for the will all observations to this new model with some observations removed.
levId <- which(hatvalues(reduce_mod2.1) >= 0.25)
summary(rating[,c("teach_total","date3","nrep_per_token","L7","gcp_confidence","taales_PC1","taasc_PC1")])

mmLev <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee), data=rating[-c(levId),])
mmLevCD <- data.frame(effect=fixef(reduce_mod2.1),
                      change=(fixef(mmLev) - fixef(reduce_mod2.1)),
                      se=sqrt(diag(vcov(reduce_mod2.1)))
)
rownames(mmLevCD) <- names(fixef(mmLev))
mmLevCD$multiples <- abs(mmLevCD$change / mmLevCD$se)
mmLevCD # some changes in fluency_PC1 and nfiller_per_token
# ICC
#ICC is a measure of how much of the variation in the response variable, which is not attributed to fixed effects, is accounted for by a random effect. 
# It is the ratio of the variance of the random effect to the total random variation. The code to calculate the ICC for the mm model is demonstrated here.
# In other words, it is a measure of the proportion of variance that is between people versus the total variance (i.e., variance between people and variance within persons).
r1Var <- as.numeric(VarCorr(reduce_mod2.1)[["ratee"]])
residVar <- attr(VarCorr(reduce_mod2.1), "sc")^2
r1Var
residVar
r1Var / (r1Var + residVar) # only 22.07918 percent of the stochastic variation is accounted for by ratee
# alternatively
library(multilevelTools)
iccMixed("teach_total", "ratee", rating) # very small between-subect variations
iccMixed("date3", "ratee", rating)
iccMixed("nrep_per_token", "ratee", rating) 
iccMixed("L7", "ratee", rating) 
iccMixed("gcp_confidence", "ratee", rating) #
iccMixed("taales_PC1", "ratee", rating)  
iccMixed("taasc_PC3", "ratee", rating) 
# vairaitons in the predictors can be attrbuted (max=46%) to between-subject variation
# variation in the teach levels is mainly attrited to within-subject variation (92%)


## 3. CLM
library(MASS)
rating$teach_total <- as.factor(rating$teach_total)
step.mod3.1 <- polr(teach_total~date3++nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, data = rating, Hess=TRUE)
# proportionl odds / parallel slope assumption *******
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
s <- with(rating, summary(as.numeric(teach_total) ~ date3++nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, fun=sf))
s
# **the assumption may be violated?
# checking the parallel slope assumption using logistic regression
glm(I(as.numeric(teach_total) >= 2) ~ date3, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ date3, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ L7, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ L7, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ nrep_per_token, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ nrep_per_token, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ gcp_confidence, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ gcp_confidence, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ lca_PC1, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ lca_PC1, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ taales_PC1, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ taales_PC1, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ taasc_PC3, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ taasc_PC3, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 2) ~ taaco_PC1, family="binomial", data = rating)
glm(I(as.numeric(teach_total) >= 3) ~ taaco_PC1, family="binomial", data = rating)
# *** how big the differneces requires fitting a differential slope model??
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
# ** the assumption may only hold for fluency_PC1
# nomial effects test
library(ordinal)
fm <- clm(teach_total~date3+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, data = rating)
nominal_test(fm) # no evidence of non-proportional odds
scale_test(fm) # some evdence of scale effects

# assess PO assumption post by Harrell: https://groups.google.com/g/medstats/c/y_94cReelQg?pli=1
library(rms)
y <- as.factor(rating$teach_total)
Y <- as.numeric(y) - 1
ncut <- length(unique(Y)) - 1
p <- 8
Coef <- matrix(NA, ncol=p, nrow=ncut,
               dimnames=list(paste('>=', levels(y)[-1],sep=''),
                             NULL))
for(k in 1:ncut) {
  f <- lrm(Y >= k ~ date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = rating)
  Coef[k,] <- coef(f)[-1]
}
colnames(Coef) <- names(coef(f))[-1]
round(Coef, 3)


