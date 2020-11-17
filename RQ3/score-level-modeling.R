# get the data
teach_levels_data5 <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/teach_levels_data5.csv")
names(teach_levels_data5)
table(teach_levels_data5$final_teach_level)
#1  2  3 
#32 65 54 
teach_levels_data5[,c(9:20)] <- scale(teach_levels_data5[,c(9:20)], center=T,scale=T)
teach_levels_data5$intercept <- rep(1,nrow(teach_levels_data5))

#################################################################################################
# 1. MLR: assuming different examinee appearing at different times and continous outcome variable
#################################################################################################
mod1 <- lm(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5)
summary(mod1) # adjusted R^2 = 0.372 
mean(mod1$residuals^2) #  0.3167771 MSE = mean((observeds - predicteds)^2)
var(mod1$residuals)
logLik(mod1) # -127.4682
AIC(mod1) #  282.9364
BIC(mod1) # 325.17834
cor(teach_levels_data5[,9:19])
# fluency_pc1 and L7, sca_PC1 are highly correlated

# include the time variable
mod1.1 <- lm(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5)
summary(mod1.1) # 0.3954  
mean(mod1.1$residuals^2) #  0.3027948
var(mod1.1$residuals)
logLik(mod1.1) # -124.0599
AIC(mod1.1) # 278.1198
BIC(mod1.1) # 323.379

# more parsimonious models
library(MASS)
step.mod1 <- stepAIC(mod1,direction = "both") 
summary(step.mod1) # adjusted R^2 = 0.3837 
mean(step.mod1$residuals^2) # 0.3221786
var(step.mod1$residuals) 
logLik(step.mod1)
AIC(step.mod1) # 275.4894
BIC(step.mod1) # 302.6449

# to be consistent, time varible is added rather than stepAIC(mod1.1), which selected other predictors
step.mod1.1 <- lm(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = teach_levels_data5)
summary(step.mod1.1) # adjusted R^2 =  0.3977  
mean(step.mod1.1$residuals^2) # 0.3126249
var(step.mod1.1$residuals)
logLik(step.mod1.1)
AIC(step.mod1.1) # 272.944
BIC(step.mod1.1) # 306.9214

# have the significant effects to interact with time
step.mod1.1.1 <- lm(final_teach_level ~ date3*fluency_PC1+date3*nrep_per_token+date3*taasc_PC1+date3*taasc_PC3+nfiller_per_token+gcp_confidence+taales_PC1, data = teach_levels_data5)
summary(step.mod1.1.1) # interaction non-significant; main effects non-significant
mean(step.mod1.1.1$residuals^2) # 0.307548
AIC(step.mod1.1.1) # 278.4717
BIC(step.mod1.1.1) # 320.7136

# model comparison
anova(mod1, mod1.1)
anova(step.mod1, step.mod1.1)
anova(step.mod1, mod1)
anova(step.mod1.1, mod1.1)

# Evaluations of mod1 based on 10-fold cv
tmp_dat = teach_levels_data5[,8:20]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <-lm(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData))
  pred1 <- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5695364
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7549669
cor.test(pred,y) # 0.4399814 
cor.test(pred1,y) # 0.5746951
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4200145
cm <- table(factor(y),  # rows
      factor(pred, levels=min(y):max(y)))  # columns: note predictions of 4 were truncated
cm
precision <- diag(cm) / colSums(cm)
precision
recall <- diag(cm) / rowSums(cm)
recall
f1 <-  2 * precision * recall / (precision + recall)
f1

# prediction on trian data
pred3 <- round(predict(mod1, newdata = teach_levels_data5))
pred4 <- predict(mod1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.609
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.794702
cor.test(pred3,y3) # 0.5454104
cor.test(pred4,y3) # 0.6498197 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.5246268

# Evaluations of mod1.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(3,8:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  
  tmp.mod <-lm(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData))
  pred1<- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5960265
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.781457
cor.test(pred,y) # 0.5009229  
cor.test(pred1,y) # 0.5815103
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4801398
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod1.1, newdata = teach_levels_data5))
pred4 <- predict(mod1.1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.6357616
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8145695
cor.test(pred3,y3) # 0.5750679 
cor.test(pred4,y3) # 0.6691535  
ScoreQuadraticWeightedKappa(pred3,y3) # 0.5524325

# Evaluations step.mod1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <-lm(final_teach_level~nfiller_per_token+nrep_per_token+fluency_PC1+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData))
  pred1 <- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.589404
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7748344
cor.test(pred,y) # 0.4947393 
cor.test(pred1,y) # 0.5941994
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4752657
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(step.mod1, newdata = teach_levels_data5))
pred4 <- predict(step.mod1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.602649
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.7880795
cor.test(pred3,y3) # 0.5355355 
cor.test(pred4,y3) # 0.6421951  
ScoreQuadraticWeightedKappa(pred3,y3) # 0.5143207

# Evaluations step.mod1.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(3,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  
  tmp.mod <-lm(final_teach_level~date3+nfiller_per_token+nrep_per_token+fluency_PC1+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData))
  pred1 <- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.615894
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.8013245
cor.test(pred,y) # 0.5510515 
cor.test(pred1,y) # 0.5948124
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.528825
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(step.mod1.1, newdata = teach_levels_data5))
pred4 <- predict(step.mod1.1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.6225166
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8211921
cor.test(pred3,y3) # 0.5750168 
cor.test(pred4,y3) # 0.6556206   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.543564

####################################################################################################################
# Model diagnostics of step.mod1.1, which is the preferred model (almost all effects significant; highest prediction accuracy)
# ****** continue with the interpretations
library(car)
# assessing outliers
outlierTest(step.mod1.1) # assess outliers
leveragePlots(step.mod1.1) # leverage plots: some high leverage points can be observed: case 65 and 121 appeared in all plots 
#When case 65 and 121 are removed and step.mod1.1 are refitted: there are some improvement – adj. R2 = 0.4373, RMSE=0.2862641, AIC = 256.4684, 286.5079, accuracy1 = 0.6174497, correlation (rounded)= 0.5669723, correlation (unrounded) = 0.6375592
# influential obs
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(teach_levels_data5)-length(step.mod1.1$coefficients)-2))
plot(step.mod1.1, which=4, cook.levels=cutoff)
# Influence Plot
avPlots(step.mod1.1) # added variable plot: same as the leverage plot
influencePlot(step.mod1.1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
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
plot(teach_levels_data5$fluency_PC1,resid(step.mod1.1))
plot(teach_levels_data5$nrep_per_token,resid(step.mod1.1)) # potential megaphone shpe
plot(teach_levels_data5$gcp_confidence,resid(step.mod1.1))
plot(teach_levels_data5$taales_PC1,resid(step.mod1.1))
# Linearity
plot(teach_levels_data5$fluency_PC1,step.mod1.1$fitted.values)
plot(teach_levels_data5$nrep_per_token,step.mod1.1$fitted.values)
plot(teach_levels_data5$gcp_confidence,step.mod1.1$fitted.values)
plot(teach_levels_data5$taales_PC1,step.mod1.1$fitted.values)

############################################################################################
# 2. LMM assuming same examinee appearing at differnet times and continous outcome variable
############################################################################################
library(lme4)
library(lmerTest)
mod2 <- lmer(final_teach_level~nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = teach_levels_data5)
summary(mod2) 
mean(residuals(mod2)^2) # 0.2127076
logLik(mod2)
AIC(mod2) # 332.7563
BIC(mod2) # 378.0155
# note lower MSE, much much higher AIC and BIC
library(ggplot2)
ggplot(data.frame(ranef(mod2)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
# looked like bimodal; mean = 2.171847e-16, var = 0.0147789
qqnorm(ranef(mod2)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2)$ratee[,1])

# include the time variable
mod2.1 <- lmer(final_teach_level~date3+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = teach_levels_data5)
summary(mod2.1) # similar to mod1.1
mean(residuals(mod2.1)^2) # 0.1655059
logLik(mod2.1)
AIC(mod2.1) # 332.7854
BIC(mod2.1) # 381.0618
library(ggplot2)
ggplot(data.frame(ranef(mod2.1)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
# looked more bimodal
qqnorm(ranef(mod2.1)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.1)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.1)$ratee[,1])

# more parsimonious models
#https://stackoverflow.com/questions/55638476/how-to-do-stepwise-model-with-random-effect-lme4-lmertest
fixmodel <- lm(formula(mod2,fixed.only=TRUE),
               data=eval(getCall(mod2)$data))
step.mod2 <- step(fixmodel)
summary(step.mod2) 
reduce_mod2 <- lmer(final_teach_level~nfiller_per_token+nrep_per_token+fluency_PC1+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = teach_levels_data5)
summary(reduce_mod2) # similar to step.mod1
mean(residuals(reduce_mod2)^2) #  0.2455307
logLik(reduce_mod2)
AIC(reduce_mod2) # 309.3072
BIC(reduce_mod2) # 339.48
# still lower RMSE, but higher AIC and BIC than step.mod1
ggplot(data.frame(ranef(reduce_mod2)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
# looked more bimodal
qqnorm(ranef(reduce_mod2)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2)$ratee[,1])

reduce_mod2.1 <- lmer(final_teach_level~date3+nfiller_per_token+nrep_per_token+fluency_PC1+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = teach_levels_data5)
summary(reduce_mod2.1) # time is significant, but gcp_confidence becomes non-significant; also similar to step.mod1.1
mean(residuals(reduce_mod2.1)^2) #   0.2068267
logLik(reduce_mod2.1)
AIC(reduce_mod2.1) # 312.3672
BIC(reduce_mod2.1) # 345.5573
ggplot(data.frame(ranef(reduce_mod2.1)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
# looked more bimodal
qqnorm(ranef(reduce_mod2.1)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.1)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.1)$ratee[,1])

# model comparison
anova(mod2, mod2.1)
anova(reduce_mod2, reduce_mod2.1)
anova(reduce_mod2, mod2)
anova(reduce_mod2.1, mod2.1)

# Evaluations of mod2 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- lmer(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData, allow.new.levels = TRUE))
  pred1 <- predict(tmp.mod, newdata = testData, allow.new.levels = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5629139
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7549669
cor.test(pred,y) # 0.4361632 
cor.test(pred1,y) # 0.575234 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4180669
cm <- table(factor(y),  # rows
            factor(pred, levels=min(y):max(y)))  # columns: note predictions of 4 were truncated
cm
precision <- diag(cm) / colSums(cm)
precision
recall <- diag(cm) / rowSums(cm)
recall
f1 <-  2 * precision * recall / (precision + recall)
f1
# prediction on trian data
pred3 <- round(predict(mod2, newdata = teach_levels_data5))
pred4 <- predict(mod2, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.6953642
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8609272
cor.test(pred3,y3) # 0.6729474 
cor.test(pred4,y3) # 0.7819123   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.6473037

# Evaluations of mod2.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- lmer(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML = FALSE, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData, allow.new.levels = TRUE))
  pred1 <- predict(tmp.mod, newdata = testData, allow.new.levels = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5960265
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.8013245
cor.test(pred,y) # 0.5173444  
cor.test(pred1,y) # 0.593333 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4904029
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod2.1, newdata = teach_levels_data5))
pred4 <- predict(mod2.1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.7549669
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8807947
cor.test(pred3,y3) # 0.7500065 
cor.test(pred4,y3) # 0.8423367   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.738913

# Evaluations of reduce_mod2 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,4,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- lmer(final_teach_level~fluency_PC1+nrep_per_token+nfiller_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData, allow.new.levels = TRUE))
  pred1 <- predict(tmp.mod, newdata = testData, allow.new.levels = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5960265
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7748344
cor.test(pred,y) # 0.4993732  
cor.test(pred1,y) # 0.5856272 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4777784
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2, newdata = teach_levels_data5))
pred4 <- predict(reduce_mod2, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) # 0.6357616
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8145695
cor.test(pred3,y3) # 0.597133 
cor.test(pred4,y3) # 0.7368431   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.5713106

# Evaluations of reduce_mod2.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,4,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- lmer(final_teach_level~date3+fluency_PC1+nrep_per_token+nfiller_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = trainData)
  pred <- round(predict(tmp.mod, newdata = testData, allow.new.levels = TRUE))
  pred1 <- predict(tmp.mod, newdata = testData, allow.new.levels = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred, pred1))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5827815
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.794702
cor.test(pred,y) # 0.5021864   
cor.test(pred1,y) # 0.5913268  
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4769053
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2.1, newdata = teach_levels_data5))
pred4 <- predict(reduce_mod2.1, newdata = teach_levels_data5)
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) #  0.6953642
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8410596
cor.test(pred3,y3) # 0.6783754 
cor.test(pred4,y3) # 0.7879254   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.6621924

#############################################################################################################################
# Model diagnostics of reduce_mod2, which is the preferred model (almost all effects significant, highest prediction accuracy)
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
# *** continue with interpretations
plot(reduce_mod2.1)
# linearity
ggplot(data.frame(x1=teach_levels_data5$nfiller_per_token,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$nrep_per_token,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$fluency_PC1,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$gcp_confidence,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$taales_PC1,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$taasc_PC1,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=teach_levels_data5$taasc_PC3,pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()

# Independence
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html#:~:text=The%20random%20variables%20of%20a,random%20variable%20groups%2C%20are%20correlated.&text=The%20assumption%20is%20relaxed%20to,by%20the%20random%20variable%20groups.
means <- aggregate(teach_levels_data5[,c("date3","nfiller_per_token","nrep_per_token","fluency_PC1", "gcp_confidence","taales_PC1","taasc_PC1","taasc_PC3")],by=list(teach_levels_data5$ratee),FUN=mean)
lmcoefs <- summary(lm(final_teach_level ~ date3+nfiller_per_token + nrep_per_token + fluency_PC1 + gcp_confidence + taales_PC1 + taasc_PC1 + taasc_PC3+ratee, data=teach_levels_data5))$coefficients[,"Estimate"]
means$effects <- c(0,lmcoefs[substr(names(lmcoefs),1,5) == "ratee"])
means$effects <- means$effects - mean(means$effects)
cor(means[,c("date3","nfiller_per_token","nrep_per_token","fluency_PC1","gcp_confidence","taales_PC1","taasc_PC1","taasc_PC3","effects")]) # predictors are only weakly correlated with the random effect
# only nfiller correlated moderately with random effects
fixef(reduce_mod2.1)
lmcoefs[1:9] # some differences
# normality of residuals
qqnorm(residuals(reduce_mod2.1)) # does not raise any significant concern with normality of the weighted residuals

# Homogeneity of variance: https://ademos.people.uic.edu/Chapter18.html
# This procedure is a variation of “Levene’s Test”. 
# Essentially, we’ll extract the residuals from the model, take their absolute value, and then square them (for a more robust analysis with respect to issues of normality, see Glaser 2006).
# Finally we’ll take a look at the ANOVA of the between subjects residuals.
anova(lm(abs(residuals(reduce_mod2.1))^2 ~ ratee, data = teach_levels_data5)) # equal variance assumption met
# Sensitivity to data (case dianostics): help with generalizability
ggplot(data.frame(lev=hatvalues(reduce_mod2.1),pearson=residuals(reduce_mod2.1,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()
#The following code determines which of the observations have the highest leverage and displays these observations. 
#The code also generates a new model without these observations and then compares the coefficients for the will all observations to this new model with some observations removed.
levId <- which(hatvalues(reduce_mod2.1) >= 0.25)
summary(teach_levels_data5[,c("final_teach_level","nfiller_per_token","nrep_per_token","fluency_PC1","gcp_confidence","taales_PC1","taasc_PC1","taasc_PC1")])

mmLev <- lmer(final_teach_level~date3+nfiller_per_token+nrep_per_token+fluency_PC1+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3+(1|ratee), data=teach_levels_data5[-c(levId),])
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
iccMixed("final_teach_level", "ratee", teach_levels_data5) # very small between-subect variations
iccMixed("fluency_PC1", "ratee", teach_levels_data5)
iccMixed("nrep_per_token", "ratee", teach_levels_data5) 
iccMixed("nfiller_per_token", "ratee", teach_levels_data5) 
iccMixed("gcp_confidence", "ratee", teach_levels_data5) # *** boudary(singular) fit error
iccMixed("taales_PC1", "ratee", teach_levels_data5)  
iccMixed("taasc_PC1", "ratee", teach_levels_data5) 
# vairaitons in the predictors can be attrbuted (max=46%) to between-subject variation
# variation in the teach levels is mainly attrited to within-subject variation (92%)

# checking that indeed the random effect does not add much 
#https://stats.stackexchange.com/questions/141746/comparing-mixed-effects-and-fixed-effects-models-testing-significance-of-random
anova(mod2, mod1)
anova(mod2.1, mod1.1) # significant
anova(reduce_mod2, step.mod1)
anova(reduce_mod2.1, step.mod1.1)

##########################
# 3. Ordinal regression
##########################
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# https://stats.idre.ucla.edu/r/faq/ologit-coefficients/
library(MASS)
teach_levels_data5$final_teach_level <- as.factor(teach_levels_data5$final_teach_level)
mod3 <- polr(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5, Hess=TRUE)
summary(mod3) # estimates and SEs are different from LM and LMM; larger SEs --> *** what's the gain of ordinal??
deviance(mod3) # 232.0928
logLik(mod3)
AIC(mod3) # 260.0928
BIC(mod3) # 302.3347
ctable <- coef(summary(mod3))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(mod3) # profiled ci's
confint.default(mod3) # assuming normality
exp(coef(mod3)) # intepret as odds ratios
exp(cbind(OR = coef(mod3), ci)) # ci's in odds ratios

# include the time variable
mod3.1 <- polr(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5, Hess=TRUE)
summary(mod3.1)
deviance(mod3.1) # 223.7374
logLik(mod3.1)
AIC(mod3.1) # 253.7374
BIC(mod3.1) # 298.9966
ctable <- coef(summary(mod3.1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(mod3.1) # profiled ci's
confint.default(mod3.1) # assuming normality
exp(coef(mod3.1)) # intepret as odds ratios
exp(cbind(OR = coef(mod3.1), ci)) # ci's in odds ratios

# more parsimonious model
step.mod3 <- stepAIC(mod3, direction = "both")
summary(step.mod3)
deviance(step.mod3) # 235.5271
logLik(step.mod3)
AIC(step.mod3) # 253.5271
BIC(step.mod3) # 280.6826
# note it has smaller AIC and BIC
ctable <- coef(summary(step.mod3))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# include the time variable
step.mod3.1 <- polr(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = teach_levels_data5, Hess=TRUE)
summary(step.mod3.1) 
deviance(step.mod3.1) # 229.9302
logLik(step.mod3.1)
AIC(step.mod3.1) # 249.9302
BIC(step.mod3.1) # 280.103
ctable <- coef(summary(step.mod3.1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable 

# Model comparison
anova(mod3, mod3.1)
anova(step.mod3, step.mod3.1)
anova(step.mod3, mod3)
anova(step.mod3.1, mod3.1)

# Evaluations of mod3 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- polr(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6225166
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.807947
cor.test(as.numeric(pred),as.numeric(y)) # 0.5355477   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5337508
cm <- table(y, pred) 
cm
precision <- diag(cm) / colSums(cm)
precision
recall <- diag(cm) / rowSums(cm)
recall
f1 <-  2 * precision * recall / (precision + recall)
f1
# prediction on trian data
pred3 <- predict(mod3, newdata = teach_levels_data5, type="class")
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) #  0.6688742
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.8211921
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.6126288 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.6081028

# Evaluations of mod3.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(3,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- polr(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6357616
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) #  0.7880795
cor.test(as.numeric(pred),as.numeric(y)) # 0.5558633   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5524837
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y) 
# prediction on trian data
pred3 <- predict(mod3.1, newdata = teach_levels_data5, type="class")
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) #  0.6754967
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.8145695
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.6355079 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.6297535

# Evaluations of step.mod3 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- polr(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6423841
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.8211921
cor.test(as.numeric(pred),as.numeric(y)) # 0.5645565   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5612398
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y) 
# prediction on trian data
pred3 <- predict(step.mod3, newdata = teach_levels_data5, type="class")
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) #   0.6688742
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.8211921
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.6227049 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.6144798

# Evaluations of step.mod3.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(3,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- polr(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6092715
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7748344
cor.test(as.numeric(pred),as.numeric(y)) # 0.5397221   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) #  0.533739
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y) 
# prediction on trian data
pred3 <- predict(step.mod3.1, newdata = teach_levels_data5, type="class")
y3 <- teach_levels_data5$final_teach_level
sum(pred3==y3)/length(y3) #  0.6490066
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.8013245
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.6038255  
ScoreQuadraticWeightedKappa(pred3,y3) # 0.5957987

#################################################################################################
# Diagmostics of step.mod3.1: https://journal.r-project.org/archive/2018/RJ-2018-004/RJ-2018-004.pdf
# proportionl odds / parallel slope assumption
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
s <- with(teach_levels_data5, summary(as.numeric(final_teach_level) ~ date3+ fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, fun=sf))
s
# checking the parallel slope assumption using logistic regression
glm(I(as.numeric(final_teach_level) >= 2) ~ date3, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ date3, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ fluency_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ fluency_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ nfiller_per_token, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ nfiller_per_token, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ nrep_per_token, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ nrep_per_token, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ gcp_confidence, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ gcp_confidence, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ taales_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ taales_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ taasc_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ taasc_PC1, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 2) ~ taasc_PC3, family="binomial", data = teach_levels_data5)
glm(I(as.numeric(final_teach_level) >= 3) ~ taasc_PC3, family="binomial", data = teach_levels_data5)
# how big the differneces requires fitting a differential slope model??
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
# the assumption may only hold for fluency_PC1
# nomial effects test
library(ordinal)
fm <- clm(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = teach_levels_data5)
nominal_test(fm) # no evidence of non-proportional odds
scale_test(fm) # some evdence of scale effects

# assess PO assumption post by Harrell: https://groups.google.com/g/medstats/c/y_94cReelQg?pli=1
library(rms)
y <- as.factor(teach_levels_data5$final_teach_level)
Y <- as.numeric(y) - 1
ncut <- length(unique(Y)) - 1
p <- 8
Coef <- matrix(NA, ncol=p, nrow=ncut,
               dimnames=list(paste('>=', levels(y)[-1],sep=''),
                             NULL))
for(k in 1:ncut) {
  f <- lrm(Y >= k ~ date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, data = teach_levels_data5)
  Coef[k,] <- coef(f)[-1]
}
colnames(Coef) <- names(coef(f))[-1]
round(Coef, 3)

#########################################
# 4. Cumulative link linear mixed model
#########################################
# using nAGQ = 10 resulted in similar coefficients, but larger estimated variance component
# for higher dimensional random effects, try SAS implmentation of monte-carlo approximation to the integrals
library(ordinal)
teach_levels_data5$final_teach_level <- as.factor(teach_levels_data5$final_teach_level)
mod4 <-  clmm2(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=teach_levels_data5, Hess = TRUE)
summary(mod4) # similar to mod3
logLik(mod4)
AIC(mod4) # 260.5029
BIC(mod4) # 304.7446
ggplot(data.frame(mod4$ranef), aes(x=mod4.ranef)) +
  geom_histogram(binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x=mod4$ranef,y=..density..),color="red")
qqnorm(mod4$ranef, pch = 1, frame = FALSE)
qqline(mod4$ranef, col = "steelblue", lwd = 2)
shapiro.test(mod4$ranef)

# adding the time variable
mod4.1 <-  clmm2(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=teach_levels_data5, Hess = TRUE)
summary(mod4.1) 
logLik(mod4.1)
AIC(mod4.1) # 251.0225
BIC(mod4.1) # 299.299
ggplot(data.frame(mod4.1$ranef), aes(x=mod4.1.ranef)) +
  geom_histogram(binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x=mod4.1$ranef,y=..density..),color="red")
qqnorm(mod4.1$ranef, pch = 1, frame = FALSE)
qqline(mod4.1$ranef, col = "steelblue", lwd = 2)
shapiro.test(mod4.1$ranef)

# more parsimonious models
# stepwise procedure not available
reduce_mod4 <-  clmm2(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, random=ratee, data=teach_levels_data5, Hess = TRUE)
summary(reduce_mod4)
logLik(reduce_mod4)
AIC(reduce_mod4) # 254.8714
BIC(reduce_mod4) # 285.0442
ggplot(data.frame(reduce_mod4$ranef), aes(x=reduce_mod4.ranef)) +
  geom_histogram(binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x=reduce_mod4$ranef,y=..density..),color="red")
qqnorm(reduce_mod4$ranef, pch = 1, frame = FALSE)
qqline(reduce_mod4$ranef, col = "steelblue", lwd = 2)
shapiro.test(reduce_mod4$ranef)

# include the time variable
reduce_mod4.1 <-  clmm2(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, random=ratee, data=teach_levels_data5, Hess = TRUE)
summary(reduce_mod4.1)
logLik(reduce_mod4.1)
AIC(reduce_mod4.1) # 249.2739
BIC(reduce_mod4.1) # 282.464
ggplot(data.frame(reduce_mod4.1$ranef), aes(x=reduce_mod4.1.ranef)) +
  geom_histogram(binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x=reduce_mod4.1$ranef,y=..density..),color="red")
qqnorm(reduce_mod4.1$ranef, pch = 1, frame = FALSE)
qqline(reduce_mod4.1$ranef, col = "steelblue", lwd = 2)
shapiro.test(reduce_mod4.1$ranef)

# model comparison
anova(mod4, mod4.1)
anova(reduce_mod4, reduce_mod4.1)
anova(reduce_mod4, mod4)
anova(reduce_mod4.1, mod4.1)

# Evaluations of mod4 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- clmm2(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)

  nx <- testData[rep(seq_len(nrow(testData)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(testData))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "final_teach_level"
  ndat$final_teach_level <- as.factor(ndat$final_teach_level)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max))
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6092715
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7880795
cor.test(as.numeric(pred),as.numeric(y)) # 0.5168351 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) #  0.5145116
cm <- table(y, pred) 
cm
precision <- diag(cm) / colSums(cm)
precision
recall <- diag(cm) / rowSums(cm)
recall
f1 <-  2 * precision * recall / (precision + recall)
f1
# getting the predictions on training data
x = teach_levels_data5[,c(9:20)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "final_teach_level"
ndat$final_teach_level <- as.factor(ndat$final_teach_level)
pmat<- matrix(predict(mod4, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max))
sum(pclass==teach_levels_data5$final_teach_level)/nrow(teach_levels_data5) # 0.6622517
library(dplyr)
pclass2 <- recode(pclass, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(teach_levels_data5$final_teach_level, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.8013245
cor.test(as.numeric(pclass),as.numeric(teach_levels_data5$final_teach_level)) # 0.6219214   
library(Metrics)
ScoreQuadraticWeightedKappa(pclass,teach_levels_data5$final_teach_level) # 0.61629

# Evaluations of mod4.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- clmm2(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)
  nx <- testData[rep(seq_len(nrow(testData)), each = 3), ]
  #nx <- nx[,c(-1,-2,-4,-15)]
  ny <- ny <- rep(c(1,2,3),nrow(testData))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "final_teach_level"
  ndat$final_teach_level <- as.factor(ndat$final_teach_level)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max))
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
#Warning message:
#  clmm2 may not have converged:
#  optimizer 'ucminf' terminated with max|gradient|: 0.157215262138962 
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5761589
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7615894
cor.test(as.numeric(pred),as.numeric(y)) # 0.522583 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) #  0.5198833
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y) 
# getting the predictions on trainign data
x = teach_levels_data5[,c(3,9:20)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "final_teach_level"
ndat$final_teach_level <- as.factor(ndat$final_teach_level)
pmat<- matrix(predict(mod4.1, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max))
sum(pclass==teach_levels_data5$final_teach_level)/nrow(teach_levels_data5) # 0.6688742
library(dplyr)
pclass2 <- recode(pclass, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(teach_levels_data5$final_teach_level, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.8013245
cor.test(as.numeric(pclass),as.numeric(teach_levels_data5$final_teach_level)) # 0.6202214   
library(Metrics)
ScoreQuadraticWeightedKappa(pclass,teach_levels_data5$final_teach_level) # 0.6138107

# Evaluations of reduce_mod4 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- clmm2(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, random=ratee, data=trainData, Hess = TRUE)
  #x <- x[,-2] # remove teach levels
  nx <- testData[rep(seq_len(nrow(testData)), each = 3), ]
  #nx <- nx[,c(-1,-2,-3)]
  ny <- ny <- rep(c(1,2,3),nrow(testData))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "final_teach_level"
  ndat$final_teach_level <- as.factor(ndat$final_teach_level)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max))
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6357616
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.807947
cor.test(as.numeric(pred),as.numeric(y)) # 0.5378078
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5346482
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y) 
# getting the predictions on trainign data
x = teach_levels_data5[,c(9,10,11,13,16,18,19)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "final_teach_level"
ndat$final_teach_level <- as.factor(ndat$final_teach_level)
pmat<- matrix(predict(reduce_mod4, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max))
sum(pclass==teach_levels_data5$final_teach_level)/nrow(teach_levels_data5) # 0.6688742
library(dplyr)
pclass2 <- recode(pclass, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(teach_levels_data5$final_teach_level, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.807947
cor.test(as.numeric(pclass),as.numeric(teach_levels_data5$final_teach_level)) # 0.6189936   
library(Metrics)
ScoreQuadraticWeightedKappa(pclass,teach_levels_data5$final_teach_level) # 0.6106814

# Evaluations of reduce_mod4.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,8,9,10,11,13,16,18,19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <- clmm2(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+gcp_confidence+taales_PC1+taasc_PC1+taasc_PC3, random=ratee, data=trainData, Hess = TRUE)
  nx <- testData[rep(seq_len(nrow(testData)), each = 3), ]
  #nx <- nx[,c(-1,-2,-4)]
  ny <- ny <- rep(c(1,2,3),nrow(testData))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "final_teach_level"
  ndat$final_teach_level <- as.factor(ndat$final_teach_level)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max))
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6225166
# for binary classification
library(dplyr)
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.781457
cor.test(as.numeric(pred),as.numeric(y)) # 0.554482
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5492537
library(caret)
cm <- confusionMatrix(pred, y)
table(pred,y)
# getting the predictions on trainign data
x = teach_levels_data5[,c(3,9,10,11,13,16,18,19)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "final_teach_level"
ndat$final_teach_level <- as.factor(ndat$final_teach_level)
pmat<- matrix(predict(reduce_mod4.1, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max))
sum(pclass==teach_levels_data5$final_teach_level)/nrow(teach_levels_data5) # 0.6556291
library(dplyr)
pclass2 <- recode(pclass, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(teach_levels_data5$final_teach_level, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.807947
cor.test(as.numeric(pclass),as.numeric(teach_levels_data5$final_teach_level)) # 0.6090585   
library(Metrics)
ScoreQuadraticWeightedKappa(pclass,teach_levels_data5$final_teach_level) # 0.6106814

#############################
# 5. Nominal regression
#############################
library(nnet)
teach_levels_data5$final_teach_level <- as.factor(teach_levels_data5$final_teach_level)
teach_levels_data5$final_teach_level <- relevel(teach_levels_data5$final_teach_level , ref = "1") # set baseline
mod5 <- multinom(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5)
summary(mod5)
# p-values using Wald tests (two-tailed z-tests)
z <- summary(mod5)$coefficients/summary(mod5)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
library(mlogit)
mod5_mlogit <- mlogit(final_teach_level ~ 0 | fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = teach_levels_data5, shape = "wide", reflevel = "1")
summary(mod5_mlogit)
deviance(mod5) # 222.9027
AIC(mod5) # 274.9027
BIC(mod5) # 353.3519

# include time variable
mod5.1 <- multinom(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taaco_PC1, data = teach_levels_data5)
summary(mod5.1)
z <- summary(mod5.1)$coefficients/summary(mod5.1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
deviance(mod5.1) # 219.1001
AIC(mod5.1) # 271.1001
BIC(mod5.1) # 349.5494

# more parsimonious model
step.mod5 <- stepAIC(mod5, direction = "both") # note taasc_PC1 is not selected
summary(step.mod5)
z <- summary(step.mod5)$coefficients/summary(step.mod5)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
deviance(step.mod5) # 244.2588
AIC(step.mod5) # 264.2588
BIC(step.mod5) # 294.4316

# include time variable
step.mod5.1 <- stepAIC(mod5.1, direction = "both") 
summary(step.mod5.1)
z <- summary(step.mod5.1)$coefficients/summary(step.mod5.1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
deviance(step.mod5.1) # 238.1234
AIC(step.mod5.1) # 262.1234
BIC(step.mod5.1) # 298.3308

# Evaluations mod5 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,8,9:20)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  
  tmp.mod <-multinom(final_teach_level~fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1  2  3 
#31 73 47 
table(y)
sum(pred==y)/length(y) # 0.5960265
cor.test(as.numeric(pred),as.numeric(y)) # 0.4865022
cm <- confusionMatrix(pred, y)
cm

# Evaluations mod5.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,8,9:19)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  
  tmp.mod <-multinom(final_teach_level~date3+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taaco_PC1, data = trainData)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1  2  3 
#24 79 48 
table(y)
sum(pred==y)/length(y) # 0.5894044
cor.test(as.numeric(pred),as.numeric(y)) # 0.5124494 
cm <- confusionMatrix(pred, y)
cm

# Evaluations step.mod5 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,8,10,11,13,16)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  tmp.mod <-multinom(final_teach_level~fluency_PC1+nrep_per_token+gcp_confidence+taales_PC1, data = trainData)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1  2  3 
#17 85 49 
table(y)
sum(pred==y)/length(y) # 0.5562914
cor.test(as.numeric(pred),as.numeric(y)) # 0.4761087
cm <- confusionMatrix(pred, y)
cm

# Evaluations step.mod5.1 based on 10-fold cv
tmp_dat = teach_levels_data5[,c(2,3,8,10,11,13,16)]
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  
  tmp.mod <-multinom(final_teach_level~date3+fluency_PC1+nrep_per_token+gcp_confidence+taales_PC1, data = trainData)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1  2  3 
#19 84 48 
table(y)
sum(pred==y)/length(y) # 0.5430464
cor.test(as.numeric(pred),as.numeric(y)) # 0.4876092 
cm <- confusionMatrix(pred, y)
cm
# **** probability plots for the significant effects & model diagnostics

#############################
# 6. Lasso regression
#############################
library(glmnet)
x = as.matrix(teach_levels_data5[,c(9:19)])
y = as.matrix(teach_levels_data5$final_teach_level)
mod6= glmnet(x, y) # linear regression
plot(mod6)
plot(mod6, xvar = "lambda", label = TRUE)
plot(mod6, xvar = "dev", label = TRUE)
print(mod6)
coef(mod6,s=0.1)
pred <- round(predict(mod6,newx=x)[,which.min(mod6$lambda)])
sum(pred==y)/length(y) # 0.5960265

# Evaluations based on 10-fold cv
tmp_dat = data.frame(cbind(y,x))
names(tmp_dat)[1] <- 'final_teach_level'
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  x_train <- data.matrix(trainData[,c(2:12)])
  y_train <- data.matrix(trainData$final_teach_level) 
  tmp.mod=glmnet(x_train, y_train)
  x_test <- data.matrix(testData[,c(2:12)])
  pred <- round(predict(tmp.mod,newx=x_test)[,which.min(tmp.mod$lambda)])
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pre
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1   2   3 
#10 106  35
table(y)
sum(pred==y)/length(y) # 0.5298013

# cross-validation for the lambda parameter
cvmod5 = cv.glmnet(x, y, type.measure = "mse", nfolds = 10)
plot(cvmod5)
cvmod5$lambda.min
cvmod5$lambda.1se
coef(cvmod5, s = "lambda.min")

# multinomial modeling
mod6.1 = glmnet(x, y, family = "multinomial") # multinomial
plot(mod6.1, xvar = "lambda", label = TRUE, type.coef = "2norm")
print(mod6.1)
coef(mod6.1,s=0.1)
cvmod6.1=cv.glmnet(x, y, family="multinomial", parallel = TRUE)
plot(cvmod6.1)
cvmod6.1$lambda.min
pred <- predict(cvmod6.1, newx = x, s = "lambda.min", type = "class")
sum(pred==y)/length(y) # 0.6423841 accuacy on training data

# Evaluations based on 10-fold cv
tmp_dat = data.frame(cbind(y,x))
names(tmp_dat)[1] <- 'final_teach_level'
names(tmp_dat)
#Randomly shuffle the data
tmp_dat$ID <- seq.int(nrow(tmp_dat))
set.seed(123)
tmp_dat<-tmp_dat[sample(nrow(tmp_dat)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(tmp_dat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
d = NULL # empty dataframe
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- tmp_dat[testIndexes, ]
  trainData <- tmp_dat[-testIndexes, ]
  x_train <- data.matrix(trainData[,c(2:12)])
  y_train <- data.matrix(trainData$final_teach_level) 
  tmp.mod=glmnet(x_train, y_train, family="multinomial") # without tuning the lamda parameter via 10cv
  x_test <- data.matrix(testData[,c(2:12)])
  pred <- predict(tmp.mod,newx=x_test,type="class")[,which.min(tmp.mod$lambda)]
  # *** Error in cbind2(1, newx) %*% (nbeta[[i]]) : 
  # *** invalid class 'NA' to dup_mMatrix_as_dgeMatrix
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$final_teach_level
table(pred)
#1  2  3 
#29 73 49 
table(y)
sum(pred==y)/length(y) # 0.602649
cor(as.numeric(pred),as.numeric(y)) # 0.274628
cm <- confusionMatrix(pred, y)
cm


