# get the data
teach_levels_data5 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data5.csv") # PCA soutions really similar to those of score-level analysis
stats_scoring_RPlat_original <- read.csv("~/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv")
rating <- merge(teach_levels_data5, stats_scoring_RPlat_original, by="soundname")
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
write.csv(rating,"~/Desktop/TEMP/DISS/rating_data.csv")

# some visualization
library(ggplot2)
rating %>% mutate(teach_total = ordered(as.factor(teach_total), levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = nrep_per_token, fill = teach_total)) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = nfiller_per_token, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = fluency_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = L7, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = gcp_confidence, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = rpvi_v, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = lca_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = taales_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = sca_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = taasc_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = taasc_PC3, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = taaco_PC1, fill = teach_total )) + geom_histogram(binwidth = 0.5) + theme_minimal() 
rating %>% mutate(teach_total = ordered(as.factor(teach_total) , levels=rev(levels(as.factor(teach_total) )))) %>% 
  ggplot( aes(x = date3, fill = teach_total )) + geom_histogram(binwidth = 2) + theme_minimal() 

###############################################################################################
# 1. MLR assuming different examinee appearing at different times and continous outcome variable
###############################################################################################
mod1 <- lm(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = rating)
summary(mod1) # adj R2 = 0.195  ; note large standard errors for rater parameters  
mean(mod1$residuals^2) # 0.3805686 
var(mod1$residuals)
logLik(mod1)
AIC(mod1) # 728.177
BIC(mod1) # 787.0007
cor(rating[,9:20])
# fluency_pc1 and L7, sca_PC1 are highly correlated

# include the time variable
mod1.1 <- lm(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = rating)
summary(mod1.1) # adj R2 = 0.2108, time is significant 
mean(mod1.1$residuals^2) #  0.3720518
var(mod1.1$residuals)
logLik(mod1.1)
AIC(mod1.1) # 721.7347
BIC(mod1.1) # 784.48

# more parsimonious models
library(MASS)
step.mod1 <- stepAIC(mod1,direction = "both") # note step is not applied for mod1.1
summary(step.mod1) # adjusted R^2 = 0.1992; note slightly different feature selelcted than score-level
mean(step.mod1$residuals^2) # 0.3870016
var(step.mod1$residuals)
logLik(step.mod1)
AIC(step.mod1) # 718.4294
BIC(step.mod1) # 745.8805
cor(rating[,c(10,12,13,16,19)]) # no issue of multicollinearity

step.mod1.1 <- lm(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3, data = rating)
summary(step.mod1.1) # adjusted R^2 =  0.2131; time is significant
mean(step.mod1.1$residuals^2) # 0.3792884
var(step.mod1.1$residuals)
logLik(step.mod1.1)
AIC(step.mod1.1) # 712.9202
BIC(step.mod1.1) # 744.2928

# model comparison
anova(mod1, mod1.1) # time is significant
anova(step.mod1, step.mod1.1) # time significant
anova(step.mod1, mod1) # prefer parsimonious model
anova(step.mod1.1, mod1.1) # prefer parsimonious model

# Evaluations of mod1 based on 10-fold cv
tmp_dat = rating[,c(9:20,25,32)]
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
  tmp.mod <-lm(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1++taasc_PC3+taaco_PC1, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5227882
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6541555
cor.test(pred,y) # 0.2772716 
cor.test(pred1,y) # 0.3957834
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.2504035
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod1, newdata = rating))
pred4 <- predict(mod1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5469169
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.6809651
cor.test(pred3,as.numeric(y3)) # 0.3342792
cor.test(pred4,as.numeric(y3)) # 0.4723699
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.2967929

# Evaluations of mod1.1 based on 10-fold cv
tmp_dat = rating[,c(3,9:20,25,32)]
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
  tmp.mod <-lm(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) #  0.5281501
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6702413
cor.test(pred,y) # 0.3084048 
cor.test(pred1,y) # 0.4112152 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.2761992
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod1.1, newdata = rating))
pred4 <- predict(mod1.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5522788
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.69437
cor.test(pred3,y3) # 0.3301744 
cor.test(pred4,y3) # 0.2953862 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3140783

# Evaluations step.mod1 based on 10-fold cv
tmp_dat = rating[,c(10,12,13,16,19,25,32)]
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
  tmp.mod <-lm(teach_total~rater_num2+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5227882
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6595174
cor.test(pred,y) # 0.2808524  
cor.test(pred1,y) # 0.4246417
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3017492
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(step.mod1.1, newdata = rating))
pred4 <- predict(step.mod1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5522788
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.69437
cor.test(pred3,as.numeric(y3)) # 0.3301744 
cor.test(pred4,as.numeric(y3)) # 0.4723699 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.3140783
# prediction on trian data
pred3 <- round(predict(step.mod1, newdata = rating))
pred4 <- predict(step.mod1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.541555
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.6836461
cor.test(pred3,y3) # 0.304991  
cor.test(pred4,y3) # 0.2703435
ScoreQuadraticWeightedKappa(pred3,y3) # 0.2908142

# Evaluations step.mod1.1 based on 10-fold cv
tmp_dat = rating[,c(3,10,12,13,16,19,25,32)]
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
  tmp.mod <-lm(teach_total~date3+rater_num2+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5254692
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6702413
cor.test(pred,y) # 0.297801  
cor.test(pred1,y) # 0.4462798  
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3560462
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(step.mod1.1, newdata = rating))
pred4 <- predict(step.mod1.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5656836
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.7265416
cor.test(pred3,y3) # 0.4212648 
cor.test(pred4,y3) # 0.450709
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3895828

# *** Model diagnostics of step.mod1.1
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
library(tidyverse)
library(broom)
theme_set(theme_classic())
model.diag.metrics <- augment(step.mod1.1)
head(model.diag.metrics)
ggplot(model.diag.metrics, aes(nrep_per_token, teach_total)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = nrep_per_token, yend = .fitted), color = "red", size = 0.3)
par(mfrow = c(2, 2))
plot(step.mod1.1)
library(ggfortify)
autoplot(step.mod1.1)
# linearity, normality, homoskedasticty are all problematic; a few influental cases can be identified
library(car)
residualPlots(step.mod1.1) # non-constant variance for some predictors; problem with pearson residuals vs. fitted values 
                           # Tukey's test of non-additivity shows that normality may be met
marginalModelPlots(step.mod1.1) # marginal relationship between the response and each regressor: model lines seemed to match well the data lines
avPlots(step.mod1.1, id=list(n=2)) # scatterplot of two sets of residuals
                                   # in each graph, two cases farthest from the horizontal mean; two cases with largest absolute residuals are identified
                                   # slope indicates the strength of relationship; the spread indicates precision of beta
mcPlots(step.mod1.1, ~ date3, overlaid = FALSE) # for comparion purposes; centering makes it possible to compare with added variable plots
mcPlots(step.mod1.1, ~ rater_num, overlaid = FALSE)
mcPlots(step.mod1.1, ~ nrep_per_token, overlaid = FALSE) 
mcPlots(step.mod1.1, ~ L7, overlaid = FALSE)
mcPlots(step.mod1.1, ~ gcp_confidence, overlaid = FALSE)
mcPlots(step.mod1.1, ~ taales_PC1, overlaid = FALSE)
mcPlots(step.mod1.1, ~ taaco_PC1, overlaid = FALSE)

###############################################################################################
# 2. LMM assuming same examinee appearing at differnet times and continous outcome variable; ratee and/or raters as random effects
###############################################################################################
library(lme4)
library(lmerTest)
mod2 <- lmer(teach_total~rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = rating)
summary(mod2) 
mean(residuals(mod2)^2) # 0.2055371
logLik(mod2)
AIC(mod2) # 731.8647
BIC(mod2) # 914.1409
ggplot(data.frame(ranef(mod2)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
# looked like bimodal; mean = 2.171847e-16, var = 0.0147789
qqnorm(ranef(mod2)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2)$ratee[,1])

# partially crossed random effects
mod2.0 <- lmer(teach_total~rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), REML = FALSE, data = rating)
summary(mod2.0)
mean(residuals(mod2.0)^2) # 0.1869196
logLik(mod2.0)
AIC(mod2.0) # 730.5585
BIC(mod2.0) # 797.2254
ggplot(data.frame(ranef(mod2.0)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(mod2.0)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.0)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.0)$ratee[,1])

ggplot(data.frame(ranef(mod2.0)$rater)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(mod2.0)$rater[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.0)$rater[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.0)$rater[,1])

# visualize Var(y) - variance covariance matrix
# http://errickson.net/stats-notes/vizrandomeffects.html
rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}
image(rescov(mod2.0, rating))
image(rescov(mod2.0, rating)[1:20,1:20]) # zoom in

# include the time variable
mod2.1 <- lmer(teach_total~date3+rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML = FALSE, data = rating)
summary(mod2.1) # time is significant
mean(residuals(mod2.1)^2) # 0.1951322
logLik(mod2.1)
AIC(mod2.1) # 728.3262
BIC(mod2.1) # 794.993
ggplot(data.frame(ranef(mod2.1)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(mod2.1)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.1)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.1)$ratee[,1])

mod2.1.0 <- lmer(teach_total~date3+rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), REML= FALSE, data = rating)
summary(mod2.1.0) # time is significant
mean(residuals(mod2.1.0)^2) # 0.1829771
logLik(mod2.1.0)
AIC(mod2.1.0) # 728.6358
BIC(mod2.1.0) # 799.2242
ggplot(data.frame(ranef(mod2.1.0)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(mod2.1.0)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.1.0)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.1.0)$ratee[,1])

ggplot(data.frame(ranef(mod2.1.0)$rater)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(mod2.1.0)$rater[,1], pch = 1, frame = FALSE)
qqline(ranef(mod2.1.0)$rater[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(mod2.1.0)$rater[,1])

# more parsimonious models
#https://stackoverflow.com/questions/55638476/how-to-do-stepwise-model-with-random-effect-lme4-lmertest
fixmodel <- lm(formula(mod2,fixed.only=TRUE),
               data=eval(getCall(mod2)$data))
step.mod2 <- step(fixmodel)
summary(step.mod2) 
reduce_mod2 <- lmer(teach_total~nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee), REML= FALSE, data = rating)
summary(reduce_mod2) # more significant features than mod2
mean(residuals(reduce_mod2)^2) # 0.2248499
logLik(reduce_mod2)
AIC(reduce_mod2) # 698.6545
BIC(reduce_mod2) # 730.0272
ggplot(data.frame(ranef(reduce_mod2)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2)$ratee[,1])

reduce_mod2.0 <- lmer(teach_total~nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee)+(1|rater), REML = FALSE, data = rating)
summary(reduce_mod2.0) # more significant features than mod2.0
mean(residuals(reduce_mod2.0)^2) #  0.2011727
logLik(reduce_mod2.0)
AIC(reduce_mod2.0) # 694.9838
BIC(reduce_mod2.0) # 730.278
ggplot(data.frame(ranef(reduce_mod2.0)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2.0)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.0)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.0)$ratee[,1])

ggplot(data.frame(ranef(reduce_mod2.0)$rater)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2.0)$rater[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.0)$rater[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.0)$rater[,1])

reduce_mod2.1 <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1++taasc_PC3+(1|ratee), REML = FALSE, data = rating)
summary(reduce_mod2.1) 
mean(residuals(reduce_mod2.1)^2) # 0.2074192
logLik(reduce_mod2.1)
AIC(reduce_mod2.1) # 692.6912
BIC(reduce_mod2.1) # 727.9854
ggplot(data.frame(ranef(reduce_mod2.1)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2.1)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.1)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.1)$ratee[,1])

reduce_mod2.1.0 <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1++taasc_PC3+(1|ratee)+(1|rater), REML = FALSE, data = rating)
summary(reduce_mod2.1.0) 
mean(residuals(reduce_mod2.1.0)^2) # 0.1925858
logLik(reduce_mod2.1.0)
AIC(reduce_mod2.1.0) # 691.609
BIC(reduce_mod2.1.0) # 730.82483
ggplot(data.frame(ranef(reduce_mod2.1.0)$ratee)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2.1.0)$ratee[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.1.0)$ratee[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.1.0)$ratee[,1])

ggplot(data.frame(ranef(reduce_mod2.1.0)$rater)) +
  geom_histogram(aes(x = X.Intercept., y = ..density..), binwidth = 0.05, fill = "grey", color = "black") +
  geom_density(aes(x = X.Intercept., y = ..density..), color="red")
qqnorm(ranef(reduce_mod2.1.0)$rater[,1], pch = 1, frame = FALSE)
qqline(ranef(reduce_mod2.1.0)$rater[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(reduce_mod2.1.0)$rater[,1])

# model comparison
anova(mod2, mod2.1) # time significant
anova(mod2.1, mod2.1.0) # time not significant
anova(reduce_mod2, reduce_mod2.1) # time significant
anova(reduce_mod2.0, reduce_mod2.1.0) # time significant
anova(reduce_mod2, mod2) # prefer simpler model
anova(reduce_mod2.1, mod2.1) # prefer simpler model
anova(reduce_mod2.0, mod2.0) # prefer simpler model
anova(reduce_mod2.1.0, mod2.1.0) # prefer simpler model
anova(mod2, mod2.0) # not useful to add rater random effect
anova(mod2.1, mod2.1.0) # not useful to add rater random effect
anova(reduce_mod2, reduce_mod2.0) # useful to add ratee random effect
anova(reduce_mod2.1, reduce_mod2.1.0) # useful to add ratee random effect

# Evaluations of mod2 based on 10-fold cv
tmp_dat = rating[,c(2,9:20,25,32)]
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
  tmp.mod <-lmer(teach_total~rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.613941
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7426273
cor.test(pred,y) # 0.4955031 
cor.test(pred1,y) # 0.6001491
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4800991
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod2, newdata = rating))
pred4 <- predict(mod2, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7345845
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8337802
cor.test(pred3,as.numeric(y3)) # 0.6870139
cor.test(pred4,as.numeric(y3)) # 0.7685609
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6696369

# Evaluations of mod2.1 based on 10-fold cv
tmp_dat = rating[,c(2,3,9:20,25,32)]
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
  tmp.mod <-lmer(teach_total~date3+rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6434316
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.766756
cor.test(pred,y) # 0.5657822 
cor.test(pred1,y) # 0.6139102 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5493082
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod2.1, newdata = rating))
pred4 <- predict(mod2.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7345845
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8284182
cor.test(pred3,as.numeric(y3)) # 0.6876195 
cor.test(pred4,as.numeric(y3)) # 0.7814494 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6719612

# Evaluations of reduce_mod2 based on 10-fold cv
tmp_dat = rating[,c(2,10,12,13,16,19,22,25)]
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
  tmp.mod <- lmer(teach_total~nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5710456
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7050938
cor.test(pred,y) # 0.4574427
cor.test(pred1,y) # 0.5947771   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4381155
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2, newdata = rating))
pred4 <- predict(reduce_mod2, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7131367
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8069705
cor.test(pred3,as.numeric(y3)) # 0.655411  
cor.test(pred4,as.numeric(y3)) # 0.7439914 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6349392

# Evaluations of reduce_mod2.1 based on 10-fold cv
tmp_dat = rating[,c(2,3,10,12,13,16,19,22,25)]
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
  tmp.mod <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee), REML = FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6461126
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.769437
cor.test(pred,y) # 0.5628876
cor.test(pred1,y) # 0.6169362   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5424759
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2.1, newdata = rating))
pred4 <- predict(reduce_mod2.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.72922257
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8257373
cor.test(pred3,as.numeric(y3)) # 0.6781863   
cor.test(pred4,as.numeric(y3)) # 0.7664126 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6349392

# Evaluations of mod2.0 based on 10-fold cv
tmp_dat = rating[,c(2,9:20,22,25,32)]
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
  tmp.mod <-lmer(teach_total~rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6193029
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7426273
cor.test(pred,y) # 0.5150173 
cor.test(pred1,y) # 0.6029266 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.4800991
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod2.0, newdata = rating))
pred4 <- predict(mod2.0, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7506702
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8391421
cor.test(pred3,as.numeric(y3)) # 0.7074489 
cor.test(pred4,as.numeric(y3)) # 0.7927885 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6908812

# Evaluations of mod2.1.0 based on 10-fold cv
tmp_dat = rating[,c(2,3,9:20,22,25,32)]
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
  tmp.mod <-lmer(teach_total~date3+rater_num2+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6380697
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.766756
cor.test(pred,y) # 0.5527446 
cor.test(pred1,y) # 0.6127547  
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5391516
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(mod2.1.0, newdata = rating))
pred4 <- predict(mod2.1.0, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7533512
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.844504
cor.test(pred3,as.numeric(y3)) # 0.7130805  
cor.test(pred4,as.numeric(y3)) # 0.7959988 
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6985594

# Evaluations of reduce_mod2.0 based on 10-fold cv
tmp_dat = rating[,c(2,10,12,13,16,19,22,25)]
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
  tmp.mod <- lmer(teach_total~nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee)+(1|rater), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5951743
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7238606
cor.test(pred,y) # 0.4863806  
cor.test(pred1,y) # 0.6073376   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5205523
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2.0, newdata = rating))
pred4 <- predict(reduce_mod2.0, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7345845
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8364611
cor.test(pred3,as.numeric(y3)) # 0.684452  
cor.test(pred4,as.numeric(y3)) # 0.7774842  
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6645226

# Evaluations of reduce_mod2.1.0 based on 10-fold cv
tmp_dat = rating[,c(2,3,10,12,13,16,19,22,25)]
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
  tmp.mod <- lmer(teach_total~date3+nrep_per_token+L7+gcp_confidence+taales_PC1+taasc_PC3+(1|ratee)+(1|rater), REML=FALSE, data = trainData)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.6246649
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7533512
cor.test(pred,y) # 0.5318441 
cor.test(pred1,y) # 0.62133   
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.5205523
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- round(predict(reduce_mod2.1.0, newdata = rating))
pred4 <- predict(reduce_mod2.1.0, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.7453083
library(dplyr)
pred5 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred5==y4)/length(y4) # 0.8364611
cor.test(pred3,as.numeric(y3)) # 0.7002882  
cor.test(pred4,as.numeric(y3)) # 0.7869042  
ScoreQuadraticWeightedKappa(pred3,as.numeric(y3)) # 0.6833758

# Modal diagnostics

#########################
# 3. Ordinal regression
#########################
library(MASS)
rating$teach_total <- as.factor(rating$teach_total)
mod3 <- polr(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = rating, Hess=TRUE)
summary(mod3) # estimates and SEs are different from LM and LMM; larger SEs --> *** what's the gain of ordinal??
deviance(mod3) # 651.581
logLik(mod3)
AIC(mod3) # 681.581
BIC(mod3) # 740.4047
ctable <- coef(summary(mod3))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# include the time variable
mod3.1 <- polr(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = rating, Hess=TRUE)
summary(mod3.1) 
deviance(mod3.1) # 641.6587
logLik(mod3.1)
AIC(mod3.1) # 673.6587
BIC(mod3.1) # 736.4039
ctable <- coef(summary(mod3.1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# more parsimonious model
step.mod3 <- stepAIC(mod3, direction = "both")
summary(step.mod3)
deviance(step.mod3) # 654.8732
logLik(step.mod3)
AIC(step.mod3) # 672.8732
BIC(step.mod3) # 708.1674
ctable <- coef(summary(step.mod3))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# include the time variable
step.mod3.1 <- polr(teach_total~date3++nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, data = rating, Hess=TRUE)
summary(step.mod3.1) # 
deviance(step.mod3.1) # 645.8168
logLik(step.mod3.1)
AIC(step.mod3.1) # 665.8168
BIC(step.mod3.1) # 705.0326
ctable <- coef(summary(step.mod3.1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable 

# model comparison
anova(mod3, mod3.1) # time is significant
anova(step.mod3, step.mod3.1) # time is significant
anova(step.mod3, mod3) # prefer simpler model
anova(step.mod3.1, mod3.1) # prefer simpler model

# Evaluations of mod3 based on 10-fold cv
tmp_dat = rating[,c(2,9:20,25,32)]
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
  tmp.mod <- polr(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5522788
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6729223
cor.test(as.numeric(pred),as.numeric(y)) # 0.3587717 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3425049 
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- predict(mod3, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5817694
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.6997319
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.3999348 
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3800325

# Evaluations of mod3.1 based on 10-fold cv
tmp_dat = rating[,c(2,3,9:20,25,32)]
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
  tmp.mod <- polr(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5308311
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6648794
cor.test(as.numeric(pred),as.numeric(y)) # 0.3238922 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3149172 
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns
# prediction on trian data
pred3 <- predict(mod3.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5495979
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.6782842
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.3658889  
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3517437

# Evaluations of step.mod3 based on 10-fold cv
tmp_dat = rating[,c(10,12,13,15,16,19,20,25)]
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
  tmp.mod <- polr(teach_total~nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5656836
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6863271
cor.test(as.numeric(pred),as.numeric(y)) # 0.3721728 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3501377
library(caret)
cm <- confusionMatrix(pred, y)
cm
# prediction on trian data
pred3 <- predict(step.mod3, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5710456
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.689008
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.3807139   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3595013

# Evaluations of step.mod3.1 based on 10-fold cv
tmp_dat = rating[,c(3,10,12,13,15,16,19,20,25)]
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
  tmp.mod <- polr(teach_total~date3+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, data = trainData, Hess=TRUE)
  pred <- predict(tmp.mod, newdata = testData, type="class")
  index <- testData['ID']
  d = rbind(d, data.frame(index, pred))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pred
pred1 <- d$pred1
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.536193
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6702413
cor.test(as.numeric(pred),as.numeric(y)) # 0.3286724
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3135986
library(caret)
cm <- confusionMatrix(pred, y)
cm
# prediction on trian data
pred3 <- predict(step.mod3.1, newdata = rating)
y3 <- rating$teach_total
sum(pred3==y3)/length(y3) # 0.5495979
library(dplyr)
pred4 <- recode(pred3, '1'=0, '2'=0, '3'=1, '4'=1)
y4 <- recode(y3, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred4==y4)/length(y4) # 0.6836461
cor.test(as.numeric(pred3),as.numeric(y3)) # 0.352714   
ScoreQuadraticWeightedKappa(pred3,y3) # 0.3357736

# model diagnostics

##########################################################################
# 4. Cumulative link linear mixed model: ratee and/or rater random effects
##########################################################################
library(ordinal)
rating$teach_total <- factor(rating$teach_total)
mod4 <-  clmm2(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=rating, Hess = TRUE)
summary(mod4) 
logLik(mod4)
AIC(mod4) # 629.8136
BIC(mod4) # 692.5589
ggplot(data.frame(mod4$ranef)) +
  geom_histogram(aes(x = mod4.ranef, y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.ranef, y = ..density..), color="red")
qqnorm(mod4$ranef, pch = 1, frame = FALSE)
qqline(mod4$ranef, col = "steelblue", lwd = 2)
shapiro.test(mod4$ranef)
# getting the predictions on trainign data
x = rating[,c(9:20,31)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "teach_total"
ndat$teach_total <- as.factor(ndat$teach_total)
pmat<- matrix(predict(mod4, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max), ordered = TRUE)
sum(pclass==rating$teach_total)/nrow(rating) # 0.6047745

mod4.0 <- clmm(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), data=rating, Hess = TRUE)
summary(mod4.0) 
logLik(mod4.0)
AIC(mod4.0) # 630.3281
BIC(mod4.0) # 696.995
ggplot(data.frame(mod4.0$ranef[1:74])) +
  geom_histogram(aes(x = mod4.0$ranef[1:74], y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.0$ranef[1:74], y = ..density..), color="red")
qqnorm(mod4.0$ranef[1:74], pch = 1, frame = FALSE)
qqline(mod4.0$ranef[1:74], col = "steelblue", lwd = 2)
shapiro.test(mod4.0$ranef[1:74])

ggplot(data.frame(mod4.0$ranef[75:95])) +
  geom_histogram(aes(x = mod4.0$ranef[75:95], y = ..density..), binwidth = 0.25, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.0$ranef[75:95], y = ..density..), color="red")
qqnorm(mod4.0$ranef[75:95], pch = 1, frame = FALSE)
qqline(mod4.0$ranef[75:95], col = "steelblue", lwd = 2)
shapiro.test(mod4.0$ranef[75:95])

# plotting the ranefs
ci <- mod4.0$ranef + qnorm(0.975) * sqrt(mod4.0$condVar) %o% c(-1,1)
ord.re <- order(mod4.0$ranef)
ci <- ci[order(mod4.0$ranef),]
plot(1:95, mod4.0$ranef[ord.re], axes=FALSE, ylim=range(ci),
     xlab="Ratee + Rater", ylab="random effect")
axis(1, at=1:95, labels = ord.re)
axis(2)
for(i in 1:95) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

# plotting the predicted probabilities

# getting the predictions on trainign data
x = rating[,c(9:19)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "teach_total"
ndat$teach_total <- as.factor(ndat$teach_total)
pmat<- matrix(predict(mod4.0, newdata = ndat), ncol=3, byrow = TRUE)
# **** Error in UseMethod("predict") : 
# no applicable method for 'predict' applied to an object of class "clmm"
pclass <- factor(apply(pmat, 1, which.max))
sum(pclass==rating$teach_total)/nrow(rating) 
# **** https://gist.github.com/Martin-Jung/33ca6591193cb59f0a88
# Arguments:
#  - model = a clmm model
#  - newdata = a dataframe of new data to apply the model to
# Returns a dataframe of predicted probabilities for each row and response level
fake.predict.clmm <- function(model, newdata) {
  # Actual prediction function
    pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }
  
  # Multiply each row by the coefficients
  coefs <- c(model$beta, unlist(model$ST))
  xbetas <- sweep(newdata, MARGIN=2, coefs, FUN="*")
  
  # Make predictions
  pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=model$Theta))
  colnames(pred.mat) <- levels(model$model[,1])
  pred.mat
}
tmp_dat <- rating[,c(9:20,2,22,25)]
names(tmp_dat)
fake.predict.clmm(mod4.0, rating)
# ** need to manually calculate the predicted probabilities

mod4.1 <-  clmm2(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=rating, Hess = TRUE)
summary(mod4.1) # time is significant
logLik(mod4.1)
AIC(mod4.1) # 616.5497
BIC(mod4.1) # 683.2165
ggplot(data.frame(mod4.1$ranef)) +
  geom_histogram(aes(x = mod4.1.ranef, y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.1.ranef, y = ..density..), color="red")
qqnorm(mod4.1$ranef, pch = 1, frame = FALSE)
qqline(mod4.1$ranef, col = "steelblue", lwd = 2)
shapiro.test(mod4.1$ranef)
# getting the predictions on trainign data
x = rating[,c(3,9:20,31)]
head(x)
nx <- x[rep(seq_len(nrow(x)), each = 3), ]
head(nx)
ny <- rep(c(1,2,3),nrow(x))
ndat <- cbind(ny,nx)
colnames(ndat)[1] <- "teach_total"
ndat$teach_total <- as.factor(ndat$teach_total)
pmat<- matrix(predict(mod4.1, newdata = ndat), ncol=3, byrow = TRUE)
pclass <- factor(apply(pmat, 1, which.max), ordered = TRUE)
sum(pclass==rating$teach_total)/nrow(rating) # 0.6074271

mod4.0.1 <- clmm(teach_total~date3+rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), data=rating, Hess = TRUE)
summary(mod4.0.1) # time is significant
logLik(mod4.0.1)
AIC(mod4.0.1) # 618.0854
BIC(mod4.0.1) # 688.6738
ggplot(data.frame(mod4.0.1$ranef[1:74])) +
  geom_histogram(aes(x = mod4.0.1$ranef[1:74], y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.0.1$ranef[1:74], y = ..density..), color="red")
qqnorm(mod4.0.1$ranef[1:74], pch = 1, frame = FALSE)
qqline(mod4.0.1$ranef[1:74], col = "steelblue", lwd = 2)
shapiro.test(mod4.0.1$ranef[1:74])

ggplot(data.frame(mod4.0.1$ranef[75:95])) +
  geom_histogram(aes(x = mod4.0.1$ranef[75:95], y = ..density..), binwidth = 0.25, fill = "grey", color = "black") +
  geom_density(aes(x = mod4.0.1$ranef[75:95], y = ..density..), color="red")
qqnorm(mod4.0.1$ranef[75:95], pch = 1, frame = FALSE)
qqline(mod4.0.1$ranef[75:95], col = "steelblue", lwd = 2)
shapiro.test(mod4.0.1$ranef[75:95])

# plotting the ranefs
ci <- mod4.0.1$ranef + qnorm(0.975) * sqrt(mod4.0.1$condVar) %o% c(-1,1)
ord.re <- order(mod4.0.1$ranef)
ci <- ci[order(mod4.0.1$ranef),]
plot(1:95, mod4.0.1$ranef[ord.re], axes=FALSE, ylim=range(ci),
     xlab="Ratee + Rater", ylab="random effect")
axis(1, at=1:95, labels = ord.re)
axis(2)
for(i in 1:95) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

# step procedure is not available
reduce_mod4 <- clmm2(teach_total~rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, random=ratee, data=rating, Hess = TRUE)
summary(reduce_mod4) 
logLik(reduce_mod4)
AIC(reduce_mod4) # 625.5211
BIC(reduce_mod4) # 668.6584
ggplot(data.frame(reduce_mod4$ranef)) +
  geom_histogram(aes(x = reduce_mod4.ranef, y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = reduce_mod4.ranef, y = ..density..), color="red")
qqnorm(reduce_mod4$ranef, pch = 1, frame = FALSE)
qqline(reduce_mod4$ranef, col = "steelblue", lwd = 2)
shapiro.test(reduce_mod4$ranef)

# add the time variable
reduce_mod4.1 <- clmm2(teach_total~date3+rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, random=ratee, data=rating, Hess = TRUE)
summary(reduce_mod4.1) # time is significant
logLik(reduce_mod4.1)
AIC(reduce_mod4.1) # 610.9688
BIC(reduce_mod4.1) # 658.0277
ggplot(data.frame(reduce_mod4.1$ranef)) +
  geom_histogram(aes(x = reduce_mod4.1.ranef, y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = reduce_mod4.1.ranef, y = ..density..), color="red")
qqnorm(reduce_mod4.1$ranef, pch = 1, frame = FALSE)
qqline(reduce_mod4.1$ranef, col = "steelblue", lwd = 2)
shapiro.test(reduce_mod4.1$ranef)

reduce_mod4.0 <- clmm(teach_total~rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1+(1+ratee)+(1|rater), data=rating, Hess = TRUE)
# Warning message:
#(1) Hessian is numerically singular: parameters are not uniquely determined 
#In addition: Absolute convergence criterion was met, but relative criterion was not met 
summary(reduce_mod4.0) 
AIC(reduce_mod4.0) 
BIC(reduce_mod4.0) 

reduce_mod4.0.1 <- clmm(teach_total~date3+rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1+(1+ratee)+(1|rater), data=rating, Hess = TRUE)
# Warning message:
#(1) Hessian is numerically singular: parameters are not uniquely determined 
#In addition: Absolute convergence criterion was met, but relative criterion was not met 
summary(reduce_mod4.0.1) 
AIC(reduce_mod4.0.1) 
BIC(reduce_mod4.0.1) 

# model comparison
anova(mod4, mod4.1) # time significant
#anova(mod4.1, mod4.0.1) # time not significant
anova(reduce_mod4, reduce_mod4.1) # time significant
anova(reduce_mod4.0, reduce_mod4.0.1) # time significant
anova(reduce_mod4, mod4) # prefer simpler model
anova(reduce_mod4.1, mod4.1) # prefer simpler model
anova(reduce_mod4.0, mod4.0) # prefer complex model
anova(reduce_mod4.0.1, mod4.0.1) # prefer complex model
#anova(mod4, mod4.0) # useful to add ratee random effect
#anova(mod4.1, mod4.0.1) # useful to add ratee random effect
#anova(reduce_mod4, reduce_mod4.0) # not useful to add ratee random effect
#anova(reduce_mod4.1, reduce_mod4.0.1) # not useful to add ratee random effect

# Evaluations of mod4 based on 10-fold cv
tmp_dat = rating[,c(2,9:20,22,25,32)]
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
  tmp.mod <- clmm2(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)
  x = testData[,c(1:14,16)]
  nx <- x[rep(seq_len(nrow(x)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(x))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "teach_total"
  ndat$teach_total <- factor(ndat$teach_total, ordered = TRUE)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5308311
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6756032
cor.test(as.numeric(pred),as.numeric(y)) # 0.3002458 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.2923347
library(caret)
cm <- confusionMatrix(pred, y)
cm

# Evaluations of mod4.0 based on 10-fold cv
tmp_dat = rating[,c(2,9:20,22,25,32)]
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
  tmp.mod <- clmm(teach_total~rater_num2+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater), data=rating, Hess = TRUE)
  x = testData[,c(1:14,16)]
  nx <- x[rep(seq_len(nrow(x)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(x))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "teach_total"
  ndat$teach_total <- factor(ndat$teach_total, ordered = TRUE)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max), ordered = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5623342
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7055703
cor.test(as.numeric(pred),as.numeric(y)) # 0.3804503 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3716945
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns

# Evaluations of mod4.1 based on 10-fold cv
#Warning messages:
#  1: clmm2 may not have converged:
#  optimizer 'ucminf' terminated with max|gradient|: 9.68134281217057e-05 
#2: clmm2 may not have converged:
#  optimizer 'ucminf' terminated with max|gradient|: 650.900912528796 
tmp_dat = rating[,c(2,3,9:20,22,25,31)]
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
  tmp.mod <- clmm2(teach_total~date3+rater_num+fluency_PC1+nfiller_per_token+nrep_per_token+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)
  x = testData[,c(1:15,17)]
  nx <- x[rep(seq_len(nrow(x)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(x))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "teach_total"
  ndat$teach_total <- factor(ndat$teach_total, ordered = TRUE)
  pmat<- matrix(predict(tmp.mod, newdata = ndat), ncol=3, byrow = TRUE)
  pclass <- factor(apply(pmat, 1, which.max), ordered = TRUE)
  index <- testData['ID']
  d = rbind(d, data.frame(index, pclass))
}
d <- d[order(d$ID),]
# evaluation
d$ID <- NULL
pred <- d$pclass
tmp_dat <- tmp_dat[order(tmp_dat$ID),]
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5862069
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.7241379
cor.test(as.numeric(pred),as.numeric(y)) #0.3994408  
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3885023
library(caret)
cm <- confusionMatrix(pred, y)
table(factor(pred, levels=min(y):max(y)),  # rows
      factor(y, levels=min(y):max(y)))  # columns

# Evaluations of mod4.1.0 based on 10-fold cv

# Evaluations of reduce_mod4 based on 10-fold cv
tmp_dat = rating[,c(2,32,10,12,13,15,16,19,20,25)]
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
  tmp.mod <- clmm2(teach_total~rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)
  x = testData[,c(1:9)]
  nx <- x[rep(seq_len(nrow(x)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(x))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "teach_total"
  ndat$teach_total <- factor(ndat$teach_total, ordered = TRUE)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.5630027
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 0.6836461
cor.test(as.numeric(pred),as.numeric(y)) # 0.358995
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 0.3406855
library(caret)
cm <- confusionMatrix(pred, y)
cm

# Evaluations of reduce_mod4.0 based on 10-fold cv
# Evaluations of reduce_mod4.1 based on 10-fold cv
#Warning message:
#  clmm2 may not have converged:
#  optimizer 'ucminf' terminated with max|gradient|: 4469.49501732795 
tmp_dat = rating[,c(2,3,32,10,12,13,15,16,19,20,25)]
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
  tmp.mod <- clmm2(teach_total~date3+rater_num2+nrep_per_token+L7+gcp_confidence+lca_PC1+taales_PC1+taasc_PC3+taaco_PC1, random=ratee, data=trainData, Hess = TRUE)
  x = testData[,c(1:10)]
  nx <- x[rep(seq_len(nrow(x)), each = 3), ]
  ny <- rep(c(1,2,3),nrow(x))
  ndat <- cbind(ny,nx)
  colnames(ndat)[1] <- "teach_total"
  ndat$teach_total <- factor(ndat$teach_total, ordered = TRUE)
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
y <- tmp_dat$teach_total
table(pred)
table(y)
sum(pred==y)/length(y) # 0.538874
library(dplyr)
# for binary classification
pred2 <- recode(pred, '1'=0, '2'=0, '3'=1, '4'=1)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred2==y2)/length(y2) # 
cor.test(as.numeric(pred),as.numeric(y)) # 
library(Metrics)
ScoreQuadraticWeightedKappa(pred,y) # 
library(caret)
cm <- confusionMatrix(pred, y)
cm
# Evaluations of reduce_mod4.1.0 based on 10-fold cv

########################3
# 5. Explanatory IRT
########################
library(eirm)
rating$teach_total <- factor(rating$teach_total, ordered = TRUE)
# reformat the categories: https://github.com/okanbulut/eirm
# level 1 --> 0
# level 2 --> 1
# level 3 or level 4 --> NA
rating_recode <- polyreformat(data=rating, id.var = "ratee", long.format = FALSE, var.name = "rater", val.name = "teach_total")
mod5 <- eirm(formula = "polyresponse ~-1+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater)", data = rating_recode)
print(mod5, difficulty = TRUE)
plot(mod5)

mod5.1 <- eirm(formula = "polyresponse ~-1+date3+nfiller_per_token+nrep_per_token+fluency_PC1+L7+gcp_confidence+rpvi_v+lca_PC1+taales_PC1+sca_PC1+taasc_PC1+taasc_PC3+taaco_PC1+(1|ratee)+(1|rater)", data = rating_recode)
print(mod5.1, difficulty = TRUE)
plot(mod5.1)

reduce_mod5 <- eirm(formula = "polyresponse ~-1+fluency_PC1++rpvi_v+taales_PC1+sca_PC1+(1|ratee)+(1|rater)", data = rating_recode)
print(reduce_mod5, difficulty = TRUE)
plot(reduce_mod5)

reduce_mod5.1 <- eirm(formula = "polyresponse ~-1+date3+fluency_PC1++rpvi_v+taales_PC1+sca_PC1+(1|ratee)+(1|rater)", data = rating_recode)
print(reduce_mod5.1, difficulty = TRUE)
plot(reduce_mod5.1)

# TAM
library(TAM)
library(tidyverse)
oect <- rating[,c(1,2,3,9:20,22,25)]
names(oect)
oect$teach_total <- oect$teach_total - 1
oect_wide <- spread(oect, rater, teach_total)
head(oect_wide)
oect_wide2 <- oect_wide[,c(1,16:37)]
rownames(oect_wide2) <- oect_wide[,1]
oect_wide2 <- oect_wide2[,-1]
library(psych)
describe(oect_wide2) 

features <- oect_wide[,c(4:15)]
features_sub <- oect_wide[,c(5,6,8,11,13,14)]

mod6 <- tam.mml(resp=oect_wide2, Y=features_sub, irtmodel = "RSM")
summary(mod2) 
mod6.0 <-  tam.mml(resp=data.matrix(oect_wide2), irtmodel = "RSM")
anova(mod2.0, mod2) 





