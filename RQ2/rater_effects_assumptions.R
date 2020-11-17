library(TAM)
library(tidyverse)
library(psych)
library(ggplot2)

# loading and preprocessing the rating data
scoring <- read.csv("~/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv")
names(scoring)
dim(scoring)
table(scoring$rater)
levels(scoring$rater)[levels(scoring$rater)=="Simpsone"] <- "simpsone"
scoring <- subset(scoring, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili" & rater != "phuongn"& rater != "gcakinci")
scoring$rater <- droplevels(scoring$rater)
table(scoring$rater)
table(scoring$teach_total)
scoring <- scoring[scoring$teach_total!=4,]
table(scoring$teach_total)
scoring$teach_total <- recode(scoring$teach_total, '1'=3, '2'=2, '3'=1) # levels --> categories
scoring$rater_num <- as.factor(as.numeric(scoring$rater))
str(scoring)
# reformat to wide
oect <- scoring %>% select(soundname1,rater_num,teach_total)
oect$teach_total <- oect$teach_total - 1
oect_wide <- spread(oect, rater_num, teach_total)
head(oect_wide)
oect_wide2 <- oect_wide[,-1]
rownames(oect_wide2) <- oect_wide[,1]
describe(oect_wide2) 

##################################################
## Local Independence: Edwards, Hous, & Cai (2018)
##################################################
# compute JSI
rsm <- tam.mml(resp=data.matrix(oect_wide2), irtmodel = "RSM")
jsi <- data.frame(matrix(NA, nrow = 21, ncol = 1))
for (i in seq(1,21)) {
  rsm2 <- tam.mml(resp=data.matrix(oect_wide2[,-i]), irtmodel = "RSM")
  jsi_k <- data.frame((rsm$xsi[-i,][1]-rsm2$xsi[1])/rsm2$xsi[2])
  jsi <- cbind(jsi, cbind(rownames(jsi_k),jsi_k))
}
dim(jsi)
write.csv(jsi,'/Users/mingxi/Desktop/TEMP/DISS/Model/rater_effects_independence.csv')
# Conclusion, LI seems to hold given the assumption for the re-taking students

##############################
## Unidimensionality
# https://stats.stackexchange.com/questions/35561/imputation-of-missing-values-for-pca
# https://www.rasch.org/rmt/rmt122m.htm
###############################
res_rsm <- IRT.residuals(rsm)
stdresid_rsm<- data.frame(res_rsm$stand_residuals)
library(missMDA)
resid.imp <- imputePCA(stdresid_rsm,method="EM",ncp=1)
resid.pca <- prcomp(resid.imp$completeObs)
resid.pca
cumsum(resid.pca$sdev^2)/sum(resid.pca$sdev^2) # obvious dominant PC1, indicating still prominent structure in standardized residuals, which are not accounted for
screeplot(resid.pca) 
cum.var.percent <- cumsum(resid.pca$sdev^2)/sum(resid.pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
loadings <- t(resid.pca$sdev * t(resid.pca$rotation))[,1:21] 
loadings # loadings on PC2 are negligible, except for X8

# let's generate a random matrix of the same size
dim(resid.imp$completeObs)
r <- replicate(21, rnorm(164)) 
rand.pca <- prcomp(r)
cumsum(rand.pca$sdev^2)/sum(rand.pca$sdev^2) # really no pattern
screeplot(rand.pca)
cum.var.percent <- cumsum(resid.pca$sdev^2)/sum(resid.pca$sdev^2)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
rand.loadings <- t(rand.pca$sdev * t(rand.pca$rotation))[,1:21] # let's select 10 PCs 
rand.loadings # no obvious patterns
# Conlusion, possible violation of unidimensionality assumption, but possible caused by the large numeber of imputed values


