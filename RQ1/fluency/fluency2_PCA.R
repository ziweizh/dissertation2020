# 1. Rating level analysis
merged_fluency <- read.csv("~/Desktop/TEMP/DISS/Fluency/merged_fluency2+lca.csv") 
dim(merged_fluency)
names(merged_fluency)

# variable selection using correlation
cor(merged_fluency[,4:18], merged_fluency$teach_total)
# the normalized versions showed higher correlations
#npause_limit         0.07171902
#nfillers             0.11919153
#nrep                 0.27361356
#mlr                 -0.17371245
#tokens_per_sec      -0.24188962
#tokens_per_sec2     -0.09137354
#words_per_sec       -0.17448568
#words_per_sec2       0.03405979
#nsyll_per_sec_p2tk  -0.25782309
#ASD_p2tk             0.26803712
#nsyll_per_sec2_p2tk -0.13622973
#ASD2_p2tk            0.14103785
#npause_per_token     0.30254457
#nfiller_per_token    0.19539787
#nrep_per_token       0.37163279
# these correlations made sense: e.g. faster/more fluent related to higher proficiency; more choppy/slower speech related to lower proficiency 

fluency1 <- merged_fluency[,c(1,2,3,7,8,12,13,16,17,18)]
head(fluency1)
cor(fluency1[,4:10]) # tokens_per_sec, nsyll_per_sec_p2tk, and ASD_p2tk are highly correlated
                     # *** only ASD_p2tk are kept due to 1) slightly higher correlation with teach_total; 2) purer measure of speed dimension


# PCA and FA for the selected variables
fluency1[,4:10] <- scale(fluency1[,4:10], center=TRUE, scale=TRUE)
pca <- prcomp(fluency1[,4:10])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
#[1] 0.5863717 0.7849348 0.9014540 0.9737374 0.9899999 0.9972216 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 418) # 7.149165e-59
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 418) # 1.774155e-22
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 418) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:3] 
loadings 
#                     PC1        PC2         PC3
#mlr                -0.7133835  0.2875940  0.58350441
#tokens_per_sec     -0.8810123 -0.3902915 -0.12783974
#nsyll_per_sec_p2tk -0.9073180 -0.3646089 -0.14896208
#ASD_p2tk            0.8853341  0.3913659  0.17808706
#npause_per_token    0.8858458 -0.2813176 -0.27319148
#nfiller_per_token   0.5444563 -0.6125730  0.08618628
#nrep_per_token      0.3623612 -0.6437480  0.56819660
#Interpretation: PC1-slowness?; P2-repair?; PC3-density?
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(fluency1[,4:10]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# varimax rotation may be needed?

# Using the PC scores
pairs(pca$x[ ,c(1,2,3)],labels=c("PC1", "PC2","PC3"),
      panel=function(x,y){panel.smooth(x,y,col = fluency1$teach_total)
        abline(lsfit(x,y),lty=2) }) # red and green may be separabale
fluency1$PC1 <- pca$x[,1]
fluency1$PC2 <- pca$x[,2]
fluency1$PC3 <- pca$x[,3]
cor(fluency1[,c(11:13)],fluency1$teach_total)
#PC1  0.3209373
#PC2 -0.1443877
#PC3  0.1974530
# may only keep PC1

table(fluency1$rater)
# remove the raters who only had less than 3 ratings
levels(fluency1$rater)[levels(fluency1$rater)=="Simpsone"] <- "simpsone"
fluency2 <- subset(fluency1, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili")
fluency2$rater <- droplevels(fluency2$rater)
table(fluency2$rater)
table(as.factor(fluency2$teach_total))

write.csv(fluency2, '/Users/mingxi/Desktop/TEMP/DISS/Fluency/fluency_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in fluency2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
fluency2$ratee <- id
fluency2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
fluency2$date2 <- date2

library(dplyr)
date3 <- recode(date2, 
                '13_7'='1', '13_8'='1',
                '13_12'='2', '14_1'='2',
                '14_4'='3','14_5'='3',
                '14_7'='4','14_8'='4',
                '14_12'='5','15_1'='5',
                '15_4'='6','15_5'='6',
                '15_7'='7','15_8'='7',
                '15_12'='8','16_1'='8',
                '16_4'='9','16_5'='9',
                '16_7'='10','16_8'='13',
                '16_12'='11','17_1'='11',
                '17_4'='12','17_5'='12',
                '17_7'='13','17_8'='13',
                '17_12'='14','18_1'='14',
                '18_4'='15','18_5'='15',
                '18_7'='16','18_8'='16',
                '18_12'='17','19_1'='17',
                '19_4'='18','19_5'='18',
                '19_7'='19','19_8'='19',
                '19_12'='20','20_1'='20')

fluency2$date3 <- date3
fluency2$ratee <- as.factor(fluency2$ratee)
fluency2$date3 <- as.numeric(fluency2$date3)
write.csv(fluency2, '/Users/mingxi/Desktop/TEMP/DISS/Fluency/fluency_data2.csv')

# Score levle analysis
teach_levels_data3 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data3.csv")
names(teach_levels_data3)
fluency <- teach_levels_data3[,c(2,13,14:28)]
names(fluency)
cor(fluency[,3:17], fluency$final_teach_level)
fluency1 <- fluency[,c(1,2,6,7,9,11,12,15:17)]
names(fluency1)  
cor(fluency1[,c(3:10)]) # tokens_per_sec, nsyll_per_sec, and ASD are highly corrlated; 
                        # mlr highly correlated with npause_per_token
# PCA for the selected variables
fluency1[,3:10] <- scale(fluency1[,3:10], center=TRUE, scale=TRUE)
pca <- prcomp(fluency1[,3:10])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
#[1] 0.5508123 0.7515682 0.8548625 0.9205637 0.9767722 0.9909067 0.9974693 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 151) # 1.852395e-27
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 151) # 2.008484e-13
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 151) # 0.0003070015
PCs.proportion.variation.enuff(pca$sdev^2, q = 4, propn = 0.90, nobs = 151) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:4] 
loadings 

#                      PC1        PC2         PC3         PC4
#mlr                -0.6718778  0.4128597  0.54765401 -0.06045611
#tokens_per_sec     -0.9004201 -0.2999215 -0.02375874 -0.06496296
#words_per_sec      -0.6134050 -0.4860539 -0.26340599  0.28268772
#nsyll_per_sec_p2tk -0.9279669 -0.2891146 -0.04845277 -0.11185298
#ASD_p2tk            0.9090238  0.3111584  0.07542312  0.10948731
#npause_per_token    0.8496730 -0.3938071 -0.25149954  0.03942219
#nfiller_per_token   0.5126961 -0.6026487  0.20186895 -0.53286100
#nrep_per_token      0.3095289 -0.6408718  0.58689232  0.35753156

specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(fluency1[,3:10]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# no large off-diagonal elements

# Using the PC scores
pairs(pca$x[ ,c(1,2,3,4)],labels=c("PC1", "PC2","PC3", "PC4"),
      panel=function(x,y){panel.smooth(x,y,col = fluency1$final_teach_level)
        abline(lsfit(x,y),lty=2) }) # red and green may be separabale
fluency1$PC1 <- pca$x[,1]
fluency1$PC2 <- pca$x[,2]
fluency1$PC3 <- pca$x[,3]
fluency1$PC4 <- pca$x[,4]
cor(fluency1[,c(11:14)],fluency1$final_teach_level)
#PC1  0.4888531
#PC2 -0.1437953
#PC3  0.1520698
#PC4  0.1869134
write.csv(fluency1[,c(1,11)], "~/Desktop/TEMP/DISS/Fluency/score_level_PC1.csv")

