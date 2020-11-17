# Rating level analysis

merged_sca <- read.csv("~/Desktop/TEMP/DISS/Grammar/merged_sca.csv") 
dim(merged_sca)

# variable selection using correlation
cor(merged_lca[,5:25], merged_sca$teach_total)
# W    -0.301782792
# S    -0.227011898
# VP   -0.272087017
# C    -0.269877235
# T    -0.211932833
# DC   -0.250606967
# CT   -0.208578020
# CN   -0.297432267

sca1 <- merged_sca[,c(2,3,4,5,6,7,8,9,10,11,13)]
head(sca1)
var(sca1[,4:11])

# PCA and FA for the selected variables
sca1[,4:11] <- scale(sca1[,4:11], center=TRUE, scale=TRUE)
pca <- prcomp(sca1[,4:11])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 415) # 0.0001159222
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 415) # 1
loadings <- t(pca$sdev * t(pca$rotation))[,1:3] 
loadings # high negative loadings on PC1; PC2 and 3 do seem to be very important
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(sca1[,4:11]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# note high correlation b/w wordtypes and swordtypes (0.92) caused issue for factor analysis:singular matrix

# Using the PC scores
pairs(pca$x[ ,c(1,2,3)],labels=c("PC1", "PC2", "PC3"),
      panel=function(x,y){panel.smooth(x,y,col = sca1$teach_total)
        abline(lsfit(x,y),lty=2) }) # seemed hard to separate
sca1$PC1 <- pca$x[,1]
sca1$PC2 <- pca$x[,2]
sca1$PC3 <- pca$x[,3]
cor(sca1[,c(12:14)],sca1$teach_total)
# PC1  0.30312256
# PC2 -0.03861455
# PC3  0.10495294
# PC1 may only be useful 

table(sca1$rater)
# remove the raters who only had less than 3 ratings
levels(sca1$rater)[levels(sca1$rater)=="Simpsone"] <- "simpsone"
sca2 <- subset(sca1, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili")
sca2$rater <- droplevels(sca2$rater)
table(sca2$rater)
table(as.factor(sca2$teach_total))

write.csv(sca2, '/Users/mingxi/Desktop/TEMP/DISS/Grammar/sca_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in sca2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
sca2$ratee <- id
sca2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
sca2$date2 <- date2

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

sca2$date3 <- date3
sca2$ratee <- as.factor(sca2$ratee)
sca2$date3 <- as.numeric(sca2$date3)
write.csv(sca2, '/Users/mingxi/Desktop/TEMP/DISS/Grammar/sca_data2.csv')

# Score level analysis
teach_levels_data3 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data3.csv")
names(teach_levels_data3)
grammar <- teach_levels_data3[,c(2,12,82:104)]
names(grammar)
cor(grammar[,3:25], grammar$final_teach_level)
#W    -0.46086593
#S    -0.29610571
#VP   -0.39093913
#C    -0.37634428
#T    -0.26516433
#DC   -0.36161082
#CT   -0.33744705
#CP   -0.07911479
#CN   -0.44233984
#MLS  -0.09824134
#MLT  -0.14681684
#MLC  -0.03384574
#C.S  -0.08762136
#VP.T -0.10684771
#C.T  -0.14573817
#DC.C -0.20009510
#DC.T -0.19251411
#T.S   0.17015336
#CT.T -0.14141991
#CP.T  0.01457277
#CP.C  0.07601995
#CN.T -0.16652545
#CN.C -0.11895584

grammar1 <- grammar[,c(1,2,4,5,6,7,8,9,11,18,19)]
names(grammar1)
cor(grammar1[,c(3:11)]) # S is highly correlated with T; VP is correlated with C; DC.C is correlated with DC.T 

grammar2 <- grammar1[,c(1,2,4,6,7,8,9,10)]
names(grammar2)

# PCA for the selected variables
grammar2[,3:8] <- scale(grammar2[,3:8], center=TRUE, scale=TRUE)
pca <- prcomp(grammar2[,3:8])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
#[1] 0.5989609 0.8669470 0.9523064 0.9796436 0.9961017 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 151) #2.882549e-19
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 151) # 0.007381226
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 151) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:3] 
loadings 
#       PC1         PC2         PC3
#VP   0.8743588 -0.37890941  0.09064953
#T    0.5445562 -0.79794350  0.15505599
#DC   0.9165300  0.34488465  0.12223535
#CT   0.9172868  0.09559168  0.21206738
#CN   0.7530298 -0.10653497 -0.64726192
#DC.C 0.5331272  0.82957649  0.03217169

specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(grammar2[,3:8]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# no large off-diagonal elements

# Using the PC scores
pairs(pca$x[ ,c(1,2,3)],labels=c("PC1", "PC2","PC3"),
      panel=function(x,y){panel.smooth(x,y,col = grammar2$final_teach_level)
        abline(lsfit(x,y),lty=2) }) 
grammar2$PC1 <- pca$x[,1]
grammar2$PC2 <- pca$x[,2]
grammar2$PC3 <- pca$x[,3]
cor(grammar2[,c(9:11)],grammar2$final_teach_level)
#PC1 -0.4360196
#PC2  0.0521644
#PC3  0.1709544
write.csv(grammar2[,c(1,9)], "~/Desktop/TEMP/DISS/Grammar/score_level_PC1.csv")

