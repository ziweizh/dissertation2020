# Rating level analyses
merged_rhythm <- read.csv("~/Desktop/TEMP/DISS/Prosody/merged_rhythm2.csv") 
dim(merged_rhythm)
names(merged_rhythm)

# variable selection using correlation
cor(merged_rhythm[,6:15], merged_rhythm$teach_total)
# X.delta_v    0.169777275
# X.rpvi_v     0.165435774
# X.npvi_v     0.125735674

rhythm1 <- merged_rhythm[,c(2,3,4,7,9,10)]
head(rhythm1)
cor(rhythm1[,4:6])

# PCA and FA for the selected variables
rhythm1[,4:6] <- scale(rhythm1[,4:6], center=TRUE, scale=TRUE)
pca <- prcomp(rhythm1[,4:6] )
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 415) # 0.007987979
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 415) # 1
loadings <- t(pca$sdev * t(pca$rotation))[,1:2] 
loadings # PC1 related to all delta_v, rpvi_v, and npvi_v
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(rhythm1[,4:6]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 

# Using the PC scores
pairs(pca$x[ ,c(1,2)],labels=c("PC1", "PC2"),
      panel=function(x,y){panel.smooth(x,y,col = rhythm1$teach_total)
        abline(lsfit(x,y),lty=2) }) # red and green may be separabale
rhythm1$PC1 <- pca$x[,1]
rhythm1$PC2 <- pca$x[,2]
cor(rhythm1[,c(7:8)],rhythm1$teach_total)
# PC1 -0.16483334
# PC2 -0.04374455

table(rhythm1$rater)
# remove the raters who only had less than 3 ratings
levels(rhythm1$rater)[levels(rhythm1$rater)=="Simpsone"] <- "simpsone"
rhythm2 <- subset(rhythm1, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili")
rhythm2$rater <- droplevels(rhythm2$rater)
table(rhythm2$rater)
table(as.factor(rhythm2$teach_total))

write.csv(rhythm2, '/Users/mingxi/Desktop/TEMP/DISS/Prosody/rhythm_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in rhythm2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
rhythm2$ratee <- id
rhythm2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
rhythm2$date2 <- date2

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

rhythm2$date3 <- date3
rhythm2$ratee <- as.factor(rhythm2$ratee)
rhythm2$date3 <- as.numeric(rhythm2$date3)
write.csv(rhythm2, '/Users/mingxi/Desktop/TEMP/DISS/Prosody/rhythm_data2.csv')

# Score level analyses
teach_levels_data3 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data3.csv")
names(teach_levels_data3)
rhythm <- teach_levels_data3[,c(2,13,37:46)]
names(rhythm)
cor(rhythm[,3:10], rhythm$final_teach_level)
#percent_v  0.07516195
#delta_v    0.22187876
#varco_v    0.10523062
#rpvi_v     0.21370350
#npvi_v     0.16193919
#percent_c -0.07516195
#delta_c   -0.01168055
#varco_c   -0.03772254
# delta_v and rpvi_v are selected
rhythm1 <- rhythm[,c(1,2,4,6)]
names(rhythm1)  
cor(rhythm1[,c(3:4)])
# As the selected feature are highly correlated rpvi_v is selected
