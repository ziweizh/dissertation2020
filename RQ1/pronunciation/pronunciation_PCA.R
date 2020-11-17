# rating levle analyses

merged_pron <- read.csv("~/Desktop/TEMP/DISS/Pronunciation/merged_pronunciation2.csv") 
dim(pron)
names(pron)

# variable selection using correlation
cor(pron[,6:12], merged_pron$teach_total)
#[,1]
#L1 -0.08165906
#L2  0.15427783
#L3  0.16895325
#L4 -0.07475294
#L5  0.15143334
#L6  0.17390104
#L7  0.24516276

pron[,6:12] <- scale(pron[,6:12], center=TRUE, scale=TRUE)
pca <- prcomp(pron[,6:12])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
# [1] 0.7433167 0.9708301 0.9901967 0.9975562 0.9991622 0.9999135 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 418) # 3.719379e-19
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 418) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:2] 
loadings 
#       PC1         PC2
#L1 -0.6034466 -0.78861473
#L2 -0.9780081  0.11417443
#L3 -0.9799317  0.09355806
#L4 -0.6628784 -0.73503442
#L5 -0.9457595  0.19092973
#L6 -0.9782478  0.10375115
#L7 -0.7946482  0.6011633
# PC1 common factor, but PC2 is hard to interpret
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(pron[,6:12]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 

# Using the PC scores
pairs(pca$x[ ,c(1,2)],labels=c("PC1", "PC2"),
      panel=function(x,y){panel.smooth(x,y,col = fluency1$teach_total)
        abline(lsfit(x,y),lty=2) }) # red and green may be separabale
pron$PC1 <- pca$x[,1]
pron$PC2 <- pca$x[,2]
cor(pron[,c(13:14)],pron$teach_total)
#PC1 -0.1394857
#PC2  0.2179486

table(pron$rater)
# remove the raters who only had less than 3 ratings
levels(pron$rater)[levels(pron$rater)=="Simpsone"] <- "simpsone"
pron2 <- subset(pron, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili")
pron2$rater <- droplevels(pron2$rater)
table(pron2$rater)
table(as.factor(pron2$teach_total))

write.csv(pron2, '/Users/mingxi/Desktop/TEMP/DISS/Pronunciation/pron_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in pron2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
pron2$ratee <- id
pron2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
pron2$date2 <- date2

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

pron2$date3 <- date3
pron2$ratee <- as.factor(pron2$ratee)
pron2$date3 <- as.numeric(pron2$date3)
write.csv(pron2, '/Users/mingxi/Desktop/TEMP/DISS/Pronunciation/pron_data2.csv')

# score level analyses
teach_levels_data3 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data3.csv")
names(teach_levels_data3)
pron <- teach_levels_data3[,c(2,13,29:36,107)]
names(pron)
cor(pron[,c(3:11)], pron$final_teach_level)
#L1             -0.1525841
#L2              0.2765972
#L3              0.2711786
#L4             -0.1352746
#L5              0.2783051
#L6              0.2744125
#L7              0.3973271
#gcp_confidence -0.3154505
#wer3            0.1323639
cor(pron[,c(4,5,7,8,9,10)]) # L2, L3, L5, L6, L7 highly correlated --> L7 selected, which correlated weakly with GCP score



