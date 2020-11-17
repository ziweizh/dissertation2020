# Rating level analysis

taaco <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/Content/results.csv") 
dim(taaco)
names(taaco)
stats_scoring_RPlat_original <- read.csv("~/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv")
merged_taaco <- merge(taaco, stats_scoring_RPlat_original, by = "soundname")
dim(merged_taaco)
names(merged_taaco)
# variable selection using correlation
cor(merged_taaco[,2:169], merged_taaco$teach_total) # setting threshold to 0.15
# lemma_ttr                                 0.166043834
# function_ttr                              0.190592877
# prp_ttr                                   0.192671119
# argument_ttr                              0.179336809
# addition                                  0.156227977
# all_demonstratives                       -0.151903820

taaco1 <- merged_taaco[,c(1,2,7,13,14,146,154,171,174)]
head(taaco1)
cor(taaco1[,2:6])

# PCA and FA for the selected variables
taaco1[,2:6] <- scale(taaco1[,2:6], center=TRUE, scale=TRUE)
pca <- prcomp(taaco1[,2:6])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 416) # 1.798116e-18
PCs.proportion.variation.enuff(pca$sdev^2, q = 4, propn = 0.90, nobs = 416) # 1
loadings <- t(pca$sdev * t(pca$rotation))[,1:4] 
loadings # PC1 related to the ttr's
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(taaco1[,1:6]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 

# Using the PC scores
pairs(pca$x[ ,c(1,2,3,4)],labels=c("PC1", "PC2", "PC3", "PC4"),
      panel=function(x,y){panel.smooth(x,y,col = taaco1$teach_total)
        abline(lsfit(x,y),lty=2) }) # seemed hard to separate
taaco1$PC1 <- pca$x[,1]
taaco1$PC2 <- pca$x[,2]
taaco1$PC3 <- pca$x[,3]
taaco1$PC4 <- pca$x[,4]

cor(taaco1[,c(8:11)],taaco1$teach_total)
#PC1 -0.21391914
#PC2  0.20750572
#PC3  0.01959727
#PC4 -0.04917092 

table(taaco1$rater)
# remove the raters who only had less than 5 ratings
levels(taaco1$rater)[levels(taaco1$rater)=="Simpsone"] <- "simpsone"
taaco2 <- subset(taaco1, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili" & rater!="phuongn")
taaco2$rater <- droplevels(taaco2$rater)
table(taaco2$rater)
table(as.factor(taaco2$teach_total))

write.csv(taaco, '/Users/mingxi/Desktop/TEMP/DISS/Content/taaco_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in taaco2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
taaco2$ratee <- id
taaco2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
taaco2$date2 <- date2

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

taaco2$date3 <- date3
taaco2$ratee <- as.factor(taaco2$ratee)
taaco2$date3 <- as.numeric(taaco2$date3)
write.csv(taaco2, '/Users/mingxi/Desktop/TEMP/DISS/Content/taaco_data2.csv')


# Score level analysis
teach_levels_data4 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data4.csv")
names(teach_levels_data4)
taaco3 <- merge(teach_levels_data4, taaco, by = "soundname")
names(taaco3)
taaco4 <- taaco3[,c(1,12,124:291)]
names(taaco4)
cor(taaco4[,3:170], taaco4$final_teach_level) # set the treshold to 0.2
# lemma_ttr                                 0.2398543554
# content_ttr                               0.2132947602
# function_ttr                              0.2283131199
# adv_ttr                                   0.2318427416
# prp_ttr                                   0.2107329463
# argument_ttr                              0.2179001613
# lsa_2_all_sent                           -0.2010086387

taaco5 <- taaco4[,c(1,2,3,7,8,13,14,15,131)]
names(taaco5)
cor(taaco5[,c(3:9)]) 

# PCA for the selected variables
taaco5[,3:9] <- scale(taaco5[,3:9] , center=TRUE, scale=TRUE)
pca <- prcomp(taaco5[,3:9] )
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 151) # 1.067572e-06
PCs.proportion.variation.enuff(pca$sdev^2, q = 4, propn = 0.90, nobs = 151) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:4] 
loadings 
#                 PC1         PC2         PC3         PC4
#lemma_ttr      -0.9647311  0.06260163 -0.05026440  0.18350503
#content_ttr    -0.9075475  0.23161864 -0.20602208  0.15692682
#function_ttr   -0.7868740 -0.26802912  0.20677460  0.09135556
#adv_ttr        -0.4798791 -0.25654239 -0.72038448 -0.42597726
#prp_ttr        -0.4940808 -0.62679861  0.45962823 -0.29734136
#argument_ttr   -0.8794740  0.08866891  0.05859503  0.21769437
#lsa_2_all_sent  0.4493549 -0.65677038 -0.31118398  0.51510551
# PC1 is the common PC for ttr's

specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(taaco5[,3:9]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# no large off-diagonal elements

# Using the PC scores
pairs(pca$x[ ,c(1,2,3,4)],labels=c("PC1", "PC2","PC3", "PC4"),
      panel=function(x,y){panel.smooth(x,y,col = taaco5$final_teach_level)
        abline(lsfit(x,y),lty=2) }) 
taaco5$PC1 <- pca$x[,1]
taaco5$PC2 <- pca$x[,2]
taaco5$PC3 <- pca$x[,3]
taaco5$PC4 <- pca$x[,4]
cor(taaco5[,c(10:13)],taaco5$final_teach_level)
#PC1 -0.288221493
#PC2 -0.036019820
#PC3 -0.003952024
#PC4 -0.183569620

write.csv(taaco5[,c(1,10)], "~/Desktop/TEMP/DISS/Content/taaco_score_level_PC1.csv")

