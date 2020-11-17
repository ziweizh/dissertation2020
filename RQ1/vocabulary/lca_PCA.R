# Rating level ananlyses
merged_lca <- read.csv("~/Desktop/TEMP/DISS/Vocabulary/merged_lca.csv") # also change to sca_output.csv
dim(merged_lca)

# variable selection using correlation
cor(merged_lca[,5:37], merged_lca$teach_total)
# wordtypes: -0.265994066
# swordtypes: -0.243001475
# wordtokens: -0.296543254
# swordtokens: -0.286026967
# ndw: -0.265994066

lca1 <- merged_lca[,c(2,3,4,5,6,9,10,19)]
head(lca1)
var(lca1[,4:8])

# PCA for the selected variables
lca1[,4:8] <- scale(lca1[,4:8], center=TRUE, scale=TRUE)
pca <- prcomp(lca1[,4:8])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 415) # 5.528416e-13
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 415) # 1
loadings <- t(pca$sdev * t(pca$rotation))[,1:2] 
loadings # high negative loadings on PC1; PCs is about differences b/w types and tokens
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(lca1[,4:8]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# note high correlation b/w wordtypes and swordtypes (0.92) caused issue for factor analysis:singular matrix

# Using the PC scores
pairs(pca$x[ ,c(1,2)],labels=c("PC1", "PC2"),
      panel=function(x,y){panel.smooth(x,y,col = lca1$teach_total)
        abline(lsfit(x,y),lty=2) }) # seemed hard to separate
lca1$PC1 <- pca$x[,1]
lca1$PC2 <- pca$x[,2]
cor(lca1[,c(9,10)],lca1$teach_total)
# PC1  0.30116053
# PC2 -0.09220535
# PC1 may only be useful 

table(lca1$rater)
# remove the raters who only had less than 3 ratings
levels(lca1$rater)[levels(lca1$rater)=="Simpsone"] <- "simpsone"
lca2 <- subset(lca1, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili")
lca2$rater <- droplevels(lca2$rater)
table(lca2$rater)
table(as.factor(lca2$teach_total))
write.csv(lca2, '/Users/mingxi/Desktop/TEMP/DISS/Vocabulary/lca_data.csv')

# add the ratee and date variable
id <- c()
date <- c()
for (i in lca2$soundname) {
  id <- c(id,unlist(strsplit(as.character(i), "_"))[1])
  date <- c(date,unlist(strsplit(as.character(i), "_"))[2])
}
lca2$ratee <- id
lca2$date <- date

# recode the date variable
date2 <- c()
for (i in date) {
  month <- unlist(strsplit(as.character(i), "-"))[1]
  year <- unlist(strsplit(as.character(i), "-"))[3]
  date2 <- c(date2, paste(year, month, sep = "_"))
}
lca2$date2 <- date2

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

lca2$date3 <- date3
lca2$ratee <- as.factor(lca2$ratee)
lca2$date3 <- as.numeric(lca2$date3)
write.csv(lca2, '/Users/mingxi/Desktop/TEMP/DISS/Vocabulary/lca_data2.csv')

# Score level analyses
teach_levels_data3 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data3.csv")
names(teach_levels_data3)
vocab <- teach_levels_data3[,c(2,13,47:79)]
names(vocab)
cor(vocab[,3:35], vocab$final_teach_level)
#wordtypes   -0.40089728
#swordtypes  -0.35853322
#lextypes    -0.25347570
#slextypes   -0.21886084
#wordtokens  -0.45722538
#swordtokens -0.41071970
#lextokens   -0.25962511
#slextokens  -0.16286898
#ld          -0.10829727
#ls1         -0.02050368
#ls2         -0.06156634
#vs1         -0.06001838
#vs2         -0.13678609
#cvs1        -0.12663817
#ndw         -0.40089728
#ndwz         0.10251908
#ndwerz      -0.05377966
#ndwesz      -0.09890285
#ttr          0.19403675
#msttr       -0.08592242
#cttr        -0.17786857
#rttr        -0.17739829
#logttr       0.12187279
#uber        -0.02408373
#lv           0.17158113
#vv1         -0.05713678
#svv1        -0.14030859
#cvv1        -0.16040061
#vv2          0.05756201
#nv           0.14825494
#adjv         0.11356994
#advv        -0.02196263
#modv         0.03232341
vocab1 <- vocab[,c(1,2,3,4,5,6,7,8,9,17)]
names(vocab1)  
cor(vocab1[,c(3:10)]) # wordtypes and ndw are the same, whcih correlates highly with swordtypes
                      # remove ndw and swordtypes
vocab2 <- vocab1[,c(1,2,3,5,6,7,8,9)]
names(vocab2)

# PCA for the selected variables
vocab2[,3:8] <- scale(vocab2[,3:8], center=TRUE, scale=TRUE)
pca <- prcomp(vocab2[,3:8])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
# [1] 0.5605333 0.8258531 0.9075173 0.9558782 0.9842730 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 151) # 4.126971e-24
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 151) # 1.103667e-06
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 151) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:3] 
loadings 
#               PC1        PC2        PC3
#wordtypes   -0.6698763 -0.5160275  0.5014609
#lextypes    -0.8453368  0.4547079  0.1424538
#slextypes   -0.7558430  0.5376644  0.1223135
#wordtokens  -0.7213605 -0.5956994 -0.1410477
#swordtokens -0.7248544 -0.5097124 -0.3323969
#lextokens   -0.7634118  0.4638188 -0.2699754

specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(vocab2[,3:8]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 

# no large off-diagonal elements

# Using the PC scores
pairs(pca$x[ ,c(1,2,3)],labels=c("PC1", "PC2","PC3"),
      panel=function(x,y){panel.smooth(x,y,col = vocab2$final_teach_level)
        abline(lsfit(x,y),lty=2) }) 
vocab2$PC1 <- pca$x[,1]
vocab2$PC2 <- pca$x[,2]
vocab2$PC3 <- pca$x[,3]
cor(vocab2[,c(9:11)],vocab2$final_teach_level)
#PC1 0.4382688
#PC2 0.2105897
#PC3 0.0146788
write.csv(vocab2[,c(1,9)], "~/Desktop/TEMP/DISS/Vocabulary/score_level_PC1.csv")

