teach_levels_data4 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data4.csv")
results <- read.csv("~/Desktop/TEMP/DISS/Vocabulary/results.csv")
tmp <- merge(teach_levels_data4,results, by="soundname")
cor_dat <- data.frame(cor(tmp[,124:364],tmp$final_teach_level))
colnames(cor_dat) <-'values'
cor_dat$features <- rownames(cor_dat)
str(cor_dat)
cor_dat[order(cor_dat$values),] 
cor_dat1 <- cor_dat[abs(cor_dat$values)>0.20,] # set threshold to 0.20
rownames(cor_dat1) <- seq(1,nrow(cor_dat1))
dim(cor_dat1) # 9 features

library(tidyverse)
taales <- tmp %>% select(cor_dat1$features)
taales$soudnname <- tmp$soundname
taales$final_teach_level <- tmp$final_teach_level
names(taales)
dim(taales)
cor(taales[,2:9]) # a lot of high correlations

taales[,2:9] <- scale(taales[,2:9], center=TRUE, scale=TRUE)
pca <- prcomp(taales[,2:7])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
# [1] 0.9621997 0.9895789 0.9980864 0.9992089 0.9999819 1.0000000
screeplot(pca)
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 1, propn = 0.90, nobs = 151) # 1

loadings <- t(pca$sdev * t(pca$rotation))[,1:2] 
loadings 
#                     PC1         PC2
#KF_Freq_AW          0.9729179 -0.22298601
#KF_Freq_FW          0.9900923  0.07816609
#TL_Freq_FW          0.9909761  0.11986868
#BNC_Written_Freq_AW 0.9688264 -0.23830122
#BNC_Written_Freq_FW 0.9910233  0.08305236
#BNC_Spoken_Freq_FW  0.9713704  0.17432499
# a dominant first factor
specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(taales[,2:7]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# no large off-diagonal elements

# Using the PC scores
taales$PC1 <- pca$x[,1]
cor(taales$PC1,taales$final_teach_level)
#PC1 0.208125
write.csv(taales, "~/Desktop/TEMP/DISS/Vocabulary/taales_selected_PC1.csv")


