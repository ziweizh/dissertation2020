teach_levels_data4 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data4.csv")
results <- read.csv("~/Desktop/TEMP/DISS/Grammar/results.csv")
tmp <- merge(teach_levels_data4,results, by="soundname")
tmp <- tmp[,apply(tmp,2,function(tmp) !all(tmp==0))]
names(tmp)
cor_dat <- data.frame(cor(tmp[,123:457],tmp$final_teach_level))
colnames(cor_dat) <-'values'
cor_dat$features <- rownames(cor_dat)
str(cor_dat)
cor_dat[order(cor_dat$values),] 
cor_dat1 <- cor_dat[abs(cor_dat$values)>0.20,] # set threshold to 0.20
rownames(cor_dat1) <- seq(1,nrow(cor_dat1))
dim(cor_dat1) # 21 features

library(tidyverse)
taasc <- tmp %>% select(cor_dat1$features)
taasc$soudnname <- tmp$soundname
taasc$final_teach_level <- tmp$final_teach_level
names(taasc)
taasc <- taasc[,-1] # remove nwords
dim(taasc)
cor(taasc[,1:20]) # a lot of high correlations
library(tidyverse)
findCorrelation(taasc[,1:20], cutoff = .9, exact = FALSE) # extract the highly correlated features

# PCA
taasc[,1:20] <- scale(taasc[,1:20], center=TRUE, scale=TRUE)
pca <- prcomp(taasc[,1:20])
pca
cumsum(pca$sdev^2)/sum(pca$sdev^2)
#[1] 0.6295467 0.7940874 0.8498547 0.8995528 0.9367726 0.9551456 0.9691808 0.9787677 0.9864947 0.9925834
#[11] 0.9961370 0.9981252 0.9991779 0.9996085 0.9997974 0.9998877 0.9999370 0.9999749 0.9999991 1.0000000
cum.var.percent <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
library(tidyverse)
cbind.data.frame(percent=cum.var.percent, PC=1:length(cum.var.percent)) %>% ggplot(aes(x=PC,y=percent))+geom_point(size=.5)+geom_line()
source("/Users/mingxi/Desktop/TEMP/stat_501/PCA/PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(pca$sdev^2, q = 2, propn = 0.90, nobs = 151) # 5.535382e-10
PCs.proportion.variation.enuff(pca$sdev^2, q = 3, propn = 0.90, nobs = 151) # 6.378765e-05 
PCs.proportion.variation.enuff(pca$sdev^2, q = 4, propn = 0.90, nobs = 151) # 0.4800776

loadings <- t(pca$sdev * t(pca$rotation))[,1:4] 
loadings 

specific.var = diag(1 - loadings %*% t(loadings)) 
specific.var
resid.var <- round(cor(taasc[,1:20]) - loadings %*% t(loadings) - diag(specific.var), 4)
resid.var 
# no large off-diagonal elements

# Using the PC scores
taasc$PC1 <- pca$x[,1]
taasc$PC2 <- pca$x[,2]
taasc$PC3 <- pca$x[,3]
taasc$PC4 <- pca$x[,4]
cor(taasc[,23:26],taasc$final_teach_level)
#PC1 0.26421217
#PC2 0.14439836
#PC3 0.25694317
#PC4 0.04368439
write.csv(taasc, "~/Desktop/TEMP/DISS/Grammar/taasc_selected_PC.csv")


