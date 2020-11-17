# get perdictions and evaluate performance on test set (10 cv)
all_acc1_blup <- c()
all_acc1 <- c()
all_acc2_blup <- c()
all_acc2 <- c()
all_cor_blup <- c()
all_cor <- c()
all_kappa_blup <- c()
all_kappa <- c()

for (i in seq(1,10)){
  sas <- read.csv(paste("/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/reduce_mod5.1.0_pred", i, ".csv", sep=""))
  testdata <- read.csv(paste("/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/10-fold_data/test.", i, ".csv", sep=""))
  testcases <- sas[sas$ID %in% testdata$ID,]
  test <- read.csv(paste("/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/10-fold_data/test.", i, ".csv", sep=""))
  y <- test$teach_total
  
  level1 <- subset(testcases,X_LEVEL_==1)
  level2 <- subset(testcases,X_LEVEL_==2) 
  
  pred_score_blup <- c()
  pred_score <- c()
  for (j in seq(1:dim(level1)[1])) {
    l1_blup <- level1$PredProb[j]
    l1 <- level1$PredProb_PA[j]
    l2_blup <- level2$PredProb[j]-level1$PredProb[j]
    l2 <- level2$PredProb_PA[j]-level1$PredProb_PA[j]
    l3_blup <- 1-level2$PredProb[j]
    l3 <- 1-level2$PredProb_PA[j]
    #pred_score_blup <- c(pred_score_blup, which(as.vector(rmultinom(1, 1, prob = c(l1_blup,l2_blup,l3_blup))) == 1))
    pred_score_blup <- c(pred_score_blup, which.max(list(l1_blup, l2_blup, l3_blup)))
    #pred_score <- c(pred_score, which(as.vector(rmultinom(1, 1, prob = c(l1,l2,l3))) == 1))
    pred_score <- c(pred_score, which.max(list(l1, l2, l3)))
    
  }
  
  # get accuracy1
  acc1_blup <- sum(pred_score_blup==y)/dim(level1)[1]
  cat('iter', i, 'accuracy1 for blup:', acc1_blup, '\n')
  all_acc1_blup <- c(all_acc1_blup, acc1_blup)
  acc1 <- sum(pred_score==y)/dim(level1)[1]
  cat('iter', i, 'accuracy1 for average:', acc1, '\n')
  all_acc1 <- c(all_acc1, acc1)
  
  # get accuracy2
  library(dplyr)
  # for binary classification
  y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
  pred_score_blup2 <- recode(pred_score_blup, '1'=0, '2'=0, '3'=1, '4'=1)
  pred_score2 <- recode(pred_score, '1'=0, '2'=0, '3'=1, '4'=1)
  acc2_blup <- sum(pred_score_blup2==y2)/dim(level1)[1]
  cat('iter', i, 'accuracy2 for blup:', acc2_blup, '\n')
  all_acc2_blup <- c(all_acc2_blup, acc2_blup)
  acc2 <- sum(pred_score2==y2)/dim(level1)[1]
  cat('iter', i, 'accuracy2 for average:', acc2, '\n')
  all_acc2 <- c(all_acc2, acc2)
  
  # get correlation
  cor_blup <- cor(as.numeric(pred_score_blup), as.numeric(y))
  cat('iter', i, 'correlation for blup:', cor_blup, '\n')
  all_cor_blup <- c(all_cor_blup, cor_blup)
  cor <- cor(as.numeric(pred_score), as.numeric(y))
  cat('iter', i, 'correlation for average:', cor, '\n')
  all_cor <- c(all_cor, cor)
  
  #get kappa
  library(Metrics)
  kappa_blup <- ScoreQuadraticWeightedKappa(pred_score_blup,y)
  cat('iter', i, 'kappa for blup:', kappa_blup, '\n')
  all_kappa_blup <- c(all_kappa_blup, kappa_blup)
  kappa <- ScoreQuadraticWeightedKappa(pred_score,y)
  cat('iter', i, 'kappa for average:', kappa, '\n')
  all_kappa <- c(all_kappa, kappa)
  
}

mean(all_acc1_blup)
mean(all_acc1)
mean(all_acc2_blup)
mean(all_acc2)
mean(all_cor_blup)
mean(all_cor)
mean(all_kappa_blup)
mean(all_kappa)

# get predictions on training set and evaluate performance on training
sas <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/reduce_mod5.1.0_pred_train.csv")
rating <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/rating_data.csv")
y <- rating$teach_total

level1 <- subset(sas,X_LEVEL_==1)
level2 <- subset(sas,X_LEVEL_==2) 

pred_score_blup <- c()
pred_score <- c()
for (j in seq(1:dim(level1)[1])) {
  l1_blup <- level1$PredProb[j]
  l1 <- level1$PredProb_PA[j]
  l2_blup <- level2$PredProb[j]-level1$PredProb[j]
  l2 <- level2$PredProb_PA[j]-level1$PredProb_PA[j]
  l3_blup <- 1-level2$PredProb[j]
  l3 <- 1-level2$PredProb_PA[j]
  #pred_score_blup <- c(pred_score_blup, which(as.vector(rmultinom(1, 1, prob = c(l1_blup,l2_blup,l3_blup))) == 1))
  pred_score_blup <- c(pred_score_blup, which.max(list(l1_blup, l2_blup, l3_blup)))
  #pred_score <- c(pred_score, which(as.vector(rmultinom(1, 1, prob = c(l1,l2,l3))) == 1))
  pred_score <- c(pred_score, which.max(list(l1, l2, l3)))
}

sum(pred_score_blup==y)/dim(level1)[1]
sum(pred_score==y)/dim(level1)[1]

library(dplyr)
y2 <- recode(y, '1'=0, '2'=0, '3'=1, '4'=1)
pred_score_blup2 <- recode(pred_score_blup, '1'=0, '2'=0, '3'=1, '4'=1)
pred_score2 <- recode(pred_score, '1'=0, '2'=0, '3'=1, '4'=1)
sum(pred_score_blup2==y2)/dim(level1)[1]
sum(pred_score2==y2)/dim(level1)[1]

cor(as.numeric(pred_score_blup), as.numeric(y))
cor(as.numeric(pred_score), as.numeric(y))

library(Metrics)
ScoreQuadraticWeightedKappa(pred_score_blup,y)
ScoreQuadraticWeightedKappa(pred_score,y)


