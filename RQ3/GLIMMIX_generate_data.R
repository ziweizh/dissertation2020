# 10-fold_data_na: DVs are set to missing value for all test sets and train and test are concatedate to send to SAS
# 10-fold_data: DVs are not set to missing value for all test sets

rating <- read.csv("~/Desktop/TEMP/DISS/rating_data.csv")

#Randomly shuffle the data
rating$ID <- seq.int(nrow(rating))
set.seed(123)
rating<-rating[sample(nrow(rating)),] # note here the rows are shuffled
#Create 10 equally size folds
folds <- cut(seq(1,nrow(rating)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation for the pre.odds model
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- rating[testIndexes, ]
  testData$teach_total <- ''
  trainData <- rating[-testIndexes, ]
  trainData$teach_total <- as.factor(trainData$teach_total)
  #Data <- rbind(trainData, testData)
  write.csv(trainData, paste('/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/10-fold_data_na/train', i, 'csv', sep = '.'), row.names=FALSE)
  write.csv(testData, paste('/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/10-fold_data_na/test', i, 'csv', sep = '.'), row.names=FALSE)
  write.csv(Data, paste('/Users/mingxi/Desktop/TEMP/DISS/GLIMMIX/10-fold_data_na/data', i, 'csv', sep = '.'), row.names=FALSE)
}
