stats_scoring_RPlat <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/stats_scoring_RPlat.csv")
stats_scoring_RPlat$date2 <- gsub("/", "-", stats_scoring_RPlat$test_date)
stats_scoring_RPlat$soundname <- paste(stats_scoring_RPlat$exam_number, stats_scoring_RPlat$date2, sep="_")
stats_scoring_RPlat2 <- stats_scoring_RPlat[,c(6,3,4)]
stats_scoring_RPlat2$teach_total <- ifelse(stats_scoring_RPlat2$teach_total>=230,1,ifelse(stats_scoring_RPlat2$teach_total<230 & stats_scoring_RPlat2$teach_total>=210,2,ifelse(stats_scoring_RPlat2$teach_total<210 & stats_scoring_RPlat2$teach_total>=170,3,ifelse(stats_scoring_RPlat2$teach_total<170,4,999))))
head(stats_scoring_RPlat2)

library(stringr)
pron <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/pronunciation.csv")
soundname <- unique(stats_scoring_RPlat2$soundname)
ratee2 <- c()
for (i in pron$person) {
  for (j in soundname) {
    if (grepl(gsub("-","",gsub("_","",j)), str_sub(i,8,str_length(i)-4))) {
      ratee2 <- c(ratee2, j)
    } 
  }
}
ratee2
pron$person
# some mismatchs in length
# checked the match manually
setwd('/Users/mingxi/Desktop/TEMP/stat_501/Presentation')
write.csv(ratee2,'ratee2_pronunciation.csv')
write.csv(pron$person,'pronunciation_ratee.csv')

ratee3 <- read.csv('ratee2_pronunciation.csv', header = TRUE)
pron$ratee2 <- ratee3$ratee2
setwd('/Users/mingxi/Desktop/TEMP/DISS')
write.csv(pron,'pronunciation2.csv') # checked that the examinee IDs matched

result <- merge(x=stats_scoring_RPlat2, y = pron, by.x='soundname', by.y='ratee2')
dim(result)
names(result)
setwd("/Users/mingxi/Desktop/TEMP/DISS/")
write.csv(result,"merged_pronunciation2.csv")

