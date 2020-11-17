stats_scoring_RPlat <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/stats_scoring_RPlat.csv")
stats_scoring_RPlat$date2 <- gsub("/", "-", stats_scoring_RPlat$test_date)
stats_scoring_RPlat$soundname <- paste(stats_scoring_RPlat$exam_number, stats_scoring_RPlat$date2, sep="_")
stats_scoring_RPlat2 <- stats_scoring_RPlat[,c(6,3,4)]
stats_scoring_RPlat2$teach_total <- ifelse(stats_scoring_RPlat2$teach_total>=230,1,ifelse(stats_scoring_RPlat2$teach_total<230 & stats_scoring_RPlat2$teach_total>=210,2,ifelse(stats_scoring_RPlat2$teach_total<210 & stats_scoring_RPlat2$teach_total>=170,3,ifelse(stats_scoring_RPlat2$teach_total<170,4,999))))
head(stats_scoring_RPlat2)

library(stringr)
rhythm <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/Prosody/rhythm.csv")
soundname <- unique(stats_scoring_RPlat2$soundname)
ratee2 <- c()
for (i in rhythm$ratee) {
  for (j in soundname) {
    if (grepl(gsub("-","",gsub("_","",j)), str_sub(i,8,str_length(i)-4))) {
      ratee2 <- c(ratee2, j)
    } 
  }
}
ratee2
rhythm$ratee
# some mismatchs in length
# checked the match manually
setwd('/Users/mingxi/Desktop/TEMP/DISS/Prosody')
write.csv(ratee2,'ratee2_rhythm.csv')
write.csv(rhythm$ratee,'rhythm_ratee.csv')

ratee3 <- read.csv('ratee2_rhythm.csv', header = TRUE)
rhythm$ratee2 <- ratee3$ratee2
setwd('/Users/mingxi/Desktop/TEMP/DISS/Prosody')
write.csv(rhythm,'rhythm2.csv') # checked that the examinee IDs matched

result <- merge(x=stats_scoring_RPlat2, y = rhythm, by.x='soundname', by.y='ratee2')
dim(result)
names(result)
setwd("/Users/mingxi/Desktop/TEMP/DISS/Prosody")
write.csv(result,"merged_rhythm2.csv")

