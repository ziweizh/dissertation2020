stats_scoring_RPlat <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/stats_scoring_RPlat.csv")
stats_scoring_RPlat$date2 <- gsub("/", "-", stats_scoring_RPlat$test_date)
stats_scoring_RPlat$soundname <- paste(stats_scoring_RPlat$exam_number, stats_scoring_RPlat$date2, sep="_")
stats_scoring_RPlat2 <- stats_scoring_RPlat[,c(6,3,4)]
stats_scoring_RPlat2$teach_total <- ifelse(stats_scoring_RPlat2$teach_total>=230,1,ifelse(stats_scoring_RPlat2$teach_total<230 & stats_scoring_RPlat2$teach_total>=210,2,ifelse(stats_scoring_RPlat2$teach_total<210 & stats_scoring_RPlat2$teach_total>=170,3,ifelse(stats_scoring_RPlat2$teach_total<170,4,999))))
head(stats_scoring_RPlat2)

lexis <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/sca_output.csv")
lexis$Filename <- gsub(".txt","",lexis$Filename)
dim(lexis)
head(lexis)

result <- merge(x=stats_scoring_RPlat2, y = lexis, by.x='soundname', by.y='Filename')
dim(result)
names(result)
setwd("/Users/mingxi/Desktop/TEMP/DISS/")
write.csv(result,"merged_sca.csv") # ** check the mismatches

# unique(result$soundname[which(is.na(result$nsyll), arr.ind=TRUE)])

# check the correlations
result <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/merged_lexis.csv")
for(i in 4:ncol(result)){
  correlations <- cor(result[,i],result$teach_total, method = "spearman")
  cat(i, correlations)
  cat('\n')
}

4 -0.29706 W
5 -0.2136516
6 -0.2601654 VP
7 -0.2381225
8 -0.1934456
9 -0.2207143
10 -0.1713114
11 -0.07394568
12 -0.3348426 CN
13 -0.02162635
14 -0.0474269
15 -0.01398473
16 -0.009009259
17 -0.04346461
18 -0.05564565
19 -0.1004561
20 -0.09598987
21 0.1027214
22 -0.03850919
23 -0.01617335
24 0.016692
25 -0.106227
26 -0.08153175