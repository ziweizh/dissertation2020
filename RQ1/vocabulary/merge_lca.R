stats_scoring_RPlat <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/stats_scoring_RPlat.csv")
stats_scoring_RPlat$date2 <- gsub("/", "-", stats_scoring_RPlat$test_date)
stats_scoring_RPlat$soundname <- paste(stats_scoring_RPlat$exam_number, stats_scoring_RPlat$date2, sep="_")
stats_scoring_RPlat2 <- stats_scoring_RPlat[,c(6,3,4)]
stats_scoring_RPlat2$teach_total <- ifelse(stats_scoring_RPlat2$teach_total>=230,1,ifelse(stats_scoring_RPlat2$teach_total<230 & stats_scoring_RPlat2$teach_total>=210,2,ifelse(stats_scoring_RPlat2$teach_total<210 & stats_scoring_RPlat2$teach_total>=170,3,ifelse(stats_scoring_RPlat2$teach_total<170,4,999))))
head(stats_scoring_RPlat2)

lexis <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/lca_output.csv")
lexis$filename <- gsub(".txt","",lexis$filename)
dim(lexis)
head(lexis)

result <- merge(x=stats_scoring_RPlat2, y = lexis, by.x='soundname', by.y='filename')
dim(result)
names(result)
setwd("/Users/mingxi/Desktop/TEMP/DISS/")
write.csv(result,"merged_lca.csv") # ** check the mismatches

# unique(result$soundname[which(is.na(result$nsyll), arr.ind=TRUE)])

# check the correlations
result <- read.csv("/Users/mingxi/Desktop/TEMP/DISS/merged_lexis.csv")
for(i in 4:ncol(result)){
  correlations <- cor(result[,i],result$teach_total, method = "spearman")
  cat(i, correlations)
  cat('\n')
}

4 -0.2551152 wordtypes
5 -0.2161684 swordtypes
6 -0.2132365 lextypes
7 -0.1973815
8 -0.2910537 wordtokens
9 -0.2895583 swordtokens
10 -0.2328114
11 -0.1819491
12 -0.1147569
13 -0.08093366
14 -0.0445702
15 -0.03745714
16 -0.1085492
17 -0.1093411
18 -0.2551152 ndw
19 0.04315155
20 -0.106945
21 -0.1041063
22 0.1557262
23 -0.09916241
24 -0.108381
25 -0.1079992
26 0.09908739
27 -0.007913328
28 0.09705577
29 0.001438632
30 -0.1284439
31 -0.1290088
32 -0.0413849
33 0.03281307
34 0.08909948
35 0.01579424
36 0.04879213

