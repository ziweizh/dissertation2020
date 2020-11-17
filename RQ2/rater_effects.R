# assumption: same person appearing at different time (duplicates in ratee and rater columns) are treated as a different person
library(TAM)
library(tidyverse)
library(psych)
library(ggplot2)

# load the rating data
scoring <- read.csv("~/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv")
names(scoring)
dim(scoring)
table(scoring$rater)
# remove raters who provided less than 5 ratings
levels(scoring$rater)[levels(scoring$rater)=="Simpsone"] <- "simpsone"
scoring <- subset(scoring, rater!="aobryan" & rater!="gcobryan" & rater!="hjyang" & rater!="hma2" & rater!="huongle" & rater!="liberato" & rater!="sonsaat" & rater!="sphng" & rater!="tpaben" & rater!="zhili" & rater != "phuongn"& rater != "gcakinci")
scoring$rater <- droplevels(scoring$rater)
table(scoring$rater)
table(scoring$teach_total)
# remove Level 4 as there are only 5 cases
scoring <- scoring[scoring$teach_total!=4,]
table(scoring$teach_total)
# reverse code the ratings so conform to usual IRT interpretations
scoring$teach_total <- recode(scoring$teach_total, '1'=3, '2'=2, '3'=1) # levels --> categories
# code the raters as numeric
scoring$rater_num <- as.factor(as.numeric(scoring$rater))
str(scoring)

# reformat the long data format to wide
oect <- scoring %>% select(soundname1,rater_num,teach_total)
oect$teach_total <- oect$teach_total - 1 # to be consistent with RCML parameterization
oect_wide <- spread(oect, rater_num, teach_total)
head(oect_wide)
oect_wide2 <- oect_wide[,-1]
rownames(oect_wide2) <- oect_wide[,1]
describe(oect_wide2) 

###############################
# Inter-rater reliability (ICC)
###############################
library(irrNA)
iccNA(oect_wide2, detail = TRUE)
# assessing the ANVOA assumptions: normality of random effects
library(lme4)
icc_case1 <- lmer(teach_total~(1|soundname), REML=FALSE, data = scoring)
qqnorm(ranef(icc_case1)$soundname[,1], pch = 1, frame = FALSE)
qqline(ranef(icc_case1)$soundname[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(icc_case1)$soundname[,1])
qqnorm(residuals(icc_case1), pch = 1, frame = FALSE)
qqline(residuals(icc_case1), col = "steelblue", lwd = 2)
shapiro.test(residuals(icc_case1))

icc_case2 <- lmer(teach_total~(1|soundname) + (1|rater), REML=FALSE, data = scoring)
qqnorm(ranef(icc_case2)$soundname[,1], pch = 1, frame = FALSE)
qqline(ranef(icc_case2)$soundname[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(icc_case2)$soundname[,1])
shapiro.test(ranef(icc_case2)$rater[,1])
qqnorm(residuals(icc_case2), pch = 1, frame = FALSE)
qqline(residuals(icc_case2), col = "steelblue", lwd = 2)
shapiro.test(residuals(icc_case2))

icc_case3 <- lmer(teach_total~(1|soundname) + rater, REML=FALSE, data = scoring)
qqnorm(ranef(icc_case3)$soundname[,1], pch = 1, frame = FALSE)
qqline(ranef(icc_case3)$soundname[,1], col = "steelblue", lwd = 2)
shapiro.test(ranef(icc_case3)$soundname[,1])
qqnorm(residuals(icc_case3), pch = 1, frame = FALSE)
qqline(residuals(icc_case3), col = "steelblue", lwd = 2)
shapiro.test(residuals(icc_case3))

# ** calculate G(q,k) statistic: Putka et al. (2008)

# fitting rating scale model
rsm <- tam.mml(resp=data.matrix(oect_wide2), irtmodel = "RSM")
summary(rsm) # parameter estimates

# try including the time variable
teach_levels_data5 <- read.csv("~/Desktop/TEMP/DISS/teach_levels_data5.csv")
scoring2 <- merge(scoring, teach_levels_data5, by="soundname")
oect2 <- scoring2 %>% select(soundname,rater, date3, teach_total)
oect2$teach_total <- oect2$teach_total - 1
oect_wide3 <- spread(oect2, rater, teach_total)
head(oect_wide3)
oect_wide4 <- oect_wide3[,-1]
rownames(oect_wide4) <- oect_wide3[,1]
describe(oect_wide4) 

resp <- oect_wide4[,-1]
rsm2 <- tam.mml(resp=resp, Y=oect_wide4$date3, irtmodel = "RSM")
summary(rsm2) 
anova(rsm, rsm2) # useful to include the time variable; but for simplicity, RSM is used (assumption for re-taking students is viable)

# person estimates
summary(rsm$person$EAP)
summary(rsm$person$SD.EAP)
ggplot(data.frame(rsm$person)) +
  geom_histogram(aes(x = EAP, y = ..density..), binwidth = 0.5, fill = "grey", color = "black") +
  geom_density(aes(x = EAP, y = ..density..), color="red")
shapiro.test(rsm$person$EAP)

# construct maps
library(knitr)
rsm$xsi %>% kable(digits = 2) 
IRT.WrightMap(rsm) 
source("/Users/mingxi/Desktop/TEMP/DISS/Model/mfrm_construct_map_2017.R")
mfrm_construct_map(theta_subj = rsm$person$EAP, 
                   alfa_rater = rsm$xsi$xsi[1:21],
                   tresholds=c(rsm$xsi$xsi[22],-rsm$xsi$xsi[22])
)


# group-level indices for raters: followed parameterization in Eckes (2015)
est <- rsm$xsi$xsi[1:21]
se <- rsm$xsi$se.xsi[1:21]
wj <- 1/rsm$xsi$se.xsi[1:21]^2
cj <- rsm$xsi$xsi[1:21]

fixed_chisquare <- sum(wj*cj^2) - sum(wj*cj^2)/sum(wj)
fixed_chisquare #   118.7873
dchisq(fixed_chisquare, 21-1) #  2.034814e-16

rater_homogen_index <- sum(wj*(cj-sum(cj*wj)/sum(wj))^2) 
rater_homogen_index # 52.44779
1-pchisq(rater_homogen_index,df=21-1) # 9.793869e-05

rater_separation_ratio <- sqrt((var(est)-sum(se^2)/21)/(sum(se^2)/21)) # 1.137976
rater_separation_index <- (4*rater_separation_ratio+1)/3 # 1.851
rater_separation_reliability <- rater_separation_ratio^2/(1+rater_separation_ratio^2) # 0.564

# group-level indices for the ratees
est <- rsm$person$EAP
se <- rsm$person$SD.EAP
wj <- 1/se^2
cj <- est

fixed_chisquare <- sum(wj*cj^2) - sum(wj*cj^2)/sum(wj)
fixed_chisquare # 416.5974
dchisq(fixed_chisquare, 164-1) # 1.207699e-24

ratee_homogen_index <- sum(wj*(cj-sum(cj*wj)/sum(wj))^2) 
ratee_homogen_index # 417.0535
1-pchisq(rater_homogen_index,df=164-1) # 0

ratee_separation_ratio <- sqrt((var(est)-sum(se^2)/165)/(sum(se^2)/165)) # 1.23169
ratee_separation_index <- (4*ratee_separation_ratio+1)/3 # 1.975586
ratee_separation_reliability <- ratee_separation_ratio^2/(1+ratee_separation_ratio^2) # 0.6027111

# fit statistics
tam.fit(rsm)
msq.itemfit(rsm)
msq.itemfitWLE(rsm)

# residual expectation correations: empirical performance is not consistent with expectations in rater effects identification
res_rsm <- IRT.residuals(rsm)
resid_rsm<- data.frame(res_rsm$residuals)
dim(resid_rsm)
exp_rsm <- data.frame(res_rsm$X_exp)
dim(exp_rsm)
for (i in seq(1,21)) {
  cat(colnames(resid_rsm)[i], cor(resid_rsm[,i], exp_rsm[,i], use = "complete.obs"), '\n')
}

# residual plots
stdres_rsm <- data.frame(res_rsm$stand_residuals)
for (i in seq(1,21)) {
  tmp <- data.frame(na.omit(stdres_rsm[,i]))
  colnames(tmp) <- c('stdres')
  print(ggplot(tmp, aes(rownames(tmp), stdres)) + geom_point() + ylim(-3,3) + 
          geom_hline(yintercept=0) + geom_hline(aes(yintercept=-2), linetype="dashed") + geom_hline(aes(yintercept=2), linetype="dashed") + 
          ggtitle(colnames(stdres_rsm)[i]))
}


# PCM modeling
# frequency counts of ratings per per score category
with(scoring, table(rater_num,teach_total))
# fitting the PCM
pcm <- tam.mml(resp=data.matrix(oect_wide2), irtmodel = "PCM")
summary(pcm)
plot(pcm, type = "expected", export = FALSE, low = -6, high = 6) # expected score curves
plot(pcm, type = "items", export = FALSE, low = -6, high = 6) # category probability plots
  

