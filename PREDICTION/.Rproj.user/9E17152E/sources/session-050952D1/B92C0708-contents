##
## file :Exercise1.R
## author: wxzher
## update: 2022-08-08
## the code for exercie in Chapter4
##
### 墨西哥的选举和条件现的转移计划
## 首先对随机化进行评估
## 假设：CCT计划将动员选民，导致在位政党的支持率增加

## 导入数据
progresa <- read.csv("Datasets/progresa.csv")
dim(progresa)
summary(progresa)
View(progresa)

## question 1 ####



mean(progresa$pri2000s[progresa$treatment == 1]) - mean(progresa$pri2000s[progresa$treatment == 0])
fit <- lm(pri2000s ~ treatment, data = progresa)

cor(progresa$treatment,progresa$pri2000s)
coef(fit)

### question 2 #####

progresa$share <- (progresa$pri2000s / progresa$t2000) * 100

fit <- lm(share ~ avgpoverty + pobtot1994 + votos1994 + pri1994 + pan1994 + prd1994, data = progresa)
fit
