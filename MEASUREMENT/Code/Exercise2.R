##
## file: Exercise2.R
## author:wxz
## up-date:2022-08-06
## The code for exercise2 in Chapter3
## 

## 中国和墨西哥的政治效力
## 导入数据
vignettes <- read.csv("Datasets/vignettes.csv")
dim(vignettes)
summary(vignettes)
head(vignettes)
## 创建子集
China <- subset(vignettes, vignettes$china == 1)
Mexico <- subset(vignettes, vignettes$china == 0)

#### Question 1 ####
## 针对自我评估，受访者的回答分布图并计算受访者回答的平均值。
par(mfrow = c(1, 2),cex = 0.8)

selfChina.ptable <- prop.table(table(China$self,exclude = NULL))

barplot(selfChina.ptable, 
        main = "中国受访者自我评价的回答分布图",
        xlab = "评分",
        ylab = "占比")
selfMexico.ptable <- prop.table(table(Mexico$self,exclude = NULL))

barplot(selfMexico.ptable, 
        main = "墨西哥受访者自我评价的回答分布图",
        xlab = "评分",
        ylab = "占比")
dev.off()
## 分数差异
mean(China$self)   # 2.621908
mean(Mexico$self)  # 1.825301
mean(China$self) - mean(Mexico$self)  # 0.7966069

#### Question 2 ####
## 年龄直方图和分位数图
## 中国
hist(China$age, freq = FALSE, ylim = c(0, 0.04), xlab = ("Age"), 
     main = "Distribution of responder's age in China")
text(50, 0.04,"median:45",col = "orange")
abline(v = median(China$age),col = "orange")
## 墨西哥
hist(Mexico$age, freq = FALSE, ylim = c(0, 0.04), xlab = ("Age"), 
     main = "Distribution of responder's age in Mexico")
text(50, 0.04,"median:45",col = "orange")
abline(v = median(Mexico$age),col = "orange")

## 分位数图
qqplot(China$age, Mexico$age, xlab = "China-age",
       ylab = "Mexico-age", xlim = c(20,90), 
       ylim = c(18,90), 
       main = "China/Mexico age dimension")
abline(0, 1)

#### Question 3 ####

China.SelfLowMose <- ifelse(China$self < China$moses , 1, 0)
Mexico.SelfLowMose <- ifelse(Mexico$self< Mexico$moses, 1, 0)

mean(China.SelfLowMose)
mean(Mexico.SelfLowMose)

#### Question 4####

## 计算排名
vignettes$compare <- NA
vignettes$compare[vignettes$self < vignettes$moses] <- 1
vignettes$compare[vignettes$self >= vignettes$moses & vignettes$self < vignettes$jane] <- 2
vignettes$compare[vignettes$self >= vignettes$jane & vignettes$self < vignettes$alison] <- 3
vignettes$compare[vignettes$self >= vignettes$alison] <- 4

## 生成表格绘制柱状图
compare.ptable <- prop.table(table(vignettes$compare, exclude = NULL))
compare.ptable
barplot(compare.ptable, names.arg = c(1,2,3,4),ylim = c(0,0.35),
        xlab = "order",ylab = "distribution",
        main = "Distribution of compare")

## 计算均值
tapply(vignettes$compare, vignettes$china, mean)

#### Question 5 #####

## 分别取 40 岁以上和一下民众子集

Age.high40 <- subset(vignettes,vignettes$age > 40) 
Age.low40 <- subset(vignettes,vignettes$age < 40)

## 绘制条形图
high40.ptable <- prop.table(table(Age.high40$compare))
low40.ptable <- prop.table(table(Age.low40$compare))
barplot(high40.ptable, names.arg = c(1,2,3,4),ylim = c(0,0.35),
        xlab = "order",ylab = "distribution",
        main = "Distribution of high40.compare")
barplot(low40.ptable, names.arg = c(1,2,3,4),ylim = c(0,0.35),
        xlab = "order",ylab = "distribution",
        main = "Distribution of low40.compare")
## 计算平均数
mean(Age.high40$compare)
mean(Age.low40$compare)





