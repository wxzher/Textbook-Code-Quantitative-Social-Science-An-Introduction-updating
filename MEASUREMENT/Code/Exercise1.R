##
## file: Exercise1.R
## author:wxz
## up-date:2022-08-06
## The code for exercise1 in Chapter3
## 

## 导入数据
gayreshaped <-read.csv("Datasets/gayreshaped.csv")
dim(gayreshaped)
summary(gayreshaped)
head(gayreshaped)

#### Question1 #####
## 观察研究1中第一波和第二波"No Contact"数据的相关性。
NoContact1 <- subset(gayreshaped, 
                     gayreshaped$study == 1 & gayreshaped$treatment == "No Contact")
cor(NoContact1$therm1,NoContact1$therm2,use = "complete.obs")

# 0.9975817
# 说明相关性很强。

#### Question 2 ####
NoContact2 <- subset(gayreshaped, 
                        gayreshaped$study == 2 & gayreshaped$treatment == "No Contact")

NoContact2.therm <- data.frame(NoContact2$therm1, NoContact2$therm2,NoContact2$therm3, NoContact2$therm4)
cor(NoContact2.therm,use = "pairwise.complete.obs")

#### Question 3 ####
## 画散点图
## 第一波和第二波的散点图
pdf(file = "Output/sandiantu.pdf", width = 6, height = 8)
par(mfrow = c(2, 2),cex = 0.8)
plot(NoContact2$therm1, NoContact2$therm2, pch = 16, 
     col = "red", ylim = c(0,100), xlab = "Therm1", ylab = "Therm2",
     main = "第一波和第二波的散点图")
## 第一波和第三波的散点图
plot(NoContact2$therm1, NoContact2$therm3, pch = 16, 
     col = "yellow", ylim = c(0,100), xlab = "Therm1", ylab = "Therm3",
     main = "第一波和第三波的散点图")
## 第一波和第四波的散点图
plot(NoContact2$therm1, NoContact2$therm4, pch = 16, 
     col = "blue", ylim = c(0,100), xlab = "Therm1", ylab = "Therm4",
     main = "第一波和第四波的散点图")
dev.off()

#### Question 4 ####

ccap <- read.csv("Datasets/ccap2012.csv")

## 2012CCAP的感受评分直方图
hist(ccap$gaytherm,las = 1, freq = FALSE, xlab = "therm", 
     main = "Histog ram of GayTherm", breaks = 20) 
abline(v = mean(ccap$gaytherm, na.rm = TRUE), lty = "dashed", col = "red ")
text(x = 67, y = 0.030, "mean(58.7)", col = "red") 
lines(density(ccap$gaytherm, na.rm = TRUE), col = "red", lwd = 2)

## 研究1中wave1的感受评分
hist(NoContact1$therm1, col = "yellow", las = 1, freq = FALSE,
     xlab = "therm", main = "Histogram of Study1's Therm1", breaks = 20)
## 研究2中wave1 的感受评分
hist(NoContact2$therm1, col = "yellow", las = 1, freq = FALSE,
     xlab = "therm", main = "Histogram of Study2's Therm1", breaks = 20)

#### Question 5 ####

qqplot(ccap$gaytherm,NoContact1$therm1,
       xlab = "gaytherm",
       ylab = "therm study1", 
       main = "gaytherm/ therm study1") 
abline(0,1)
qqplot(ccap$gaytherm,NoContact2$therm1, 
       xlab = "gaytherm",
       ylab = "therm study2", 
       main = "gaytherm/ therm study2") 
abline(0,1)
