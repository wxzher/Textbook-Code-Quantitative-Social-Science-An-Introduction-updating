##
## file: Exercise3.R
## author:wxz
## up-date:2022-08-06
## The code for exercis31 in Chapter3
## 

## 联合国大会投票表决
## 导入数据
unvoting <- read.csv("Datasets/unvoting.csv")
## 观察数据
dim(unvoting)
summary(unvoting)
head(unvoting)

#### Question 1 ####
## 绘制柏林墙倒塌前后十年理想分数的分布图。
## 创建子集
BeforeDecade <- unvoting$idealpoint[unvoting$Year >= 1980 & unvoting$Year < 1990]
AfterDecade <- unvoting$idealpoint[unvoting$Year >= 1990 & unvoting$Year < 2000]

par(mfrow = c(1, 2),cex = 0.8)

## 前十年
hist(BeforeDecade, 
     freq = FALSE, 
     xlab = "idealpoint",
     main = "Distribution of 1980-1990 idealpoint")

## add a text label 
text(x = 1, y = 0.6, "median", col = "red")
## add a vertical line representing median
abline(v = median(BeforeDecade),
       lty = "dotted",col = "red")

## 后十年
hist(AfterDecade, 
     freq = FALSE, 
     xlab = "idealpoint",
     main = "Distribution of 1990-2000 idealpoint")

## add a text label 
text(x = 1, y = 0.6, "median", col = "red")
## add a vertical line representing median
abline(v = median(AfterDecade),
       lty = "dotted",col = "red")
dev.off()
## 计算quantile
quantile(BeforeDecade) 
quantile(AfterDecade)

#### Question 2 ####
## 与美国投票一致的国家如何变化，并与俄罗斯匹配程度变化
agreeUS <- tapply(unvoting$PctAgreeUS, unvoting$Year, mean,na.rm=TRUE) 
agreeRussia <- tapply(unvoting$PctAgreeRUSSIA, unvoting$Year, mean,na.rm=TRUE)
## 画出折线图
plot(names(agreeUS), agreeUS, type = "l",col = "blue", ylim = c(0, 0.9), 
     xlab = "Year", ylab = "distribution", main = "与美国和俄罗斯投票一致国家百分比图") 
lines(names(agreeRussia), agreeRussia, type = "l", col = "red") 
text(2008, 0.1, "US", col = "blue") 
text(2008, 0.6, "Russia", col = "red")

## 最支持美国的国家
PctAgreeUS <- tapply(unvoting$PctAgreeUS, unvoting$CountryAbb, mean)
sort(PctAgreeUS, decreasing = TRUE)
sort(PctAgreeUS, decreasing = TRUE)[2]

## 最支持俄罗斯的国家
PctAgreeRussia <- tapply(unvoting$PctAgreeRUSSIA, unvoting$CountryAbb, mean)
sort(PctAgreeRussia, decreasing = TRUE)
sort(PctAgreeRussia, decreasing = TRUE)[2]
## 一贯支持美国或俄罗斯的国家
unvoting$AgreeScore <- ifelse(unvoting$PctAgreeUS >= unvoting$PctAgreeRUSSIA, 1, 0)

AgreePercentage <- tapply(unvoting$AgreeScore,unvoting$CountryAbb,sum, na.rm = TRUE) /table(unvoting$CountryAbb)

sort(AgreePercentage)
AgreePercentage[AgreePercentage == 1] # 一贯支持美国
AgreePercentage[AgreePercentage == 0] # 一贯支持俄罗斯

#### Question 3 ####

idealpoint.US <- subset(unvoting, unvoting$CountryName == "United States of America")

idealpoint.Russia <- subset(unvoting, unvoting$CountryName == "Russia")

idealpoint.mean <- tapply(unvoting$idealpoint, unvoting$Year, mean)

## 画图
plot(idealpoint.US$Year, idealpoint.US$idealpoint, type = "l", col = "red",
     ylim = c(-3, 3),xlab = "Year", ylab = "idealpoint", 
     main = "Distribution of idealpoint" )
lines(idealpoint.Russia$Year, idealpoint.Russia$idealpoint,col = "blue", lty = "dotted")
lines(names(idealpoint.mean), idealpoint.mean,col = "green", lty = "dashed")
## 标签

text(2010, 2, "US", col = "red")
text(2010, 0.5, "Russia", col = "blue")
text(2010, -0.5, "Mean\n of all", col = "green")


#### Question 4 #####

## 曾经是苏联国家
BeforeSoveit <- c("Estonia","Latvia","Lithuania","Belarus","Moldova","Ukraine",
                  "Armenia","Georgia","Kazakhstan","Kyrgyzstan","Tajikistan",
                  "Turkmenistan","Uzbekistan","Russia")

## 创建子集
BeforeSoveit.voting <- subset(unvoting, CountryName %in% BeforeSoveit)
## 2012年的数据
SovietCountryAbb <- unique(BeforeSoveit.voting$CountryAbb)
BeforeSoveit2012.idea <- BeforeSoveit.voting$idealpoint[BeforeSoveit.voting$Year == 2012]
BeforeSoveit2012.agressUS <- BeforeSoveit.voting$PctAgreeUS[BeforeSoveit.voting$Year == 2012]
## 曾经的苏联国家2012年的理想点和对美国的投票比例进行对比
par(mfrow = c(1,2), cex = 0.8 )
plot(BeforeSoveit2012.idea , type = "l", col = "red",
   xlab = "Year", ylab = "idealpoint", 
     main = "idealpoint of Soviet in 2012" )
plot(BeforeSoveit2012.agressUS , type = "l", col = "blue",
     xlab = "Year", ylab = "pctAgressUS", 
     main = "AgreeUS of Soviet in 2012" )
dev.off()

## 苏联国家和其他国家做对比
## 其他国家2012年的数据
Other.voting <- subset(unvoting, CountryName %in% BeforeSoveit == FALSE)
Other2012.idea <- Other.voting$idealpoint[Other.voting$Year ==2012]
Other2012.agreeUS <- Other.voting$PctAgreeUS[Other.voting$Year ==2012]

## 画图

plot(BeforeSoveit2012.idea, BeforeSoveit2012.agressUS, pch = 16, col = "blue",
     xlab = "idealpoint", ylab = "agreeUS",xlim = c(-2,3),ylim = c(0,1),
     main = "Soviet & non-Soviet in 2012") 
points(Other2012.idea, Other2012.agreeUS, pch = 17, col = "red")
text(0.5, 0.15, "Not-Soviet", col = "red")
text(0, 0.3, "Soviet", col = "blue")

#### Question 5 ####
## 按年份绘制理想点的分数

## 计算各年份的中位数
BeforeSoveit.idea.median <- tapply(BeforeSoveit.voting$idealpoint, 
                                   BeforeSoveit.voting$Year,median)
BeforeSoveit.idea.median

Other.idea.median <- tapply(Other.voting$idealpoint, Other.voting$Year, median)

Other.idea.median

## 画图

plot(names(BeforeSoveit.idea.median), BeforeSoveit.idea.median, type = "l",
     xlab = "Year", ylab = "idealpoint", col = "green",ylim = c(-3, 1.5))
lines(names(Other.idea.median), Other.idea.median, col = "blue", lty = "dotted")
abline(v = 1989, lty ="dashed", col = "red")
text(1998, 1, "柏林墙倒塌", col = "red")
text(2008, -0.1, "Soviet", col = "green")
text(2008, -0.8, "Not- Soviet", col = "blue")

####Question 6 #####
## 通过k-mean均值计算方法应用于理想点和美国一致投票比例这两个问题。
## 用两个质心开始计算

## 1982与2012年理想点和美国支持率
ideaAgreeUS1989 <- cbind(unvoting$idealpoint[unvoting$Year == 1989],
                         unvoting$PctAgreeUS[unvoting$Year == 1989])
ideaAgreeUS2012 <- cbind(unvoting$idealpoint[unvoting$Year == 2012],
                         unvoting$PctAgreeUS[unvoting$Year == 2012])
## 创建两个质心的聚类
K1989two.out <- kmeans(ideaAgreeUS1989, centers = 2, nstart = 5)
K2012two.out <- kmeans(ideaAgreeUS2012, centers = 2, nstart = 5)
K1989two.out$centers
K2012two.out$centers

## 可视化聚类结果
par(mfrow = c(1, 2), cex = 0.8)
plot(ideaAgreeUS1989, col = K1989two.out$cluster + 1,
     xlab = "idealpoint", ylab = "agreeUS",
     main = "1989")
points(K1989two.out$centers,pch = 8,cex =2)
plot(ideaAgreeUS2012, col = K2012two.out$cluster + 1,
     xlab = "idealpoint", ylab = "agreeUS",
     main = "2012")
points(K2012two.out$centers,pch = 8,cex =2)