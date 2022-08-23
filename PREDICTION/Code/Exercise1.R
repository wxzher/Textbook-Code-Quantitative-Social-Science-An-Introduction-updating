##
## file :Exercise1.R
## author: wxzher
## update: 2022-08-12
## the code for exercie in Chapter4
##

## 基于博彩市场的预测
## H：市场可以有效汇总可用信息
## 分析民主党和共和党候选人在每个州胜利的合同的市场价格来检验这种假设。

## 导入数据
intrade08 <- read.csv("Datasets/intrade08.csv")
intrade12 <- read.csv("Datasets/intrade12.csv")
pres12 <- read.csv("Datasets/pres12.csv")
pres12 <- read.csv("Datasets/pres12.csv")
## 分析数据
dim(intrade12)
dim(intrade12)
summary(intrade12)
summary(intrade12)
head(intrade12)
#### Question 1 ####

## 

## convert to a Date object

intrade08$day <- as.Date(intrade08$day)
intrade12$day <- as.Date(intrade12$day)

## compute Day to election
intrade08$DaytoElection <- as.Date("2008-11-04") - intrade08$day
intrade12$DaytoElection <- as.Date("2012-11-06") - intrade12$day
## 获取2008年选举前一天的数据
intrade08.pred <- subset(intrade08, subset = (intrade08$DaytoElection == 1))
##获取2012年选举前一天的数据
intrade12.pred <- subset(intrade12, subset = (intrade12$DaytoElection == 1))
## 计算差值
intrade08.pred$margin <- intrade08.pred$PriceD - intrade08.pred$PriceR
pres08$margin <- pres08$Obama - pres08$McCain
sum.pre.08 <- merge(intrade08.pred, pres08, by = "state")
# 假阳性
sum.pre.08$statename[sign(sum.pre.08$margin.x) > sign(sum.pre.08$margin.y)]
# 假阴性
sum.pre.08$statename[sign(sum.pre.08$margin.x) < sign(sum.pre.08$margin.y)]

## 计算差值
intrade12.pred$margin <- intrade12.pred$PriceD - intrade12.pred$PriceR
pres12$margin <- pres12$Obama - pres12$Romney
sum.pre.12 <- merge(intrade12.pred, pres12, by = "state")
sum.pre.12 <- subset(sum.pre.12, is.na(sum.pre.12$margin.x)== FALSE)
# 假阳性
sum.pre.12$statename[sign(sum.pre.12$margin.x) > sign(sum.pre.12$margin.y)]
# 假阴性
sum.pre.12$statename[sign(sum.pre.12$margin.x) < sign(sum.pre.12$margin.y)]
#### Question 2 ###

intrade08$margin <- intrade08$PriceD - intrade08$PriceR
## 计算准确率
Obama.pred <- rep(NA, 90)
for (i in 90:1) {
  ## 08年的数据不存在NA的缺失值
  data <- subset(intrade08, DaytoElection == i)
  Obama.pred[i] <- sum(pres08$EV[data$margin > 0])
  
}
Obama.pred
plot(1:90, Obama.pred, type = "l", xlim = c(90,0), ylim = c(200,400),
     col = "blue", xlab = "Days to the election", ylab = "Prediction")

points(0, 365, pch = 19, col = "red")
abline(v = 0)
text(10, 365, "选票总数：365",col = "red")

#### question 3 ####
Obama.pred.week  <- rep(NA, 90)
for(i in 1:90){
  week.data <- subset(intrade08, 
                      subset = ((DaytoElection <= (90 - i + 7))
                                & (DaytoElection > (90 - i))))
  week.data.state <- data.frame(tapply(week.data$PriceD, week.data$state, mean),
                                tapply(week.data$PriceR, week.data$state, mean))
  names(week.data.state)<-c("priceD","priceR")
  week.data.state$margin<- week.data.state$priceD- week.data.state$priceR 
  Obama.pred.week [i]<-sum(pres08$EV[ week.data.state$margin >0])
  
}
Obama.pred.week
plot(90:1, Obama.pred.week, type = "l", xlim = c(90,0), ylim = c(200,400),
     col = "blue", xlab = "weeks to the election", ylab = "support for Obama")

points(0, 365, pch = 19, col = "red")
abline(v = 0)
text(10, 365, "选票总数：365",col = "red")

#### question 4 ####
polls08 <- read.csv("Datasets/polls08.csv")
summary(polls08)
View(polls08)
## 日期
polls08$DaytoElection <- as.Date("2008-11-04") - as.Date(polls08$middate)
## 每个州的获胜幅度
polls08$margin<-polls08$Obama-polls08$McCain
## 每个州的选举人票数

state.vote <- tapply(pres08$EV, pres08$state,sum)
## 循环
poll.pred  <- rep(NA, 90)
for(i in 1:90){
  day.data <- subset(polls08, 
                     subset = (polls08$DaytoElection >= 90-i))
  ## 考虑到最小选举日
  mindate <- tapply(day.data$DaytoElection, day.data$state, min)
  ## 平均民调结果
  day.poll.mean <- rep(NA, 51)
  ## 将最小选举日的数据代替缺失数据的日期之后重新获取数据
  count = 1
  for (j in unique(day.data$state)) {
    day.date.state <- subset(day.data, day.data$state == j)
    mindate.state <- subset(day.date.state, DaytoElection == mindate[j])
    day.poll.mean[count] <- mean(mindate.state$margin)
    count = count + 1
    
  }
 
  ## 民调预测的选票数据
  poll.pred[i] <- sum(state.vote[day.poll.mean >= 0])
  
  }

plot(90:1, poll.pred, type = "l", xlim = c(90,0), ylim = c(100,400),
     col = "blue", xlab = "Days to the election", ylab = "support for Obama")

points(0, 365, pch = 19, col = "red")
abline(h = 365)
text(10, 365, "选票总数：365",col = "red")

##### question 5  #####
intrade08.pred$fact.margin <- pres08$margin
fit <- lm(fact.margin ~ margin, data = intrade08.pred)
coef(fit)

## 最新的民意调查数据
pres08$new.pred <- rep(NA, 51)
for (i in 1:51){
  newdata.state <- subset(polls08, polls08$state == pres08$state[i])
  newdata <- subset(newdata.state, DaytoElection == min(DaytoElection))
  pres08$new.pred[i] <- mean(newdata$margin)
}
fit2 <- lm(margin ~ new.pred, data = pres08)
coef(fit2)
#### question 6 #####

##判断真实结果奥巴马获胜 
sum.pre.08$pred.vote <- predict(fit, newdata = data.frame(margin = sum.pre.08$margin.x ))
sum.pre.08$misclassified <- sign(sum.pre.08$margin.y) != sign(sum.pre.08$pred.vote)

plot(sum.pre.08$pred.vote, sum.pre.08$margin.y, cex = 0.8, type = "n",
     xlim = c(-50, 50), ylim = c(-50, 50), xlab = "Predict Obama margin",
     ylab = "Actual Obana Margin" )
text(sum.pre.08$pred.vote, sum.pre.08$margin.y, sum.pre.08$state,
     col = ifelse(sum.pre.08$misclassified, "red", "black"))
abline(h = 0)
abline(v = 0)

## 2012年最新民意调查结果

polls12 <- read.csv("Datasets/polls12.csv")
polls12$middate <- as.Date(polls12$middate)
## 日期
polls12$DaysToElection<-as.Date("2012-11-06")-polls12$middate
polls12$margin<- polls12$Obama-polls12$Romney
## 各州的平均获胜幅度
pres12$new.pred <- rep(NA, 51)
polls12$state<-as.character(polls12$state)
for (i in 1:51){
  newdata.state <- subset(polls12, polls12$state == pres12$state[i])
  newdata <- subset(newdata.state, DaysToElection == min(DaysToElection))
  pres12$new.pred[i] <- mean(newdata$margin)
}
## 预测值
pres12$pred.vote<-predict(fit2, newdata = data.frame(new.pred = pres12$new.pred ))
## 错误分类
pres12$misclassified <- sign(pres12$margin) != sign(pres12$pred.vote)
##画图
plot(pres12$pred.vote,pres12$margin, cex = 0.8, type = "n",
     xlim = c(-50, 50), ylim = c(-50, 50), xlab = "Predict Obama margin",
     ylab = "Actual Obana Margin" )
text(pres12$pred.vote, pres12$margin, pres12$state,
     col = ifelse(pres12$misclassified, "red", "black"))
abline(h = 0)
abline(v = 0)