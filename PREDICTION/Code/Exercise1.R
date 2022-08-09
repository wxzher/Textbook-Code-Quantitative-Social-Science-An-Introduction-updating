##
## file :Exercise1.R
## author: wxzher
## update: 2022-12-12
## the code for exercie in Chapter4
##

## 基于博彩市场的预测
## H：市场可以有效汇总可用信息
## 分析民主党和共和党候选人在每个州胜利的合同的市场价格来检验这种假设。

## 导入数据
intrade12 <- read.csv("Datasets/intrade12.csv")
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
## 股票预测选取的结果
intrade12$winner <- ifelse(intrade12$PriceD > intrade12$PriceR, 1,-1)
intrade12$winner <- ifelse(intrade12$PriceD > intrade12$PriceR, 1,-1)
pres12$winner <- ifelse(pres12$Obama > pres12$McCain, 1, -1)
pres12$winner <- ifelse(pres12$Obama > pres12$Romney, 1, -1)


## convert to a Date object

intrade12$day <- as.Date(intrade12$day)
intrade12$day <- as.Date(intrade12$day)

## compute Day to election
intrade12$DaytoElection <- as.Date("2012-11-04") - intrade12$day
intrade12$DaytoElection <- as.Date("2012-11-06") - intrade12$day

##获取2012年选举前一天的数据
intrade12.pred <- subset(intrade12, subset = (intrade12$DaytoElection == 1))
sum.pre.2012 <- merge(intrade12.pred, pres12, by = "state")
# 假阳性
sum.pre.2012$state[sum.pre.2012$winner.x > sum.pre.2012$winner.y]
# 假阴性
sum.pre.2012$state[sum.pre.2012$winner.x < sum.pre.2012$winner.y]

##获取2012年选举前一天的数据
intrade12.pred <- subset(intrade12, subset = (intrade12$DaytoElection == 1))
sum.pre.2012 <- merge(intrade12.pred, pres12, by = "state")
sum.pre.2012
# 假阳性
sum.pre.2012$state[sum.pre.2012$winner.x > sum.pre.2012$winner.y]
# 假阴性
sum.pre.2012$state[sum.pre.2012$winner.x < sum.pre.2012$winner.y]

#### Question 2 ###

Obama.pred.day  <- rep(NA, 90)
for(i in 1:90){
  day.data <- subset(intrade12, 
                      subset = (intrade12$DaytoElection == i))
  day.data.sum <- merge(day.data, pres12, by = "state")
  
  Obama.pred.day[i] <- sum(day.data.sum$EV * day.data.sum$PriceD) / 100
}


Obama.pred.day
plot(90:1, Obama.pred.day, type = "b", xlim = c(90,0), ylim = c(200,400),
     col = "blue", xlab = "Days to the election", ylab = "support for Obama")

points(0, 365, pch = 19, col = "red")
abline(v = 0)
text(10, 365, "选票总数：365",col = "red")

#### question 3 ####
Obama.pred.week  <- rep(NA, 90)
for(i in 1:90){
  week.data <- subset(intrade12, 
                      subset = ((DaytoElection <= (90 - i + 7))
                                & (DaytoElection > (90 - i))))
  week.data.state <- tapply(week.data$PriceD, week.data$state, mean)
  state <- names(week.data.state)
  names(week.data.state) <- NA
  week.data <- data.frame(state, data = week.data.state)
  week.data.sum <- merge(week.data, pres12, by = "state")
  Obama.pred.week[i] <- sum(week.data.sum$EV * week.data$data) / 100
  
}
Obama.pred.week
plot(90:1, Obama.pred.week, type = "b", xlim = c(90,0), ylim = c(200,400),
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
## 循环
poll08.pred.day  <- rep(NA, 90)
for(i in 1:90){
  day.data <- subset(polls08, 
                     subset = (polls08$DaytoElection == i))
  if(length(day.data) == 0){
    poll08.pred.day[i] <- 0
  }else{
    daydata.state <- tapply(day.data$Obama, day.data$state, mean)
    state <- names(daydata.state) 
    # names(daydata.state) <- NA
    day.data  <- data.frame(state, data = daydata.state )
    day.data.sum <- merge(day.data, pres08, by = "state")
    day.data.sum
    poll08.pred.day[i - 2] <- sum(day.data.sum$EV * day.data.sum$Obama) / 100
  }
  
  
}

poll12.pred.day

plot(90:1, poll12.pred.day, type = "b", xlim = c(90,0), ylim = c(200,400),
     col = "blue", xlab = "Days to the election", ylab = "support for Obama")

points(0, 365, pch = 19, col = "red")
abline(v = 0)
text(10, 365, "选票总数：365",col = "red")

##### question 5  #####
intrade12$margin <- intrade12$PriceD - intrade12$PriceR
Beforeaday <- subset(intrade12, subset = (intrade12$DaytoElection == 1))
Beforeaday.sum <- merge(Beforeaday,pres12,by = "state")

Beforeaday.sum
cor(Beforeaday.sum$margin.x, Beforeaday.sum$margin.y)
fit <- lm(margin.y ~ margin.x, data = Beforeaday.sum)
coef(fit)

#### question 6 #####
intrade12$margin <- intrade12$PriceD - intrade12$PriceR
pres12$margin <- pres12$Obama - pres12$Romney
Beforeaday.12 <- subset(intrade12, subset = (intrade12$DaytoElection == 1))
Beforeaday.sum.12 <- merge(Beforeaday.12,pres12,by = "state")

Beforeaday.sum.12
cor(Beforeaday.sum.12$margin.x, Beforeaday.sum.12$margin.y)
fit2 <- lm(margin.y ~ margin.x, data = Beforeaday.sum.12)
coef(fit2)

## 民意调查和选举结果
polls12 <- read.csv("Datasets/polls12.csv")
polls12$margin <- polls12$Obama - polls12$Romney
## 日期
polls12$DaytoElection <- as.Date("2012-11-04") - as.Date(polls12$middate)
Beforeaday.poll <- subset()
