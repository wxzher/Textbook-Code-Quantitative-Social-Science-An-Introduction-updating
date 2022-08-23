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
## 平均差值和回归系数的计算
## 方法1：平均差值
## 获取干预组和控制组
intervene<-subset(progresa, treatment == 1) 
control<-subset(progresa, treatment == 0)
## t2000差异
dif.t2000<-mean(intervene$t2000) - mean(control$t2000)
dif.t2000
## pri2000s差异
dif.pri2000s <- mean(intervene$pri2000s) - mean(control$pri2000s)
dif.pri2000s
## 干预组投票率高于控制组

## 方法2：回归系数
## CCT计划对投票率的影响
fit.t2000 <- lm(t2000 ~ treatment, data = progresa)
## CCT计划对pri支持率的影响
fit.pri2000s <- lm(pri2000s ~ treatment, data = progresa)

## 系数
coef(fit.t2000)
coef(fit.pri2000s)
## 对比两种方法的结果
list(data.frame(dif.t2000,coef(fit.t2000)[2]),data.frame(dif.pri2000s, coef(fit.pri2000s)[2]))
## 结果证明对于二元变量的回归系数可以代表平均干预效果

### question 2 #####
## 多元线性回归模型的拟合
## 对投票率
##总票数
progresa$sum1994<-progresa$pri1994+progresa$pan1994+progresa$prd1994
## 多元回归
fit3<-lm(t2000 ~ avgpoverty+pobtot1994+votos1994+sum1994+treatment,data = progresa) 
coef(fit3)

## 对在位政党的投票率
fit4 <- lm(pri2000s ~ avgpoverty+pobtot1994+votos1994+sum1994+treatment,data = progresa)
coef(fit4)
## 对比
list(data.frame(dif.t2000,coef(fit3)[6]),data.frame(dif.pri2000s, coef(fit4)[6]))
## 结果产生偏差

#### question 3 #####

## 变换人口份额和区域自然对数后构建新的模型
progresa$adult.vote<-progresa$pri1994s+progresa$pan1994s+progresa$prd1994s
## 对在位政党的投票率
fit5<-lm(pri2000s~avgpoverty+t1994+I(log(pobtot1994))+adult.vote+treatment,data = progresa)
coef(fit5)
## 投票率
fit6 <- lm(t2000~avgpoverty+t1994+I(log(pobtot1994))+adult.vote+treatment,data = progresa)
coef(fit6)
## 展示
list(data.frame(coef(fit3)[6],coef(fit5)[6]),data.frame(coef(fit4)[6], coef(fit6)[6]))
## 定义一个计算R方的函数
R2 <- function(fit){
  resid <- resid(fit)
  y <- fitted(fit) + resid
  TTS <- sum((y - mean(y)) ^2)
  SSR <- sum(resid^2)
  R2 <- (TTS - SSR) / TTS
  return(R2)
}

## 计算R方
list(data.frame(R2(fit3), R2(fit5)), data.frame(R2(fit4), R2(fit6)))
#### question 4#####

## 绘制箱型图
##区域人口分布 
progresa$proportion1994<-progresa$pobtot1994/progresa$villages 
boxplot(proportion1994 ~ treatment,data = progresa,ylim=c(0,1800))
## 平均贫困指数
boxplot(avgpoverty~treatment,data = progresa,ylim=c(3,6))
## 之前的投票率
boxplot(t1994~treatment,data = progresa,ylim=c(0,100))
##之前的 PRI 支持率 
boxplot(pri1994s~treatment,data = progresa,ylim=c(0,100))
##### question 5 #####
## 三个政党的官方投票率
progresa$official<-progresa$pri1994v+progresa$pan1994v+progresa$prd1994v
##在位党支持 
fit7<-lm(pri2000v~avgpoverty+I(log(pobtot1994))+t1994r+official+treatment,data = progresa)
coef(fit7)
##投票率 
fit8<-lm(t2000r~avgpoverty+I(log(pobtot1994))+t1994r+official+treatment,data = progresa) 
coef(fit8)
list(data.frame(coef(fit5)[6],coef(fit7)[6]),data.frame(coef(fit6)[6], coef(fit8)[6]))
list(R2(fit7), R2(fit8))
## R方值过低，模型拟合效果不好
#####question 6 ####
## 平均干预效果与区域贫困水平的交互
fit9<-lm(t2000~treatment*I((avgpoverty)^2)+treatment:avgpoverty+avgpoverty+I(log(pobtot1994)),data = progresa)
coef(fit9)
##prediction 
range<- unique(progresa$avgpoverty ) 
yT.hat <- predict(fit9,newdata =data.frame(avgpoverty = range,treatment=1,pobtot1994=10000)) 
## predicted turnout rate under the control condition 
yC.hat <- predict(fit9,newdata =data.frame(avgpoverty = range,treatment =0,pobtot1994=10000))
dif<-yT.hat - yC.hat
## 每个数值的平均效应
df <- data.frame("平均贫困指数"=range,"平均干预效应"=dif)
df[order(range,decreasing = TRUE), ]
##画图 
plot(x = range, y = dif, xlab = "贫困指数",ylab = "平均干预效应")
##散点图
lines(a$平均贫困指数,a$平均干预效应,col="blue") 
##抛物线