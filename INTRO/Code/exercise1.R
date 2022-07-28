##

## Update: 2022-07-26

## File: exercise1.R

## Author: WXZ

## the code addresses excerice 1 in Chapter1

##

## 导入数据
turnout <- read.csv("../Datasets/turnout.csv")


#### QUETION1 ####

dim(turnout)  # 输出数据的维度
summary(turnout)  # 观察数据的统计量
turnout # 观测值
year <- turnout$year
year.gap <- range(year)  # 覆盖年份
year.gap 


#### QUETION 2 ####

# 计算总符合投票年龄的投票人数

totalVoters <- turnout$VAP + turnout$overseas


# 计算VAP+oversea的投票率

vapPercentage <- (turnout$total / totalVoters) * 100
vapPercentage

# 用vepPercentage表示投票率

vepPercentage <-  (turnout$total / turnout$VEP) * 100
vepPercentage

# 计算两者的差值

difPercentage <-  vapPercentage - vepPercentage

# 计算两者差值的统计量，比如平均值，方差和标准差

summary(difPercentage)
sd(difPercentage)   # 计算标准差
var(difPercentage)  # 计算方差
  
# 两个数据放一起进行比较
Per <- data.frame(year,vapPercentage,vepPercentage)
Per

## conclusion ##
# vep（符合投票条件人数）的投票率明显高于vap(符合投票年龄人数)的投票率
# 或许说明存在一部分人是符合年龄但是不符合条件，比如罪犯、非公民等等。

#### QUESTION3 ####

# 定义两个函数可以计算差值和统计量

DifPerCalculate <- function(x, y){
  # 计算两个数据的差和它们的差之间的平均值、范围、方差和极差
  #
  # Args:
  #   x: numeric
  #   y: numeric
  #
  # Returns:
  #   返回数据差dif
  #    
  
  return(x - y)
  
  
}
DifPerSummaryCal <- function(dif){
  # 计算两个数据的差的平均值、最大值、最小值、标准差和方差
  #
  # Args:
  #   向量vector
  #
  # Returns:
  #   返回平均值、最大值、最小值、标准差和方差
  
  dif.mean <- mean(dif)
  dif.max <- max(dif)
  dif.min <- min(dif)
  dif.var <- var(dif)
  dif.sd <- sd(dif)
  dif.summary <- c(dif.mean, dif.max, dif.min, dif.sd, dif.var)
  names(dif.summary) <- c("mean", "max","min", "sd", "var")
  return(dif.summary)
}


# 计算vap与anes投票率的差异

difAnesVap <- DifPerCalculate(turnout$ANES, vapPercentage)
difAnesVap

# 计算差值的平均值、最大值、最小值、标准差和方差

DifPerSummaryCal(difAnesVap)

# 计算vep与anes投票率的差异

difAnesVep <- DifPerCalculate(turnout$ANES, vepPercentage)
difAnesVep

# 计算差值的平均值、最大值、最小值、标准差和方差

DifPerSummaryCal(difAnesVep)


# 放在一起进行比较
data.frame(year, anes = turnout$ANES, vapPercentage,difAnesVap)

data.frame(year, anes = turnout$ANES, vepPercentage,difAnesVep)





## conclusion ##
# anes的投票率远远高于vap和vep的投票率
# 


#### QUESTION 4 ####

## PS ##
# 中期选举的年份与总统大选的年份相间隔
# 1980年之后的奇数位都是总统大选年
# 1982年之后的偶数位都是中期选举年
# 因为少了2006年的数据，所以第14个数据是2008年，是总统大选年


## 总统中期选举##

# 获取vep的投票率数据

vepPercentageCenter <- vepPercentage[seq(from = 2, to=13, by=2)]
vepPercentageCenter


# 获取anes的投票率数据

anesCenter <- turnout$ANES[seq(from = 2, to=13, by=2)]
anesCenter


## 总统大选年##

# 获取vep投票率数据

vepPercentagePre <- c(vepPercentage[seq(from = 1, to=14,by=2)], vepPercentage[14])
vepPercentagePre


# 获取anes投票率数据

anesPre <- c(turnout$ANES[seq(from = 1, to=14,by=2)], turnout$ANES[14])
anesPre


# 计算差值

difCenterAnesVep <- DifPerCalculate(anesCenter, vepPercentageCenter)
difCenterAnesVep

difPreAnesVep <- DifPerCalculate(anesPre, vepPercentagePre)
difPreAnesVep

# 计算差值的平均值、最大值、最小值、均值、标准差和方差

DifPerSummaryCal(difCenterAnesVep)

DifPerSummaryCal(difPreAnesVep)

## conclusion ##
# 发现anes大选年的时候误差要大于中期选举年。


#### QUESTION 5 ####


# 上半段

difAnesVep1 <-  DifPerCalculate(turnout$ANES[1:7], vepPercentage[1:7])
difAnesVep1


# 下半段

difAnesVep2 <-  DifPerCalculate(turnout$ANES[8:14], vepPercentage[8:14])
difAnesVep2

# 计算difAnesVep1和difAnesVep2的统计量

DifPerSummaryCal(difAnesVep1)

DifPerSummaryCal(difAnesVep2)

## 画图 ##

ID<-c(1:7)
my_frame<-data.frame(ID, difAnesVep1, difAnesVep2)
plot(difAnesVep1~ID,pch=15,col="DarkTurquoise",ylim=c(8,23),ylab="dif")
## pch表示散点用什么形状表示，col表示颜色，ylim表示Y轴范围，ylab表示Y轴标题，main表示图片标题

points(ID, difAnesVep1, pch=16, col="DeepPink",cex=1)  # cex表示散点的大小

lines(difAnesVep1, col="DarkTurquoise", lty=1)  # lty=1表示用实线连起来
lines(difAnesVep2, col="DeepPink", lty=2)  # lty=2表示用虚线连起来

legend("topleft",c("difAnesVep1","difAnesVep2"),
       col=c("DarkTurquoise","DeepPink"),text.col=c("DarkTurquoise","DeepPink"),pch=c(15,16),lty=c(1,2))
# 12表示x轴坐标为12,400表示y轴坐标为400，意思为图例的左边和上边界限，text.col表示图例文本颜色


## conclusion ##
# 发现误差随着时间而增加


###  QUESTION 6  ####


# 获取VAP2008年调整后的人数

vap2008New <-  turnout$VAP[14] - turnout$felons[14] - turnout$noncit[14]

# 计算VAP2008年调整后的投票率


vapPercentage2008New <-  ((turnout$total[14] - turnout$osvoters[14])  / vap2008New) * 100
vapPercentage2008New


# 原来2008年vap投票率的值

vapPercentage2008Old <- vapPercentage[14]

vapPercentage2008Old 

vepPercentage2008 <-  vepPercentage[14]
vepPercentage2008 

anes2008 <-  turnout$ANES[14]
anes2008 


# 同样把数据放在一起显示
data.frame(vapPercentage2008New,vapPercentage2008Old,vepPercentage2008,anes2008)

# 计算差值

dif2008 <- data.frame(DifPerCalculate(vapPercentage2008New,vapPercentage2008Old), 
                      DifPerCalculate(vapPercentage2008New,vepPercentage2008),
                      DifPerCalculate(vapPercentage2008New,anes2008) )
names(dif2008) <- c("NewOld", "NewVep", "NewAnes")
dif2008

## conclusion ##
# 调整后的vap投票率提高了，但是仍旧小于anes的投票。

# 保存
save.image("exercise1.RData")
