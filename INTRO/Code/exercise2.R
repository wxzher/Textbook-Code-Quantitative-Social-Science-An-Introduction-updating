##

## Update: 2022-07-26

## File: exercise2.R

## Author: WXZ

## the code addresses excerice 2 in Chapter1

##


#### 导入数据####

kenya <- read.csv("../Datasets/Kenya.csv")  # 读取肯尼亚的表格数据
sweden <-  read.csv("../Datasets/Sweden.csv")  # 读取瑞典的表格数据
world <- data.frame(read.csv("../Datasets/World.csv"))  # 读取世界的表格数据

summary(kenya)
summary(sweden)
summary(world)

#### QUESTION 1  计算CBR 粗出生率####

# 定义一个计算CBR的函数

CBRCalculate <- function(x){
  
  
  # 计算1950-1955年和2005-2010年的CBR
  #
  # Args:
  #   x: data.frame
  #  
  #
  # Returns:
  #   返回1950-1955年和2005-2010年的CBR数值
  #    
  x1 <- x$py.men[1:15] + x$py.women[1:15]   # 计算1950-1955年的寿命数（人-年）
  x1CBR <- sum(x$births[1:15]) / sum(x1)   # 计算1950-1955年的粗出生率
  x2 <- x$py.men[16:30] + x$py.women[16:30]  # 计算2005-2010年的寿命数（人-年 
  x2CBR <- sum(x$births[16:30]) / sum(x2)   # 计算2005-2010年的粗出生率
  xCBR <- c(x1CBR, x2CBR)
  return(xCBR)
  
}

## 肯尼亚、瑞典和全世界的CBR ##

kenCBR <- CBRCalculate(kenya)
sweCBR <- CBRCalculate(sweden)
worCBR <- CBRCalculate(world)

# 数据放一起比较

year <- c("1950-1955","2005-2010")
CBR <- data.frame(year,kenCBR,sweCBR,worCBR)
CBR

## conclusion ##

# 肯尼亚的粗出生率明显高于瑞典和世界平均水平，说明肯尼亚出生率很高
# 瑞典的粗出生率低于世界平均水平，因为瑞典是发达国家，老龄化问题比较严重
# 随着经济的发展和教育水平的提升，世界的平均粗出生率都在下降。


#### QUESTION 2 计算年龄生育率#####



# 妇女的年龄区间一般为[15,50)

# 定义一个计算ASFR-年龄生育率的函数

ASFRCalculate <- function(x){
  
  # 计算ASFR
  #
  # Args:
  #   x: data.frame
  #  
  #
  # Returns:
  #   返回numeric 表示ASFR
  #    
  
  return(x$births/ x$py.women)
  
}

# 定义一个画图函数，后面方便使用

PlotDraw <- function(num1, num2, num3, y1, y2, cha1, cha2,cha3, cha4){

  # 画两条折线图
  #
  # Args:
  #   num1: numeric , 表示x轴数据的个数
  #   num2、num3:y轴取值范围
  #   y1:vector, y1的取值
  #   y2:vector, y2的取值
  #   cha1:character, y1的名称
  #   cha2:character, y2的名称
  #   cha3:character, y轴的名称
  #   cha4:character, 图名称
  #
  # Returns:
  #   没有
  #    
  

ID <- c(1:num1)

my_frame <- data.frame(ID, y1, y2)

plot(y1~ID, pch=15, col="DarkTurquoise", ylim=c(num2, num3), ylab=cha3, main = cha4)
##pch表示散点用什么形状表示，col表示颜色，ylim表示Y轴范围，ylab表示Y轴标题，main表示图片标题

points(ID, y2, pch=16, col="DeepPink", cex=1)#cex表示散点的大小

lines(y1, col="DarkTurquoise", lty=1)  # lty=1表示用实线连起来
lines(y2, col="DeepPink", lty=2)  # lty=2表示用虚线连起来

legend("topleft", c(cha1, cha2), col=c("DarkTurquoise", "DeepPink"), 
       text.col=c("DarkTurquoise","DeepPink"), pch=c(15,16),lty=c(1,2))
# 12表示x轴坐标为12,400表示y轴坐标为400，意思为图例的左边和上边界限，text.col表示图例文本颜色
}


kenASFR <- ASFRCalculate(kenya)  # 肯尼亚的ASFR 

sweASFR <- ASFRCalculate(sweden)  # 瑞典的ASFR 

worASFR <- ASFRCalculate(world)   # 世界的ASFR 

# 写入数据

kenya$ASFR  <- kenASFR
sweden$ASFR <- sweASFR 
world$ASFR  <- worASFR

# 三个放在一起进行比较

ASFR1 <- data.frame(age=kenya$age[4:10], kenASFR[4:10], sweASFR[4:10], worASFR[4:10])
colnames(ASFR1) <- c("age", "kenya", "sweden", "world")

ASFR2 <-  data.frame(age=kenya$age[19:25], kenASFR[19:25], sweASFR[19:25], worASFR[19:25])
colnames(ASFR2) <- c("age", "kenya", "sweden", "world")

list("1950-1955年ASFR" = ASFR1, "2005-2010年ASFR" = ASFR2)

# 画图
PlotDraw(7, 0, 0.4, kenASFR[4:10], kenASFR[19:25], "1950-1955", "2005-2010", "ASFR", "kenASFR" )
PlotDraw(7, 0, 0.15, sweASFR[4:10], sweASFR[19:25], "1950-1955", "2005-2010", "ASFR", "sweASFR" )
PlotDraw(7, 0, 0.3, worASFR[4:10], worASFR[19:25], "1950-1955", "2005-2010", "ASFR", "worASFR" )

## conclusion ##
# 1950年-1955年肯尼亚20-40的女性有20%以上的生育率，20-30岁甚至到达35%
# 35-40岁属于高龄产妇，会有生育风险。
# 肯尼亚妇女的生育率很高。
# 瑞典的生育率普遍不高。


#### QUESTION 3 计算妇女的平均子女数####



# 定义一个计算TFR的函数

TFRCalculate <- function(x){
  
  # 计算TFR
  #
  # Args:
  #   x: data.frame 
  #  
  #
  # Returns:
  #   返回1950-1955年和2005-2010年的TFR数值
  #    
  
  xTFR <- c(sum(x[4:10] * 5), sum(x[19:25] * 5))
  
  return(xTFR)
  
}

# 计算TFR值

kenTFR <- TFRCalculate(kenASFR)
kenTFR # [1] 7.591410 4.879568

sweTFR <- TFRCalculate(sweASFR)
sweTFR # [1] 2.226917 1.902764

worTFR <- TFRCalculate(worASFR)
worTFR # [1] 5.007248 2.543623

# 三个数据放一起
data.frame(year,kenTFR,sweTFR,worTFR)


## 全世界女性数量的变化 ##

worWomenChange = sum(world$py.women[16:30]) - sum(world$py.women[1:15])
worWomenChange 
 
## 世界上出生的总人数 ## 

sum(world$births[1:15]) # 1950-1955年出生总人数

sum(world$births[16:30]) # 2005-2010年出生总人数


#### QUSTION 4 计算粗死亡率 ####


# 定义一个计算CDR的函数

CDRCalculate <- function(x){
  # 计算CDR
  #
  # Args:
  #   x: data.frame 
  #  
  #
  # Returns:
  #   返回1950-1955年和2005-2010年的CDR数值
  #    
  x1 <- x$py.men[1:15] + x$py.women[1:15]   # 计算1950-1955年的寿命数（人-年）
  x1CDR <- sum(x$deaths[1:15]) / sum(x1)   # 计算1950-1955年的粗死亡率
  
  x2 <- x$py.men[16:30] + x$py.women[16:30]  # 计算2005-2010年的寿命数（人-年 
  x2CDR <- sum(x$deaths[16:30]) / sum(x2)   # 计算2005-2010年的粗死亡率
  
  xCDR <- c(x1CDR, x2CDR)
  return(xCDR)
  
}

# 计算粗死亡率CDR 

kenCDR <- CDRCalculate(kenya)
kenCDR

sweCDR <- CDRCalculate(sweden)
sweCDR

worCDR <- CDRCalculate(world)
worCDR

# 三个数据放一起

data.frame(year, kenCDR, sweCDR, worCDR)

## conclusion ##

# 肯尼亚的死亡率高于瑞典和世界水平，瑞典的死亡率低于世界水平
# 1950-1955年肯尼亚的死亡率明显高出瑞典两倍以上，2005-2010年差距并不是很大
# 瑞典的死亡率稳定在1%以下
# 随着医疗水平的进步，人类对疾病的抵抗力和认知度增强，死亡率也逐渐下降。


#### QUSTION 5 计算ASDR各年龄组的死亡率####

# ASDR = 死亡人数 / 寿命数（人-年）


# 定义一个函数计算2005-2010年的ASDR 

ASDRCalculate <- function(x){
  
  # 计算ASDR
  #
  # Args:
  #   x: data.frame 
  #  
  #
  # Returns:
  #   返回2005-2010年的ASDR数值
  #    
  
  x2 <- x$py.men[16:30] + x$py.women[16:30]  # 计算2005-2010年的寿命数（人-年）
  xASDR <- x$deaths[16:30] / x2   # 计算2005-2010年的ASDR
  
  return(xASDR)
  
}

# 计算2005-2010年的ASDR 

kenASDR <- ASDRCalculate(kenya)
kenASDR

sweASDR <- ASDRCalculate(sweden)
sweASDR

# 两个数据放一起

data.frame(kenASDR,sweASDR)

## 画图 ##

PlotDraw(15, 0, 0.18, kenASDR, sweASDR, "kenASDR", "sweASDR", "ASDR", "ken-swe-ASDR" )

## conclusion ##

# 肯尼亚婴幼儿死亡率和中年死亡率远高于瑞典。可能与2008年的选举骚乱有关
# 2007年１２月底肯尼亚举行总统选举，肯选举委员会宣布总统姆瓦伊·齐贝吉成功连任，但主要反对党“橙色民主运动”指责其舞弊，拒绝接受选举结果。随后，肯尼亚各地爆发大规模骚乱，造成上千人死亡、３０多万人无家可归。
# 瑞典是一个老龄化社会，中老年的死亡率较高。


#### QUESTION 6 计算肯尼亚的CDR反事实 ####


# 定义一个函数计算2005-2010年的人口分布比例
PCalculate <- function(x){
  # 计算2005-2010年的P人口分布比例
  #
  # Args:
  #   x: data.frame 
  #  
  #
  # Returns:
  #   返回2005-2010年的P数值
  #    
  P <- (x$py.men[16:30] + x$py.women[16:30]) / sum(x$py.men[16:30] + x$py.women[16:30])
  return(P)
  
}

# 计算瑞典2005-2010年的人口分布比例
sweP <- PCalculate(sweden)


# 计算肯尼亚的反事实CDR

kenSweCDR  <-  sum(kenASDR * sweP)
kenSweCDR # 肯尼亚的反事实为0.02321646 原来的CDR值为0.01038914

# 为了找到合理的解释，我们需要对比一下人口分布
# 计算肯尼亚2005-2010年的人口分布比例

kenP <- PCalculate(kenya)

# 放在一起

data.frame(age = kenya$age[16:30], sweP, kenP)


## conclusion ##

# 我们可以发现在肯尼亚原来的人口分布属于金字塔型
# 青少年占比很高，超过了人口的一半，而45岁以上的人口甚至不到10%
# 因此即使肯尼亚的中老年死亡率高，但是加权来看对整体的影响不大。
# 但是在瑞典的人口分布中，基本是比较平均的，人口分布属于倒金字塔。
# 所以在肯尼亚的反事实CDR中高死亡率的中老年群体的权重占比增大了，结果会明显高于原来的。

# 保存

save.image("exercise2.RData")


