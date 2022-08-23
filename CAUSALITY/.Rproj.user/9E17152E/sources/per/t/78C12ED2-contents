##
## file: exercise3
## author: wxz
## update: 2022-08-02
## the code for exercise3 in Chapter2
##

## 刺杀领导人成功率的自然实验

# 假设刺杀企图的成功和失败被认为是随机的。

## 导入数据
leaders <- read.csv("Datasets/leaders.csv")
## 数据描述
dim(leaders)
summary(leaders)
head(leaders)

#### Question 1 ####

country <- leaders$country
## 刺杀企图的次数
length(country)
# 250次

## 多少国家至少遇到一次领导人刺杀

unique(leaders$country)

length(unique(leaders$country))

## 这些国家中每年刺杀的平均次数

range(leaders$year)
year.gap <- max(leaders$year) - min(leaders$year)  # 获取年的跨度

## 计算公式为 刺杀总数 / 年份数
table(leaders$country) / year.gap

#### Question 2 ####

unique(leaders$result) # 观察结果的分类

## 创建新的二元变量

leaders$success <- NA

## 赋值
leaders$success[(leaders$result== "dies within a day after the attack" )| 
                  (leaders$result=="dies between a week and a month" )  | 
                  (leaders$result=="dies, timing unknown" )|
                  (leaders$result=="dies between a day and a week") ] <- 1

leaders$success[(leaders$result != "dies within a day after the attack" )& 
                  (leaders$result!="dies between a week and a month" )  & 
                  (leaders$result!="dies, timing unknown" )&
                  (leaders$result!="dies between a day and a week") ] <- 0

## 总体成功率
mean(leaders$success)

##### Question 3 #####

## 前三年的政权平均分数是否与暗杀企图的成功率有关。
politybe.success <- tapply(leaders$politybefore, leaders$success, mean)
politybe.success

## 暗杀成功与否与年龄是否有关系
tapply(leaders$age,leaders$success, mean)

#### Question 4 ####

## 创建新的数据框
## 赋值

leaders$warbefore <- ifelse(leaders$interwarbefore == 1 |leaders$civilwarbefore == 1, 1, 0)
leaders$warbefore

## 刺杀之前的三年内是否参加国内或国际战争是否与暗杀企图的成功率有关。
warbefore.success <- tapply(leaders$warbefore, leaders$success, mean)
warbefore.success

## 感觉相关性不是很大

####  Question 5 ####

## 计算刺杀成功与失败两种情况的后三年平均政权得分差值
afterPolity.mean <- tapply(leaders$polityafter, leaders$success, mean)
beforePolity.mean <- tapply(leaders$politybefore, leaders$success, mean)
## 计算前后平均政权得分差值
difPolity.mean <- afterPolity.mean - beforePolity.mean  

## 用数据框展示前后及之间差值的数据
data.frame(beforePolity.mean, afterPolity.mean, difPolity.mean)


## 新建刺杀后国内外战争是否发生的二元变量
leaders$warafter <- ifelse(leaders$interwarafter == 1 | leaders$civilwarafter == 1, 1, 0)
leaders$warafter

beforeWar.mean <- mean(leaders$warbefore[leaders$success == 1])
afterWar.mean <-  mean(leaders$warafter[leaders$success == 1])

difWar.mean <- afterWar.mean - beforeWar.mean

data.frame(beforeWar.mean, afterWar.mean, difWar.mean)