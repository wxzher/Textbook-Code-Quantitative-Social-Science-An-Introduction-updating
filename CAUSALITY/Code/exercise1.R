##
## file :exercise 1.R
## author: wxz
## update: 2022-07-11
## the code for the exercise1 in Chapter2
##



# 早教小班化的有效性


##读取文件与概览文件数据
star <- read.csv("Datasets/star.csv")
dim(star)
head(star)

####  Question 1####

## 创建kinder新因子变量

unique(star$classtype)   # 返回一个把重复元素或行给删除的向量数据框

star$kinder <- NA 

# 为每个不同类型赋值
star$kinder[star$classtype == 1] <- "smallClass" 
star$kinder[star$classtype == 2] <- "general"
star$kinder[star$classtype == 3] <- "extracurricular" 

# 将向量转化为因子变量
star$kinder <- as.factor(star$kinder)


# 将种族变量重新编码；覆盖数据框中的原始变量而非创建新变量

unique(star$race)

star$race[star$race == 1] <- "white"
star$race[star$race == 2] <- "black"
star$race[star$race == 4] <- "hispanic"
star$race[star$race == 3 | star$race == 5 | star$race == 6 ] <- "others"

#### Question 2 ####

# 创建新的数据框

smallClass <- subset(star, star$kinder == "smallClass") 
generalClass <- subset(star, star$kinder == "general")

# 使用na.rm = TRUE，在操作发生之前，删除缺失数据
# 不同班级四年级阅读平均成绩

smallClassG4R.mean <- mean(smallClass$g4reading, na.rm = TRUE)
generalClassG4R.mean <- mean(generalClass$g4reading, na.rm = TRUE)

## 不同班级四年级数学平均成绩

smallClassG4M.mean <- mean(smallClass$g4math, na.rm = TRUE)
generalClassG4M.mean <- mean(generalClass$g4math, na.rm = TRUE)


## 展示同一科目的不同班级规模的考试的平均数

Reading <- data.frame(smallClassG4R.mean, generalClassG4R.mean)
colnames(Reading) <- c("small", "general")

Math <- data.frame(smallClassG4M.mean, generalClassG4M.mean)
colnames(Math) <- c("small", "general")

## 计算均值差
difmean <- data.frame(smallClassG4R.mean - generalClassG4R.mean,
                      smallClassG4M.mean - generalClassG4M.mean)
colnames(difmean) = c("Reading", "Math")

## 计算标准差
sd.Reading <- data.frame(sd(smallClass$g4reading, na.rm = TRUE), 
                         sd(generalClass$g4reading, na.rm = TRUE))
colnames(sd.Reading) = c("small", "general")

sd.Math <- data.frame(sd(smallClass$g4math, na.rm = TRUE), 
                      sd(generalClass$g4math, na.rm = TRUE))
colnames(sd.Math) = c("small", "general")
## 计算标准差的差值

difsd <- data.frame(sd.Reading[1,1] - sd.Reading[1,2], 
                    sd.Math[1,1] - sd.Math[1,2])
colnames(difsd) = c("Reading", "Math")



list("ReadingMean" = Reading, "MathMean" = Math, "difmean" = difmean,
     "Readingsd" = sd.Reading,"Mathsd" = sd.Math,"difsd" = difsd)


####  Question 3 #### 

##小班和普通班的高分和低分

## 阅读

smallClassRQ <- quantile(smallClass$g4reading, 
                probs = seq(from = 0.33, to = 0.66, by = 0.33), na.rm = TRUE)
smallClassRQ 


generalClassRQ <- quantile(generalClass$g4reading,
                probs = seq(from = 0.33, to = 0.66, by = 0.33), na.rm = TRUE)

generalClassRQ

smallClassRQ  - generalClassRQ


## 数学
smallClassMQ <- quantile(smallClass$g4math, 
                probs = seq(from = 0.33, to = 0.66, by = 0.33), na.rm = TRUE)
smallClassMQ 
generalClassMQ <- quantile(generalClass$g4math,
                probs = seq(from = 0.33, to = 0.66, by = 0.33), na.rm = TRUE)
generalClassMQ

smallClassMQ - generalClassMQ


#### Question 4 ####


## 创建列联表

table(class_size = star$kinder, year = star$yearssmall)

## 使用tapply(x2, x1, mean)计算对于x1的x2的平均数
## 使用tapply(x2, x1, median)计算对于x1的x2的中位数

## 在小班不同年数的学生的阅读平均数和中位数
tapply(star$g4reading, star$yearssmall, mean, na.rm = TRUE)
tapply(star$g4reading, star$yearssmall, median, na.rm = TRUE)

## 在小班不同年数的学生的数学的平均数和中位数
tapply(star$g4math, star$yearssmall, mean, na.rm = TRUE)
tapply(star$g4math, star$yearssmall, median, na.rm = TRUE)


#### Question 5 ####

## 在各个班级规模中，白人都有更高的成绩

## 正常大小班级中不同种族的四年级阅读平均分
tapply(generalClass$g4reading, generalClass$race, mean, na.rm = TRUE)

## 正常大小班级中不同种族的四年级数学平均分
tapply(generalClass$g4math, generalClass$race, mean, na.rm = TRUE)

## 小班中不同种族的四年级阅读平均分
tapply(smallClass$g4reading, smallClass$race, mean, na.rm = TRUE)

## 小班中不同种族的四年级数学平均分
tapply(smallClass$g4math, smallClass$race, mean, na.rm = TRUE)


#### Question 6 ####

## 分配给不同班级类型的学生的高中毕业率
tapply(star$hsgrad, star$kinder, mean, na.rm = TRUE)

## 小班学习年数的高中毕业率
tapply(star$hsgrad, star$yearssmall, mean, na.rm = TRUE)

## 不同种族之间的高中毕业率
tapply(star$hsgrad, star$race, mean, na.rm = TRUE)

