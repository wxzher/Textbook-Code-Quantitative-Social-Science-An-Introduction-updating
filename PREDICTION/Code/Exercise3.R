## 巴西政府转移和减少贫困率

## 导入数据
transfer <- read.csv("Datasets/transfer.csv")

## question 1 ###
## 假设我们这些选取的观测值在其他维度都是相似的

#### question 2####
View(transfer)
## 10188、13584、16980
transfer$pop82.per1 <- ((transfer$pop82 - 10188) / 10188 )* 100 
transfer$pop82.per2 <- ((transfer$pop82 - 13584) / 13584)* 100 
transfer$pop82.per3 <- ((transfer$pop82 - 16980) / 16980)* 100 

## question 3 #####

## 计算效果
transfer$educ.margin <- transfer$educ91 - transfer$educ80
transfer$poverty.margin <- transfer$poverty91 - transfer$poverty80
## 挑选变量
city.value1 <- subset(transfer, subset = (transfer$pop82.per1 <= 3 & transfer$pop82.per1 >= -3 ) )
city.value2 <- subset(transfer, subset = (transfer$pop82.per2 <= 3 & transfer$pop82.per2 >= -3 ) )
city.value3 <- subset(transfer, subset = (transfer$pop82.per3 <= 3 & transfer$pop82.per3 >= -3 ) )

##计算关系

# 教育

educ.fit1 <- lm(educ.margin ~ pop82.per1, data = city.value1[city.value1$pop82.per1 < 0, ])
educ.fit2 <- lm(educ.margin ~ pop82.per1, data = city.value1[city.value1$pop82.per1 > 0, ])

coef(educ.fit1)
coef(educ.fit2)

## 识字率
liter.fit1 <- lm(literate91 ~ pop82.per1, data = city.value1[city.value1$pop82.per1 < 0, ])
liter.fit2 <- lm(literate91 ~ pop82.per1, data = city.value1[city.value1$pop82.per1 > 0, ])
coef(liter.fit1)
coef(liter.fit2)

## 贫困程度
poverty.fit1 <- lm(poverty.margin ~ pop82.per1, data =city.value1[city.value1$pop82.per1 < 0, ] )
poverty.fit2 <- lm(poverty.margin ~ pop82.per1, data =city.value1[city.value1$pop82.per1 > 0, ] )

## 