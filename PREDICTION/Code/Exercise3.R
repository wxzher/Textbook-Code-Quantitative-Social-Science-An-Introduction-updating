## 巴西政府转移和减少贫困率

## 导入数据
transfer <- read.csv("../Datasets/transfer.csv")

#### question 1 ####
## 假设我们这些选取的观测值在其他维度都是相似的
## 有点：有效利用现实约束条件分析变量之间因果关系；避免参数估计的内生 性问题；结果十分接近于随机实验结果
## 缺点：对数据形式有要求；外部效度低，其结果可能无法推广到远离不连续 点位置的观察值。
#### question 2####
View(transfer)
## 10188、13584、16980三个点的阈值如何进行标准化
## 找到第一个点和第二个点的中间值，第二个点和第三个点的中间值 
## 观察其距离哪个中间值最近，分成三段
pop82 <- transfer$pop82
## 利用差值计算百分比数，对人口数进行标准化
pop.standard <- rep(NA, length(pop82))
for(i in 1:length(pop82)){
  p1 <- 10188
  p2 <- 13584
  p3 <- 16980
  s1 <- (p1 + p2) / 2
  s2 <- (p2 + p3) / 2
  if(pop82[i] <= s1 ){
    pop.standard[i] <- (pop82[i] - p1) / p1
  }
  else if(pop82[i] >= s2){
    pop.standard[i] <- (pop82[i] - p3) / p3
  }
  else{
    pop.standard[i] <- (pop82[i] - p2) / p2
  }
  
}
pop.standard <- pop.standard * 100
transfer$pop.standard <- pop.standard 

## question 3 #####
## 平均因果效应计算，可以采用回归方式进行计算
df.city <- subset(transfer, abs(pop.standard) <=3)
## 教育平均因果效应
educ.fit1 <- lm(educ91 ~ pop.standard, data = df.city[df.city$pop.standard < 0,])
coef(educ.fit1)
educ.fit2 <- lm(educ91  ~ pop.standard, data = df.city[df.city$pop.standard > 0,])
coef(educ.fit2)
## 预测
educ.pred1 <- predict(educ.fit1, newdata = data.frame(pop.standard = c(-1,0))) 
educ.pred2 <- predict(educ.fit2, newdata = data.frame(pop.standard = c(0,1)))
  
## 断点处的值的差为平均因果效应，断点处为0
educ.pred2[1] -educ.pred1[2]
## 结果为0.5627598 
## 贫困率平均因果效应
pover.fit1 <- lm(poverty91~ pop.standard, data = df.city[df.city$pop.standard < 0,])
coef(pover.fit1)
pover.fit2 <- lm(poverty91 ~ pop.standard, data = df.city[df.city$pop.standard > 0,])
coef(pover.fit2)
## 预测
pover.pred1 <- predict(pover.fit1, newdata = data.frame(pop.standard = c(-1,0))) 
pover.pred2 <- predict(pover.fit2, newdata = data.frame(pop.standard = c(0,1)))

## 断点处的值的差为平均因果效应，断点处为0
pover.pred2[1] -pover.pred1[2]
## 结果为-0.06079046 
## 识字率平均因果效应
liter.fit1 <- lm(literate91~ pop.standard, data = df.city[df.city$pop.standard < 0,])
coef(liter.fit1)
liter.fit2 <- lm(literate91 ~ pop.standard, data = df.city[df.city$pop.standard > 0,])
coef(liter.fit2)
## 预测
liter.pred1 <- predict(liter.fit1, newdata = data.frame(pop.standard = c(-1,0))) 
liter.pred2 <- predict(liter.fit2, newdata = data.frame(pop.standard = c(0,1)))

## 断点处的值的差为平均因果效应，断点处为0
liter.pred2[1] -liter.pred1[2]
## 结果为0.05447161

####question 4 ####
## 可视化
## 定义一个函数
getPlot <- function(y,ylab, pred1, pred2){
  plot(df.city$pop.standard, y,xlab = "人口阈值接近程度",ylab = ylab ,xlim = c(-1,1))
  abline(v = 0, lty = "dashed")
  lines(c(-1,0), pred1, col = "blue")
  lines(c(0,1), pred2, col = "blue")
}

getPlot(df.city$educ91, "受教育程度", educ.pred1,educ.pred2)
getPlot(df.city$poverty91, "贫困率", pover.pred1,pover.pred2)
getPlot(df.city$literate91, "识字率", liter.pred1,liter.pred2)
#### question 5 ####

## 受教育年数均值差异
mean(transfer$educ91[transfer$pop.standard > 0],
     na.rm = TRUE) - mean(transfer$educ91[transfer$pop.standard <0],na.rm = TRUE)
## 贫困率均值差异
mean(transfer$poverty91[transfer$pop.standard > 0],
     na.rm = TRUE) - mean(transfer$poverty91[transfer$pop.standard <0],na.rm = TRUE)
## 识字率均值差异
mean(transfer$literate91[transfer$pop.standard > 0],
     na.rm = TRUE) - mean(transfer$literate91[transfer$pop.standard <0],na.rm = TRUE)
#### question 6 ####
## 五个百分点分析宽度重复问题
df.city.new <- subset(transfer, abs(pop.standard) <=5)
## 教育平均因果效应
educ.fit1.new <- lm(educ91 ~ pop.standard, data = df.city.new[df.city.new$pop.standard < 0,])
coef(educ.fit1.new)
educ.fit2.new <- lm(educ91  ~ pop.standard, data = df.city.new[df.city.new$pop.standard > 0,])
coef(educ.fit2.new)
## 预测
educ.newpred1 <- predict(educ.fit1.new, newdata = data.frame(pop.standard = c(-5:0))) 
educ.newpred2 <- predict(educ.fit2.new, newdata = data.frame(pop.standard = c(0:5)))

## 断点处的值的差为平均因果效应，断点处为0
educ.newpred2[1] -educ.newpred1[2]
## 结果为0.6751517 

## 贫困率平均因果效应
pover.fit1.new <- lm(poverty91 ~ pop.standard, data = df.city.new[df.city.new$pop.standard < 0,])
coef(pover.fit1.new)
pover.fit2.new <- lm(poverty91  ~ pop.standard, data = df.city.new[df.city.new$pop.standard > 0,])
coef(pover.fit2.new)
## 预测
pover.newpred1 <- predict(pover.fit1.new, newdata = data.frame(pop.standard = c(-5:0))) 
pover.newpred2 <- predict(pover.fit2.new, newdata = data.frame(pop.standard = c(0:5)))

## 断点处的值的差为平均因果效应，断点处为0
pover.newpred2[1] -pover.newpred1[2]
## 结果为-0.08247134

## 识字率平均因果效应
liter.fit1.new <- lm(literate91 ~ pop.standard, data = df.city.new[df.city.new$pop.standard < 0,])
coef(liter.fit1.new)
liter.fit2.new <- lm(literate91  ~ pop.standard, data = df.city.new[df.city.new$pop.standard > 0,])
coef(liter.fit2.new)
## 预测
liter.newpred1 <- predict(liter.fit1.new, newdata = data.frame(pop.standard = c(-5:0))) 
liter.newpred2 <- predict(liter.fit2.new, newdata = data.frame(pop.standard = c(0:5)))

## 断点处的值的差为平均因果效应，断点处为0
liter.newpred2[1] -liter.newpred1[2]
## 结果为0.06386408

list("educ" = data.frame("高于人口阈值" = educ.newpred1,
      "低于人口阈值"=  educ.newpred2),"pover" = data.frame("高于人口阈值" = pover.newpred1,
      "低于人口阈值"=  pover.newpred2),"liter" = data.frame("高于人口阈值" = liter.newpred1,
                                                       "低于人口阈值"=  liter.newpred2))
getPlot2 <- function(y,ylab, pred1, pred2){
  plot(df.city.new$pop.standard, y,xlab = "人口阈值接近程度",ylab = ylab ,xlim = c(-5,5))
  abline(v = 0, lty = "dashed")
  lines(c(-5:0), pred1, col = "blue")
  lines(c(0:5), pred2, col = "blue")
}
getPlot2(df.city.new$educ91, "受教育程度", educ.newpred1,educ.newpred2)
getPlot2(df.city.new$poverty91, "贫困率", pover.newpred1,pover.newpred2)
getPlot2(df.city.new$literate91, "识字率", liter.newpred1,liter.newpred2)
#### question 7 #####
##分析过程与问题 3 相同，使用数据变为 1980 年的贫困率与受教育程 
##教育程度平均因果效应
educ.fit3 <- lm(educ80 ~ pop.standard, data = df.city[df.city$pop.standard < 0,])
coef(educ.fit3)
educ.fit4 <- lm(educ80  ~ pop.standard, data = df.city[df.city$pop.standard > 0,])
coef(educ.fit4)
## 预测
educ.pred3 <- predict(educ.fit3, newdata = data.frame(pop.standard = c(-1,0))) 
educ.pred4 <- predict(educ.fit4, newdata = data.frame(pop.standard = c(0,1)))

## 断点处的值的差为平均因果效应，断点处为0
educ.pred4[1] -educ.pred3[2]
## 结果为0.1967105 
##贫困率平均因果效应
pover.fit3 <- lm(poverty80 ~ pop.standard, data = df.city[df.city$pop.standard < 0,])
coef(pover.fit3)
pover.fit4 <- lm(poverty80  ~ pop.standard, data = df.city[df.city$pop.standard > 0,])
coef(pover.fit4)
## 预测
pover.pred3 <- predict(pover.fit3, newdata = data.frame(pop.standard = c(-1,0))) 
pover.pred4 <- predict(pover.fit4, newdata = data.frame(pop.standard = c(0,1)))

## 断点处的值的差为平均因果效应，断点处为0
pover.pred4[1] -pover.pred3[2]
## 结果为-0.02423826

## 与人口阈值接近程度因子本身对该地区的受教育程度和贫困率存在一定的因果效应，所以第三题中的分析有效性不高。