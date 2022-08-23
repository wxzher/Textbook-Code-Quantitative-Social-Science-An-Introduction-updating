##
## file: exercise2
## author: wxz
## update: 2022-07-31
## the code for exercise2 in Chapter2
##

## 改变对同性恋婚姻的看法

# 获取数据

gay <- read.csv("Datasets/gay.csv") 

# 日常对数据进行简单的观察

dim(gay)
summary(gay)
head(gay)

#### Question 1 ####

# 观察treatment的值
unique(gay$treatment)



# 获取第一项研究的三组的数据值
NoContact1 <- subset(gay, subset =((gay$study == 1)& 
                                     (gay$treatment == "No Contact")))

SSMbyGay1 <- subset(gay, subset =((gay$study == 1)& 
              (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser")))

SSMbyStra1<- subset(gay, subset =((gay$study == 1)& 
               (gay$treatment == "Same-Sex Marriage Script by Straight Canvasser")))

# 分析数据

summary(NoContact1)
summary(SSMbyGay1)
summary(SSMbyStra1)


# 干预分配的随机化是否实现
# 获取第一项研究的第一次干预前的数据
Study1.wave1 <- subset(gay, subset = ((gay$study == 1)& 
                                        (gay$wave == 1)))
tapply(Study1.wave1$ssm, Study1.wave1$treatment, mean)

# 差距也不是非常大，这说明干预随机分配使得这五组调查对象基本相同。

#### Question 2 ####
## 计算控制组的两次访谈的结果差异
NoContact1.wave1.mean <- mean(NoContact1$ssm[NoContact1$wave == 2]) - 
  mean(NoContact1$ssm[NoContact1$wave == 1])
NoContact1.wave1.mean

# 获取第一个研究数据中同性恋宣传对同性恋话题第一次和第二次结果的效果

SSMbyGay1.wave1 <- SSMbyGay1$ssm[SSMbyGay1$wave == 1]

# 计算平均值
mean(SSMbyGay1.wave1)

SSMbyGay1.wave2 <- SSMbyGay1$ssm[SSMbyGay1$wave == 2]

# 计算平均值
mean(SSMbyGay1.wave2)

# 计算DID
DiD.gay1wave2 <- (mean(SSMbyGay1.wave2) - mean(SSMbyGay1.wave1)) - NoContact1.wave1.mean
DiD.gay1wave2

# 获取第一个研究数据中异性恋宣传对同性恋话题第一次和第二次结果的效果
SSMbyStra1.wave1 <- SSMbyStra1$ssm[SSMbyStra1$wave == 1]

# 计算平均宣传值
mean(SSMbyStra1.wave1)

SSMbyStra1.wave2 <- SSMbyStra1$ssm[SSMbyStra1$wave == 2]

# 计算平均值
mean(SSMbyStra1.wave2)

# 计算DiD
DiD.stra1wave2 <- (mean(SSMbyStra1.wave2) - mean(SSMbyStra1.wave1)) - NoContact1.wave1.mean
DiD.stra1wave2
## 计算差值
DiD.gay1wave2 - DiD.stra1wave2
#### Question 3 ####
# 获取同性恋和异性恋鼓励回收废品的脚本的数据
RecyclebyGay1 <- subset(gay, subset = ((gay$study == 1)&
                        gay$treatment == "Recycling Script by Gay Canvasser") )
RecyclebyStra1<- subset(gay, subset = ((gay$study == 1)&
                        gay$treatment == "Recycling Script by Straight Canvasser") )


# 获取第二次访谈的结果

RecyclebyGay1.wave2 <- RecyclebyGay1$ssm[RecyclebyGay1$wave == 2]
mean(RecyclebyGay1.wave2)

RecyclebyStra1.wave2 <- RecyclebyStra1$ssm[RecyclebyStra1$wave == 2]
mean(RecyclebyStra1.wave2)
## 获取第一次访谈结果
RecyclebyGay1.wave1 <- RecyclebyGay1$ssm[RecyclebyGay1$wave == 1]
mean(RecyclebyGay1.wave1)

RecyclebyStra1.wave1 <- RecyclebyStra1$ssm[RecyclebyStra1$wave == 1]
mean(RecyclebyStra1.wave1)

# 比较
# 回收废品话题中的同性恋宣传者DID
DiD.RGay1wave2 <- (mean(RecyclebyGay1.wave2) - mean(RecyclebyGay1.wave1)) - NoContact1.wave1.mean
DiD.RGay1wave2
# 同性恋话题中异性恋宣传者DID
DiD.Rstar1wave2 <- (mean(RecyclebyStra1.wave2) - mean(RecyclebyStra1.wave1)) - NoContact1.wave1.mean
DiD.Rstar1wave2

# 比较
DiD.gay1wave2


#### Question 4 ####

# 定义一个函数
# 计算控制组的mean
NoContact1.mean <- data.frame(mean = tapply(NoContact1$ssm, NoContact1$wave, mean))
NoContact1.mean

getSSMeffect <- function(x){
  ## 
  ## Args: data.frame
  ## return: 每轮的平均效果
  ## 计算第一项研究中XXX对同性恋脚本的每轮效果的平均值
  count <- (2:7)
  output <- c()
  for (i in count) {
    meanNoContact <- NoContact1.mean[i, 1] - NoContact1.mean[1, 1] 
    meanEffect <-mean(x$ssm[x$wave == i]) - mean(x$ssm[x$wave == 1]) - meanNoContact
    output[i - 1] <- meanEffect
    
  }
  return(output)
  
}


# 每轮同性恋宣传同性恋脚本的平均效果
SSMbyGay1.mean <- getSSMeffect(SSMbyGay1)
SSMbyStra1.mean <- getSSMeffect(SSMbyStra1)


# 展示

SSMbyGay1.mean
SSMbyStra1.mean

##### Question 5####

# 检验是否实现干预的随机化

# 获取第二项研究第一次调查的数据
Study2.Wave1 <- subset(gay, gay$study == 2 & gay$wave == 1)

# 计算干预前的平均得分水平
Study2.Wave1.mean <- tapply(Study2.Wave1$ssm, Study2.Wave1$treatment, mean)
Study2.Wave1.mean

#### Question 6 ####

## 创建第二项研究第二次调查的数据子集
Study2.Wave2 <- subset(gay, gay$study == 2 & gay$wave == 2)

## 以干预方式分组分别计算第二轮的平均得分水平
Study2.Wave2.mean <- tapply(Study2.Wave2$ssm, Study2.Wave2$treatment, mean)

## 将第一次和第二次数据转换成数据框
Study2.Wave1.mean <- as.data.frame(Study2.Wave1.mean)

Study2.Wave2.mean <- as.data.frame(Study2.Wave2.mean)

Study2.Wave1.mean 
Study2.Wave2.mean

## 计算平均干预效应
DiD.gay2wave1 <- (Study2.Wave2.mean[2,1] - Study2.Wave1.mean[2,1]) - (Study2.Wave2.mean[1,1] - Study2.Wave1.mean[1,1])
DiD.gay2wave1 - DiD.gay1wave1

## 第二次研究第二轮的平均干预效应稍大于第一次研究第二轮的平均干预效应。


#### Question 6 ####

# 计算控制组的mean
NoContact2 <- subset(gay,subset =((gay$study == 2)& 
                                    (gay$treatment == "No Contact")) )
NoContact2.mean <- data.frame(mean = tapply(NoContact2$ssm, NoContact2$wave, mean))
NoContact2.mean

getSSMeffect2 <- function(x){
  ## 
  ## Args: data.frame
  ## return: 每轮的平均效果
  ## 计算第一项研究中XXX对同性恋脚本的每轮效果的平均值
  count <- (2:7)
  count <- count[-c(4,5)]
  output <- c()
  for (i in 1:4) {
    meanNoContact <- NoContact2.mean[i + 1, 1] - NoContact2.mean[1, 1] 
    meanEffect <-mean(x$ssm[x$wave == count[i]]) - mean(x$ssm[x$wave == 1]) - meanNoContact
    output[i] <- meanEffect
    
  }
  return(output)
  
}
## 获取数据框
SSMbyGay2 <- subset(gay, (gay$study == 2)& 
                      (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser"))
SSMbyGay2.mean <- getSSMeffect2(SSMbyGay2)



## 用数据框对比展示
Studydf <- data.frame("study1" = SSMbyGay1.mean[c(1,2,3,6)], "study2" = SSMbyGay2.mean )
rownames(Studydf) <- c(2, 3 ,4,7)  

Studydf










