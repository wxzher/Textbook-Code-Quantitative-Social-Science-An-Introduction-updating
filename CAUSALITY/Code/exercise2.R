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

# 获取第一个研究数据中同性恋宣传对同性恋话题第一次和第二次结果的效果

SSMbyGay1.wave1 <- SSMbyGay1$ssm[SSMbyGay1$wave == 1]

# 计算平均值
mean(SSMbyGay1.wave1)

SSMbyGay1.wave2 <- SSMbyGay1$ssm[SSMbyGay1$wave == 2]

# 计算平均值
mean(SSMbyGay1.wave2)

# 差值
mean(SSMbyGay1.wave1) - mean(SSMbyGay1.wave2)

# 获取第一个研究数据中异性恋宣传对同性恋话题第一次和第二次结果的效果
SSMbyStra1.wave1 <- SSMbyStra1$ssm[SSMbyStra1$wave == 1]

# 计算平均值
mean(SSMbyStra1.wave1)

SSMbyStra1.wave2 <- SSMbyStra1$ssm[SSMbyStra1$wave == 2]

# 计算平均值
mean(SSMbyStra1.wave2)

# 差值
mean(SSMbyStra1.wave1) - mean(SSMbyStra1.wave2)

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

# 比较
# 回收废品话题中的同性恋宣传者和异性恋宣传者
mean(RecyclebyGay1.wave2) - mean(RecyclebyStra1.wave2)

# 同性恋话题中的同性恋宣传者和异性恋宣传者
mean(SSMbyGay1.wave2) - mean(SSMbyStra1.wave2)

# 同性恋宣传者针对同性恋话题和回收废品话题
mean(SSMbyGay1.wave2) - mean(RecyclebyGay1.wave2)

# 异性恋宣传者针对同性恋话题和回收废品话题
mean(SSMbyStra1.wave2) - mean(RecyclebyStra1.wave2)


#### Question 4 ####

# 定义一个函数

getSSMeffect <- function(x){
  ## 
  ## Args: data.frame
  ## return: 七轮的平均值
  ## 计算第一项研究中XXX对同性恋脚本的每轮效果的平均值
  count <- (1:7)
  output <- c()
  for (i in count) {
    meanEffect <-mean(x$ssm[x$wave == i ])
    output[i] <- meanEffect
    
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
Study2DIDgay1 <- (Study2Wave2b[2,1] - Study2Wave1b[2,1]) - (Study2Wave2b[1,1] - Study2Wave1b[1,1])
Study2DIDgay1 - DIDgay1

## 第二次研究第二轮的平均干预效应稍大于第一次研究第二轮的平均干预效应。


#### Question 6 ####


# 计算异性恋宣讲者的同性恋婚姻脚本的每轮ssm平均分
Study2WaveMean <- function(x){
  Study2wavex <- subset(gay, gay$study == 2 & gay$wave == x)
  Study2wavexa <- tapply(Study2wavex$ssm, Study2wavex$treatment, mean)
  Study2wavexb <- as.data.frame(Study2wavexa)
  Study2wavexb
}
Study2wave3b <- Study2WaveMean(3)
Study2wave4b <- Study2WaveMean(4)
Study2wave7b <- Study2WaveMean(7)
## 计算同性恋宣传者的同性恋婚姻脚本的每轮平均效应
Study2DIDgay <- function(x){
  Study2DIDgay <- (Study2WaveMean(x)[2,] - Study2WaveMean(x-1)[2,]) - (Study2WaveMean(x)[1,] - Study2WaveMean(x-1)[1,])
  Study2DIDgay
}
Study2DIDgay1 <- Study2DIDgay(2)
Study2DIDgay2 <- Study2DIDgay(3)
Study2DIDgay3 <- Study2DIDgay(4)

Study2DIDgay1

Study2DIDgay2

Study2DIDgay3

Study2DIDgay6 <- (Study2WaveMean(7)[2,] - Study2WaveMean(4)[2,]) - (Study2WaveMean(7)[1,] - Study2WaveMean(4)[1,])
## 用数据框对比展示
Study2df <- data.frame("second" = c(DIDgay1, Study2DIDgay1), "third" = c(DIDgay2, Study2DIDgay2), "fourth" = c(DIDgay3, Study2DIDgay3), "seventh" = c(DIDgay6, Study2DIDgay6))
row.names(Study2df) <- c("Study1", "Study2")
Study2df












