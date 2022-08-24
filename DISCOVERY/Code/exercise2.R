##
## file :exercise2.R
## author: wxzher
## update: 2022-08-14
## the code for exercie2 in Chapter5
##

## 导入工具包
library("igraph")

## 导入数据
trade <- read.csv("../Datasets/trade.csv")
summary(trade)

#### question 1 ####
country <- unique(trade$country1)
n <- length(country)
n
year.num <- unique(trade$year)
## 填充空值
trade$exports[is.na(trade$exports)] <- 0

getAdj <- function(x){
  trade.mat <- matrix(0, nrow = n, ncol = n)
  colnames(trade.mat) <- rownames(trade.mat) <- country
  for(i in 1:nrow(x)){
    if(x$exports[i] != 0){
      trade.mat[x$country1[i], x$country2[i]] <-  1
    }
  }
  trade.adj <- graph.adjacency(trade.mat, mode = "directed",
                               diag = FALSE)
  return(trade.adj)
}
density <- rep(NA, 7)
for(i in 1:7){
  trade.year = subset(trade, subset = (trade$year == year.num[i]))
  trade.adj <- getAdj(trade.year)
  ### 计算网络密度
  density[i] <- graph.density(trade.adj)
  
  
}

plot(year, density, type = "b", col = "red")



#### question 2 #### 

## 1. 定义一个返回度值、接近值、中介值的函数，传入值为data.frame,并排好序
getCentralDegree <- function(df){
  year.adj <- getAdj(df)
  year.degree <- degree(year.adj)
  year.closeness <- closeness(year.adj)
  year.betweenness <- betweenness(year.adj)
  year.list <- list(sort(year.degree, decreasing = TRUE), sort(year.closeness, 
                  decreasing = TRUE),sort(year.betweenness, decreasing = TRUE))
  return( year.list )
  
}

## 2. 函数应用于1900、1955和2009年
## 获取数据
trade.1900 <- subset(trade,trade$year == 1900)
trade.1955 <- subset(trade,trade$year == 1955)
trade.2009 <- subset(trade,trade$year == 2009)
## 调用函数
centralDegree.1900 <- getCentralDegree(trade.1900)

centralDegree.1955 <- getCentralDegree(trade.1955)

centralDegree.2009 <- getCentralDegree(trade.2009)

## 3. 获取top5
## 度值top5
list("1900" = centralDegree.1900[[1]][1:5], "1955" = centralDegree.1955[[1]][1:5], "2009" = centralDegree.2009[[1]][1:5] )
## 接近值top5
list("1900" = centralDegree.1900[[2]][1:5], "1955" = centralDegree.1955[[2]][1:5], "2009" = centralDegree.2009[[2]][1:5] ) 
## 中介值top5
list("1900" = centralDegree.1900[[3]][1:5], "1955" = centralDegree.1955[[3]][1:5], "2009" = centralDegree.2009[[3]][1:5] ) 
## 4. 画出1900、1955和2009年的网络图
plot(getAdj(trade.1900))
plot(getAdj(trade.1955))
plot(getAdj(trade.2009))
#### question 3 #####
## 1.定义一个函数返回有权重的、有向的网络
getAdj2 <- function(x){
  trade.mat2 <- matrix(0, nrow = n, ncol = n)
  colnames(trade.mat2) <- rownames(trade.mat2) <- country
  for(i in 1:nrow(x)){
    
    trade.mat2[x$country1[i], x$country2[i]] <- x$exports[i]
    
  }
  trade.adj2 <- graph.adjacency(trade.mat2, mode = "directed",
                                diag = FALSE)
  return(trade.adj2)
}

## 2. 相应的定义一个获取中心度的函数
getCentralDegree2 <- function(df){
year.adj <- getAdj2(df) ## 只需要修改这个为getAdj2()
year.degree <- degree(year.adj)
year.closeness <- closeness(year.adj)
year.betweenness <- betweenness(year.adj)
year.list <- list(sort(year.degree, decreasing = TRUE), sort(year.closeness, 
                                                             decreasing = TRUE),sort(year.betweenness, decreasing = TRUE))
return( year.list )

}

## 3. 调用函数
centralDegree2.1900 <- getCentralDegree2(trade.1900)

centralDegree2.1955 <- getCentralDegree2(trade.1955)

centralDegree2.2009 <- getCentralDegree2(trade.2009)

## 4. 获取top5

## 度值top5
list("1900" = centralDegree2.1900[[1]][1:5], "1955" = centralDegree2.1955[[1]][1:5], "2009" = centralDegree2.2009[[1]][1:5] )
## 接近值top5
list("1900" = centralDegree2.1900[[2]][1:5], "1955" = centralDegree2.1955[[2]][1:5], "2009" = centralDegree2.2009[[2]][1:5] ) 
## 中介值top5
list("1900" = centralDegree2.1900[[3]][1:5], "1955" = centralDegree2.1955[[3]][1:5], "2009" = centralDegree2.2009[[3]][1:5] ) 

## 5. 计算加了权重的度值
strength.1900 <- graph.strength(getAdj2(trade.1900))
strength.1955 <- graph.strength(getAdj2(trade.1900))
strength.2009 <- graph.strength(getAdj2(trade.1900))

## 6. 画出1900、1955和2009年的网络图
plot(getAdj2(trade.1900), vertex.size = centralDegree2.1900[[3]] * 0.1, edge.width = strength.1900 * 0.001,edge.arrow.size = 0.1)
plot(getAdj2(trade.1955), vertex.size = centralDegree2.1900[[3]] * 0.4, edge.width = strength.1900 * 0.001 ,edge.arrow.size = 0.1)
plot(getAdj2(trade.2009), vertex.size = centralDegree2.1900[[3]] * 0.3, edge.width = strength.1900 * 0.001 ,edge.arrow.size = 0.1)

#### question 4 ####

## 1. 定义一个函数获取每一年的adj后计算page-rank，并把结果排序后输出
getPagerank <- function(i){
  trade.year = subset(trade, subset = (trade$year == i))
  trade.adj <- getAdj2(trade.year)
  pagerank.year <- page.rank(trade.adj)$vector
  return(sort(pagerank.year, decreasing = TRUE))
  
}


## 2.应用到每一年中并获取top5 
pagerank.1900 <- getPagerank(year.num[1])
pagerank.1920 <- getPagerank(year.num[2])
pagerank.1940 <- getPagerank(year.num[3])
pagerank.1955 <- getPagerank(year.num[4])
pagerank.1980 <- getPagerank(year.num[5])
pagerank.2000 <- getPagerank(year.num[6])
pagerank.2009 <- getPagerank(year.num[7])

pagerank.1900[1:5]
pagerank.1920[1:5]
pagerank.1940[1:5]
pagerank.1955[1:5]
pagerank.1980[1:5]
pagerank.2000[1:5]
pagerank.2009[1:5]
## 3. 定义一个函数获取对应国家逐年的page-rank
getPRByCountry <- function(country){
  out <- c(pagerank.1900[country],pagerank.1920[country], pagerank.1940[country],
           pagerank.1955[country],pagerank.1980[country],pagerank.2000[country],
           pagerank.2009[country])
  return(out)
}
## 4. 调用函数
US.pagerank <- getPRByCountry("United States of America")
UK.pagerank <- getPRByCountry("United Kingdom")
Russia.pagerank <- getPRByCountry("Russia")
China.pagerank <- getPRByCountry("China")
Japan.pagerank <- getPRByCountry("Japan")

## 5. 画图显示
par(mfrow = c(3,2),cex = 0.8)
plot(year.num, US.pagerank, type = "l", xlab = "year", xlim = c(1900, 2009),ylim = c(0, 0.15),main = "US")
plot(year.num, UK.pagerank, type = "l", xlab = "year", xlim = c(1900, 2009),ylim = c(0, 0.15),main = "UK")
plot(year.num, Russia.pagerank, type = "l", xlab = "year", xlim = c(1900, 2009),ylim = c(0, 0.15),main = "Russia")
plot(year.num, China.pagerank, type = "l", xlab = "year", xlim = c(1900, 2009),ylim = c(0, 0.15),main = "China")
plot(year.num, Japan.pagerank, type = "l", xlab = "year", xlim = c(1900, 2009),ylim = c(0, 0.15),main = "Japan")
