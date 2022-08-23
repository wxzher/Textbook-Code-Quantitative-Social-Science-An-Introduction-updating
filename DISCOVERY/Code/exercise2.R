##
## file :exercise2.R
## author: wxzher
## update: 2022-08-14
## the code for exercie2 in Chapter5
##

## 导入工具包
library("igraph")

## 导入数据
trade <- read.csv("Datasets/trade.csv")
summary(trade)

#### question 1 ####
country <- c(unique(trade$country1),unique(trade$country2))
country <- unique(country)
n <- length(country)
n
trade.mat <- matrix(0, nrow = n, ncol = n)
colnames(trade.mat) <- rownames(trade.mat) <- country
for(i in 1:nrow(trade)){
  trade.mat[trade$country1[i], trade$country2[i]] <-  1
  if(is.na(trade$exports[i])){
    trade.mat[trade$country1[i], trade$country2[i]] <-  0
  }
  
}
trade.adj <- graph.adjacency(trade.mat, mode = "directed",
                               diag = FALSE)
plot(trade.adj)
### 计算网络密度
graph.density(trade.adj)

#### question 2#####
trade.1900 <- subset(trade,trade$year == 1900)
trade.1900 <- subset(trade,trade$year == 1955)
trade.1900 <- subset(trade,trade$year == 2009)

trade.mat.1900 <- matrix(0, nrow = n, ncol = n)
colnames(trade.mat.1900 ) <- rownames(trade.mat) <- country
for(i in 1:nrow(trade.1900)){
  trade.mat.1900[trade.1900$country1[i], trade.1900$country2[i]] <-  1
  if(is.na(trade.1900$exports[i])){
    trade.mat.1900[trade.1900$country1[i], trade.1900$country2[i]] <-  0
  }
  
}
trade.1900.adj <- graph.adjacency(trade.mat.1900, mode = "directed",
                             diag = FALSE)

degree.1900 <- degree(trade.1900.adj)
data.1900 <- data.frame(country = country, degree.1900 = degree.1900)
order <- order(data.1900$degree.1900, decreasing = TRUE)
data.1900[order[1:5],]

