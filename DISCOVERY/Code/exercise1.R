##
## file :exercise1.R
## author: wxzher
## update: 2022-08-14
## the code for exercie1 in Chapter5
##


## 分析宪法的序言


###### question 1 ######

## load two required libraries
library(tm, SnowballC)

## 导入数据
doc <- read.csv("../Datasets/constitution.csv")
dim(doc)

 
docCorpus.raw <- Corpus( VectorSource(doc$preamble) )
control.list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE,
                     stripWhitespace = TRUE, stemDocument = TRUE,removeNumbers = TRUE)
doc.dtm <- DocumentTermMatrix(docCorpus.raw, control = control.list)

inspect(doc.dtm[1:5, 1:8])
## 转化成标准矩阵
doc.dtm.mat <- as.matrix(doc.dtm)
## 计算逆文本频率
doc.tfidf <- weightTfIdf(doc.dtm)
## 转化为标准矩阵
doc.tfidf.mat <- as.matrix(doc.tfidf)
## 查看美国的位置
US <- subset(doc, doc$country == "united_states_of_america")
US
## 词云图
library(wordcloud)
wordcloud(colnames(doc.dtm.mat), doc.dtm.mat[149, ], max.word = 20) 
wordcloud(colnames(doc.tfidf.mat), doc.tfidf.mat[149, ], max.word = 20) 


##### question 2  ######
sq <- rep(NA, 155)
for(i in 1:155){
  a <- length(doc.tfidf.mat[i,])
  sq[i] <- a ^2
}
a <- sqrt(sum(sq))
a

km <- kmeans(doc.tfidf / a, centers = 5)
km$iter

for ( i in 1:5){
  cat("CLUSTER", i, "\n")
  cat("Top 10 words: \n")
  print(head(sort(km$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  
}

###### question 3 #####

cosine <- function(a,b){
  numer <- apply(a * t(b), 2, sum)
  denom <- sqrt(sum(a ^2) * sqrt(apply(b^2, 1, sum)))
  return(numer / denom)
}
doc.cosine <- cosine(doc.dtm.mat[155, ], doc.dtm.mat)
doc$cosine <- doc.cosine
top5 <-  sort(doc.cosine, decreasing = TRUE)[1:5]

names(top5) <- doc$country[as.numeric(names(top5))]
top5
##### question 4 #####
year <- seq(from = 1960, to = 2010, by = 10)
consine.mean <- rep(NA,length(year)-1)
for(i in 1:length(year)-1){
  consine.mean[i] <- mean(doc$cosine[doc$year >= year[i] & doc$year < year[i+1]])
}
consine.mean
plot(year[-1], consine.mean,type = "l", col = "blue",
     xlim = c(1970, 2010), ylim = c(0.5, 0.8),
     xlab = "year", ylab = "consine")

##### question 5 #####

n <- nrow(doc)
doc.mat <- matrix(0, nrow = n, ncol = n)
colnames(doc.mat) <- rownames(doc.mat) <- doc$country
for(i in 1:nrow(doc)){
  cosine.i <- cosine(doc.dtm.mat[i, ], doc.dtm.mat)
  cosine.i[doc$year > doc$year[i]] <- 0
  doc.mat[i,] <- cosine.i
  
}
library("igraph")
doc.adj <- graph.adjacency(doc.mat, mode = "directed",
                               diag = FALSE)
sort(page.rank(doc.adj, directed = TRUE)$vector, decreasing = TRUE)[1:5]
plot(doc.adj,vertex.size = 10)
