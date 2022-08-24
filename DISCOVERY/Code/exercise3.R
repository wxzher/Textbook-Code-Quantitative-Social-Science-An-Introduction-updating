##
## file :exercise3.R
## author: wxzher
## update: 2022-08-15
## the code for exercie3 in Chapter5
##

## 导入数据
elections <- read.csv("../Datasets/elections.csv")
## 导入工具包
library("igraph")
library("maps")
library("animation")
library("ineq")

#### question1 可视化 2008 年美国总统大选郡一层级的结果 ####
## 1.计算郡内两党选票比例
elections$rep.county.prop <- elections$rep / (elections$rep + elections$dem)
elections$dem.county.prop <- elections$dem / (elections$rep + elections$dem)


## 2. 创建2008 年 Massachusetts Arizona 两州的数据子集
elections.Mass <- subset(elections, (elections$year == 2008)&(elections$state == "massachusetts"))
elections.Ari <- subset(elections, (year == 2008)&(state == "arizona"))
## 3. 创建不同州和郡的名字
elections$name <- paste(elections$state, elections$county, sep = ",")
## 4.创建一个函数可以绘制不同郡的情况
getMap <- function(x){
  ## 利用循环为地图中每一个郡填入相应的颜色 (红色的浓度表示对共和党的支持率，蓝色的浓度表示对民主党的支持率)
  for (i in (1:nrow(x))) {
    map(database = "county", regions = x$name[i],
        col = rgb(red = x$rep.county.prop[i],
                  blue = x$dem.county.prop[i],green = 0), fill = TRUE, add = TRUE) }
  
}
## 5. 代入massachusetts和arizona的数据
map(database = "county", regions = "massachusetts")
getMap(elections.Mass)
map(database = "county", regions = "arizona")
getMap(elections.Ari)

#### question 2 可视化全美国郡一层级的选举结果 ####

for (i in unique(elections$state)) {
  elections.state <- subset(elections,(year == 2008)&(state == i))
  map("county",i)
  getMap(elections.state)
}
## 不知为何这里的郡一级的名称识别不出来了

#### question 3 用动画可视化 1960 到 2012 年郡一级的选举结果####

## 1. 创建函数绘制年份的全美地图
getMapByYear <- function(x){
  ## 创建全美的年份选举子集
  elections.year <- subset(elections, elections$year == x)
  ## 绘制地图
  map("county")
  for (i in unique(elections$state)) {
    elections.state <- subset(elections.year,state == i)
    getMap(elections.state)
    
  }
  title(as.character(x))
  
}
## 2. 获取年份数据
years <- unique(elections$year)

## 3. 应用函数在每个年份并保存为html文件
saveHTML({
  for (var in years) {
    getMapByYear(var)
  }
}, title = "elections over years", htmlfile = "getMapByYear.html",
outdir = getwd(), autobrowse = FALSE)


#### question 4 相异指数量化党派分割指数####


## 1. 定义函数计算某个年份各州的相异指数

getDif <- function(x){
  ## 获取年份数据
  elections.year <- subset(elections, elections$year == x)
  ## 获取各州的相异指数
  ## 州的名称
  statenames <- unique(elections.year$state)
  ## 创建变量接受
  output <- rep(NA,length(statenames))
  names(output) <- statenames
  ## 计算
  for (i in 1:length(statenames)) {
    elections.state <- subset(elections.year,state == statenames[i])
    if( sum(elections.state$dem) == 0){
      result <- 0
    } else{
      D <- elections.state$dem / sum(elections.state$dem)
      R <- elections.state$rep / sum(elections.state$rep)
      result <- sum(abs(D - R)) * 0.5
    }
    
    output[i] <- result
  }
 return(output)
  
}

## 2. 代入2008年得出结果
Dif.2008 <- getDif(2008)
Dif.2008

## 3. 最不易分割的州和最易分割的州
## 最不易分隔的州——指数最小
sort(Dif.2008)[1]
## 最易分隔的州——指数最大
sort(Dif.2008, decreasing = TRUE)[1]

## 4. 可视化全美地图
map(database = "state")

for (i in 1:length(Dif.2008)) {
  map("state", names(Dif.2008[i]), 
      col = rgb(red = Dif.2008 * 3, blue = Dif.2008 * 3, green = Dif.2008 * 3),fill = TRUE,
      add = TRUE)
  
}

#### question 5 计算每个州的基尼系数观察政治分割程度####

## ineq包在最开始已经导入

## 1. 定义一个函数计算某个年份各州的相异指数

getGini <- function(x){
  ## 获取年份数据
  elections.year <- subset(elections, elections$year == x)
  ## 州的名称
  statenames <- unique(elections.year$state)
  ## 创建变量接受
  output2 <- rep(NA,length(statenames))
  names(output2) <- statenames
  ## 计算基尼系数
  for (i in 1:length(statenames)) {
    elections.state <- subset(elections.year,state == statenames[i])
    output2[i] <- ineq(elections.state$dem.county.prop, type = "Gini")
    
  }
  return(output2)
}

## 2. 获取2008年的基尼系数
Gini.2008 <- getGini(2008)

Gini.2008

## 3. 可视化全美地图
map(database = "state")

for (i in 1:length(Gini.2008)) {
  map("state", names(Gini.2008[i]), 
      col = rgb(red = Gini.2008 * 3 , blue = Gini.2008 * 3, green = Gini.2008 * 3),fill = TRUE,
      add = TRUE)
  
}


#### question 6 动画可视化呈现各州政治分割程度 ####

## 1. animation包已经在开始时导入

## 2. 创建函数可视化

getDifMapByYear <- function(x){
  Dif.year <- getDif(x)
  map(database = "state")
  
  for (i in 1:length(Dif.year)) {
    map("state", names(Dif.year[i]), 
        col = rgb(red = Dif.year , blue = Dif.year, green = Dif.year),fill = TRUE,
        add = TRUE)
  }
  title(as.character(x))
} 

## 制作动画
saveHTML({
  for (var in years) {
    getDifMapByYear(var)
    
  }
  
}, title = "political segregation", outdir = getwd(), 
htmlfile = "getDifMapByYear.html",autobrowse = FALSE)





