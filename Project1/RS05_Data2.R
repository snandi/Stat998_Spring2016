rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(psych)
library(reshape2)
library(robustX)


source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'

Filename <- paste0(RScriptPath, 'data_set2_sn.csv')
Data2 <- read.csv(Filename, header = T)

dim(Data2)

head(Data2)
Median <- L1median(Data2[,c('Days', 'Yield', 'Fat', 'Protein', 'Lactose')])$estimate

Data2$Dist <- 0
for(Row in 1:nrow(Data2)){
  X <- Data2[Row, c('Days', 'Yield', 'Fat', 'Protein', 'Lactose')]
  Y <- Median
  Data2[Row, 'Dist'] <- dist(rbind(X, Y), method = 'euclidean')
}

Data2 <- Data2[order(Data2$Dist, decreasing = T), ]

xtable(head(Data2))

xtable(tail(Data2))
