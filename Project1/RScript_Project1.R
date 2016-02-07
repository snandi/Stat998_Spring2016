rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'

Filename <- paste0(RScriptPath, 'data_set1.csv')
Data <- read.csv(Filename, header = T)

str(Data)
## 59 observations 1083 covariates

########################################################################
## Some basic diagnostics
########################################################################
length(unique(Data$SampleID))
length(unique(Data$cow.id))
names(Data)[names(Data) == 'cow.id'] <- 'CowID'
names(Data)[names(Data) == 'body.weight..lb'] <- 'BodyWeight'
names(Data)[names(Data) == ' body.condition.score..range.1..low..to.5..high..'] <- 'Body_Condition_Score'

            
            
          
