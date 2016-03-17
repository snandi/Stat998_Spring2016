rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 2, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)
library(psych)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project2/'

Filename <- paste0(RScriptPath, 'Data_Combined.txt')
Data <- read.table(file = Filename, header = T, sep = '\t')

dim(Data)
str(Data)

str(Data)

qplot() + geom_point(aes(x = Ht, y = Yield_tonperac), data = Data)

qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type), data = Data) +
  facet_wrap(~ Veg_Type)

qplot() + geom_point(aes(y = Yield_tonperac, x = Sorghum_Type, col = as.factor(Year)), data = Data) +
  facet_wrap(~ Veg_Type)
