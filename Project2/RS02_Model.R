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

Filename <- paste0(RScriptPath, 'Data_Combined.RData')
load(Filename)

dim(Data)
str(Data)

########################################################################
## Correlation between different response variables
########################################################################
Responses <- c('Yield_tonperac', 'Ht', 'StemCount', 'CP', 'NDF', 'IVTD', 'NDFD')
Corr.Test <- corr.test(Data[,Responses])
round(Corr.Test$r, 4)
round(Corr.Test$p, 4)

########################################################################
## Model 1: Yield ~ Location + Year + Sorghum_Type + Veg_Type
########################################################################
Model1 <- lm(Yield_tonperac ~ Location + Year + Sorghum_Type + Veg_Type, data = Data)
summary(Model1)
anova(Model1)

########################################################################
## Model 2: Yield ~ Location + Year + Sorghum_Type + Veg_Type
########################################################################
Model2 <- lm(Yield_tonperac ~ Location + Year + Veg_Type + Sorghum_SubType +
               Location*Veg_Type, data = Data)
summary(Model2)
anova(Model2)


