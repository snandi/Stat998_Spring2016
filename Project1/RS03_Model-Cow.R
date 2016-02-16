rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(GGally)
library(ggplot2)
library(reshape2)
library(psych)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'
RPlotPath <- '~/Courses/Stat998_Spring2016/Project1/Plots/'

Filename <- paste0(RScriptPath, 'Data1.RData')
load(Filename)
colnames(Data1)

########################################################################
## Fit C15 with cow characteristics
########################################################################
Cols_Cow <- c("Wt",
              "BC_Score",
              "Parity",
              "Days",
              "Intake",
              "Milk_Yield"
)
Cols_Milk <- c(
  "Fat",
  "Protein",
  "Lactose" ,
  "Nitrogen"
)

M15_1 <- lm(C15_0 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield, data = Data1)
summary(M15_1)

M15_2 <- lm(C15_0 ~ Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_2)

M15_3 <- lm(C15_0 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield + 
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_3)

step(object = M15_3, direction = 'backward', k = log(nrow(Data1)))

########################################################################
## Fit C18 with cow characteristics
########################################################################
M18_1 <- lm(C18_1 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield, data = Data1)
summary(M18_1)

M18_2 <- lm(C18_1 ~ Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_2)

M18_3 <- lm(C18_1 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield + 
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M18_3)

step(object = M18_3, direction = 'backward', k = log(nrow(Data1)))

