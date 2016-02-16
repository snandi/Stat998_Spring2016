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

Cols_Cow <- c("Wt",
              "BC_Score",
              "Parity",
              "Days",
              "Intake",
              "Milk_Yield",
              "Fat",
              "Protein",
              "Lactose" ,
              "Nitrogen"
)

Cols_Acid <- c("C14_0", "C15_0", "C16_0",
               "C17_0", "C18_0", "C18_1"
)
table(Data1$Parity)

Filename <- paste0(RPlotPath, 'Pairwiseplots.pdf')
pdf(file = Filename, onefile = T, pointsize = 6)
ggpairs(Data1[,Cols_Cow])
ggpairs(Data1[,Cols_Acid])

for(Col in Cols_Acid){
  print(ggpairs(Data1[,c(Cols_Cow, Col)]))
}
dev.off()



