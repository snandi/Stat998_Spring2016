rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 3, Stat 998, Spring 2016
## This script reads in california data set and makes exploratory plots
########################################################################
library(fda)
library(gridExtra)
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)
library(RFunctionsSN)
library(psych)

########################################################################
## Define folder paths
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project3/RScripts_Project3/'
DataPath <- '~/Courses/Stat998_Spring2016/Project3/Data/'
PlotPath <- '~/Courses/Stat998_Spring2016/Project3/Plots/'
source(paste0(RScriptPath, 'fn_Library_Project3.R'))

########################################################################
## Load Coefficients
########################################################################
Wavelength <- c(1, 350:2500)
Scores_All <- cbind(Wavelength, Drop = rep(x = 1, length.out = length(Wavelength)))
Components = c('ADF', 'Carbon', 'LMA', 'ADL', 'Cellulose', 'Nitrogen')
Comp <- Components[1]
for(Comp in Components){
  Scores.Comp <- fn_loadScoresByComp(Comp = Comp, DataPath = DataPath)
  Scores_All <- merge(x = Scores_All, y = Scores.Comp, by = 'Wavelength', all = T)  
  Scores_All[,Comp] <- na.is.zero(Scores_All[,Comp])
}



########################################################################
## Load calibration data
########################################################################
Filename <- 'Common_SE_ASD_no_meta.csv'
Data_cal <- read.csv(file = paste0(DataPath, Filename), header = T)

colnames_SE <- colnames(Data_cal)[grep(pattern = "Wave_.*x", x = colnames(Data_cal))]
colnames_ASD <- colnames(Data_cal)[grep(pattern = "Wave_.*y", x = colnames(Data_cal))]
colnames_Other <- colnames(Data_cal) %w/o% c(colnames_SE, colnames_ASD)

Data_cal_ASD <- Data_cal[,colnames_ASD]
File.Comp <- fn_getFilename(Comp = 'Nitrogen')
Scores.Nitrogen <- read.table(file = paste0(DataPath, File.Comp), header = T, sep = ',')

File.Carbon <- fn_getFilename(Comp = 'Carbon')
Scores.Carbon <- read.table(file = paste0(DataPath, File.Carbon), header = T, sep = ',')

File.LMA <- fn_getFilename(Comp = 'LMA')
Scores.LMA <- read.table(file = paste0(DataPath, File.LMA), header = T, sep = ',')


Row <- 10
# for(Row in 1:nrow(Data_cal)){
#   #  for(Row in 1:10){
Data <- fn_longData_byRow(Data_cal, Row = Row )
LongData <- Data[['LongData']]
File.Nitrogen <- fn_getFilename(Comp = 'Nitrogen')
Scores.Nitrogen <- read.table(file = paste0(DataPath, File.Nitrogen), header = T, sep = ',')
Scores.Nitrogen$Wavelength <- gsub(pattern = "Wave_", replacement = "", x = Scores.Nitrogen$X)

LongData <- merge(x = LongData, y = Scores.Nitrogen, by = 'Wavelength', all = T)

# }
# dev.off()
