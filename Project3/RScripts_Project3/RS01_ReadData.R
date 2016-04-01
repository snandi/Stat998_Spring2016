rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 3, Stat 998, Spring 2016
########################################################################
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

########################################################################
## Load calibration data
########################################################################
File_asd <- 'asd_spectra_ben_crosscal_df.csv'
Data_asd <- read.csv(file = paste0(DataPath, File_asd), header = T)

File_sed <- 'sed_spectra_ben_crosscal_df.csv'
Data_sed <- read.csv(file = paste0(DataPath, File_sed), header = T)

str(Data_asd$idstr)
WaveLengths <- colnames(Data_asd)[-1]

matplot(y = t(Data_asd[,2:2152]), type = 'l')

matplot(y = t(Data_sed[,2:2152]), type = 'l')

plot(x = 1:2151, y = Data_sed[6,2:2152],  type = 'l')
lines(x = 1:2151, y = Data_asd[6,2:2152], col = 'red')  
