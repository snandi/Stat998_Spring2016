rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 3, Stat 998, Spring 2016
########################################################################
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

########################################################################
## Load calibration data
########################################################################
File_asd <- 'asd_spectra_ben_crosscal_df.csv'
Data_asd <- read.csv(file = paste0(DataPath, File_asd), header = T)

File_sed <- 'sed_spectra_ben_crosscal_df.csv'
Data_sed <- read.csv(file = paste0(DataPath, File_sed), header = T)

str(Data_asd$idstr)
WaveLengths <- colnames(Data_asd)[-1]
WL_Num <- as.numeric(gsub(pattern = 'WVL_', replacement = '', x = WaveLengths))

matplot(y = t(Data_asd[,2:2152]), type = 'l')

matplot(y = t(Data_sed[,2:2152]), type = 'l')

plot(x = 1:2151, y = Data_sed[6,2:2152],  type = 'l')
lines(x = 1:2151, y = Data_asd[6,2:2152], col = 'red')  

################ Leaf 10 ################
Leaf10_Diff <- as.matrix(Data_sed[1,2:2152] - Data_asd[1,2:2152])
Leaf10_toPlot <- as.data.frame(cbind(Wavelengths = WL_Num, 
                                     Leaf10_Diff = t(Leaf10_Diff), 
                                     SED = t(Data_sed[1,2:2152]), 
                                     ASD = t(Data_asd[1,2:2152])
))
colnames(Leaf10_toPlot) <- c('Wavelengths', 'Leaf10_Diff', 'Leaf10_sed', 'Leaf10_asd')

Plot10_1 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf10_asd), data = Leaf10_toPlot, col = 'red') +
  geom_line(aes(x = Wavelengths, y = Leaf10_sed), data = Leaf10_toPlot, col = 'blue') +
  ggtitle(label = 'Spectroscopic measurements of Leaf 10') +
  ylab(label = 'Intensity')

Plot10_2 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf10_Diff), data = Leaf10_toPlot) +
  ylab(label = 'Difference')

grid.arrange(Plot10_1, Plot10_2, heights = c(2/3, 1/3))

################ Leaf 15 ################
Leaf15_Diff <- as.matrix(Data_sed[6,2:2152] - Data_asd[6,2:2152])
Leaf15_toPlot <- as.data.frame(cbind(Wavelengths = WL_Num, 
                                     Leaf15_Diff = t(Leaf15_Diff), 
                                     SED = t(Data_sed[6,2:2152]), 
                                     ASD = t(Data_asd[6,2:2152])
))
colnames(Leaf15_toPlot) <- c('Wavelengths', 'Leaf15_Diff', 'Leaf15_sed', 'Leaf15_asd')

Plot15_1 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf15_asd), data = Leaf15_toPlot, col = 'red') +
  geom_line(aes(x = Wavelengths, y = Leaf15_sed), data = Leaf15_toPlot, col = 'blue') +
  ggtitle(label = 'Spectroscopic measurements of Leaf 15') +
  ylab(label = 'Intensity')

Plot15_2 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf15_Diff), data = Leaf15_toPlot) +
  ylab(label = 'Difference')

grid.arrange(Plot15_1, Plot15_2, heights = c(2/3, 1/3))

################ Leaf 20 ################
Leaf20_Diff <- as.matrix(Data_sed[12,2:2152] - Data_asd[12,2:2152])
Leaf20_toPlot <- as.data.frame(cbind(Wavelengths = WL_Num, 
                                     Leaf20_Diff = t(Leaf20_Diff), 
                                     SED = t(Data_sed[12,2:2152]), 
                                     ASD = t(Data_asd[12,2:2152])
))
colnames(Leaf20_toPlot) <- c('Wavelengths', 'Leaf20_Diff', 'Leaf20_sed', 'Leaf20_asd')

Plot20_1 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf20_asd), data = Leaf20_toPlot, col = 'red') +
  geom_line(aes(x = Wavelengths, y = Leaf20_sed), data = Leaf20_toPlot, col = 'blue') +
  ggtitle(label = 'Spectroscopic measurements of Leaf 20') +
  ylab(label = 'Intensity')

Plot20_2 <- qplot() + geom_line(aes(x = Wavelengths, y = Leaf20_Diff), data = Leaf20_toPlot) +
  ylab(label = 'Difference')

grid.arrange(Plot20_1, Plot20_2, heights = c(2/3, 1/3))

Filename <- paste0(PlotPath, 'ExploratoryPlots.pdf')
pdf(file = Filename, onefile = T)
grid.arrange(Plot10_1, Plot10_2, heights = c(2/3, 1/3))
grid.arrange(Plot15_1, Plot15_2, heights = c(2/3, 1/3))
grid.arrange(Plot20_1, Plot20_2, heights = c(2/3, 1/3))
dev.off()
