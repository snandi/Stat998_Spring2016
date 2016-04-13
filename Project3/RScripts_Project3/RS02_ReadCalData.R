rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 3, Stat 998, Spring 2016
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
## Load calibration data
########################################################################
Filename <- 'Common_SE_ASD_no_meta.csv'
Data_cal <- read.csv(file = paste0(DataPath, Filename), header = T)

colnames_SE <- colnames(Data_cal)[grep(pattern = "Wave_.*x", x = colnames(Data_cal))]
colnames_ASD <- colnames(Data_cal)[grep(pattern = "Wave_.*y", x = colnames(Data_cal))]
colnames_Other <- colnames(Data_cal) %w/o% c(colnames_SE, colnames_ASD)

table(Data_cal$Species)
table(Data_cal$SpeciesCat)
table(Data_cal$Spectra)
table(Data_cal$Instrument.x)
table(Data_cal$Instrument.y)
table(Data_cal$SimpleInstrument.x)
table(Data_cal$SimpleInstrument.y)

# Row <- 1
# Wavelength <- 350:2500
# Intensity.ASD <- as.numeric(as.vector(Data_cal[Row, colnames_ASD]))
# #attributes(Intensity.ASD)$dimnames[[2]] <- 'Intensity'
# 
# LongData.ASD <- as.data.frame(cbind(Wavelength = Wavelength, 
#                                     Intensity = as.numeric(Intensity.ASD), 
#                                     SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.y']),
#                                     Instr = as.vector(Data_cal[Row, 'Instrument.y']),
#                                     Date = Data_cal[Row, 'Date.y'],
#                                     Name = as.vector(Data_cal[Row, 'Name_long.y']),
#                                     Species = as.vector(Data_cal[Row, 'Species']),
#                                     SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
#                                     Spectra = as.vector(Data_cal[Row, 'Spectra'])
# ), stringsAsFactors = F)
# str(LongData.ASD)
# 
# Intensity.SE <- as.numeric(as.vector(Data_cal[Row, colnames_SE]))
# LongData.SE <- as.data.frame(cbind(Wavelength = Wavelength, 
#                                     Intensity = as.numeric(Intensity.SE), 
#                                     SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.x']),
#                                     Instr = as.vector(Data_cal[Row, 'Instrument.x']),
#                                     Date = Data_cal[Row, 'Date.x'],
#                                     Name = as.vector(Data_cal[Row, 'Name_long.x']),
#                                     Species = as.vector(Data_cal[Row, 'Species']),
#                                     SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
#                                     Spectra = as.vector(Data_cal[Row, 'Spectra'])
# ), stringsAsFactors = F)
# 
# LongData <- rbind(LongData.ASD, LongData.SE)
# LongData <- within(data = LongData,{
#     Intensity <- as.numeric(Intensity)
#     Wavelength <- as.numeric(Wavelength)
#   })
# str(LongData)
# 
# qplot() + geom_line(aes(x = Wavelength, y = Intensity, col = factor(SimpleInstr)), data = LongData, size = 2) +
#   ggtitle(label = LongData$Spectra[1]) + 
#   ylab(label = '') + 
#   theme(legend.position = 'top') +
#   guides(color = guide_legend(title = ''))

Filename.plot <- paste0(PlotPath, 'PairwisePlots_cal.pdf')
pdf(file = Filename.plot, onefile = T)
for(Row in 1:nrow(Data_cal)){
#  for(Row in 1:10){
    LongData <- fn_longData_byRow(Data_cal, Row = Row )
  plot(plot_pairs(LongData = LongData))
}
dev.off()

