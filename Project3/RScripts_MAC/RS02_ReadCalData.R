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
library(plyr)

########################################################################
## Define folder paths
########################################################################
RScriptPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/RScripts_MAC/'
DataPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/Data/'
PlotPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/Plots/'
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
#table(Data_cal$Spectra)
table(Data_cal$Instrument.x)
table(Data_cal$Instrument.y)
table(Data_cal$SimpleInstrument.x)
table(Data_cal$SimpleInstrument.y)

Table_Species <- table(Data_cal$Species)
Table_Species <- Table_Species[order(Table_Species, decreasing = T)]
xtable(as.data.frame(Table_Species))

Row <- 10

Filename.plot <- paste0(PlotPath, 'PairwisePlots_withDiff.pdf')
pdf(file = Filename.plot, onefile = T)
for(Row in 1:nrow(Data_cal)){
  #  for(Row in 1:10){
  Data <- fn_longData_byRow(Data_cal, Row = Row )
  LongData <- Data[['LongData']]
  DiffData <- Data[['DiffData']]
  LongPlot <- plot_pairs(LongData = LongData)
  DiffPlot <- plot_diff(DiffData = DiffData)
  grid.arrange(LongPlot, DiffPlot, heights = c(3/4, 1/4))
}
dev.off()

Data_cal1 <- Data_cal[-c(209:212),]

## Below are plots without the noisy data, truncated from 500 onwards
Filename.plot <- paste0(PlotPath, 'PairwisePlots_withDiff_trunc.pdf')
pdf(file = Filename.plot, onefile = T)
for(Row in 1:nrow(Data_cal1)){
#   for(Row in 1:10){
  Data <- fn_longData_byRow(Data_cal1, Row = Row)
  LongData <- Data[['LongData']]
  DiffData <- Data[['DiffData']]
  LongData <- subset(LongData, Wavelength >= 500)
  DiffData <- subset(DiffData, Wavelength >= 500)
  LongPlot <- plot_pairs(LongData = LongData)
  DiffPlot <- plot_diff(DiffData = DiffData)
  grid.arrange(LongPlot, DiffPlot, heights = c(2/3, 1/3))
}
dev.off()


DiffData <- fn_longData_byRow(Data_cal, Row = Row )[['DiffData']]
Plot <- qplot() + geom_line(aes(x = Wavelength, y = Diff), data = DiffData, size = 1.5)
Plot <- Plot + ggtitle(label = LongData$Spectra[1])
Plot <- Plot + ylab(label = 'Difference')
Plot <- Plot + theme(legend.position = 'top')
Plot <- Plot + geom_hline(yintercept = 0, lty = 2)
Plot

Data_cal$species <- mapvalues(
  x = Data_cal$Species, 
  from = c("ABCO", "ARCTO3", "CAAN", "CAANsweet", "CADE27", "CECU", "Grape", 
           "LOPE", "MatureOat", "PEAM", "PHOEN2", "PICO3", "PILA", "PIPO", 
           "PISA2", "QUCH2", "QUDO", "QUKE", "QUWI2", "SAME3"), 
  to = c("Others", "ARCTO3", "CAAN", "Others", "CADE27", "Others", "Grape",
         "Others", "Others", "PEAM", "Others", "PICO3", "PILA", "Others", 
         "Others", "QUCH2", "Others", "QUKE", "QUWI2", "Others")
)
Table_species <- table(Data_cal$species)
Table_species <- Table_species[order(Table_species, decreasing = T)]
xtable(as.data.frame(Table_species))

DiffData_All <- c()
for(Row in 1:nrow(Data_cal)){
#    for(Row in 1:100){
  if(Data_cal[Row, 'species'] != 'Others'){
    Data <- fn_longData_byRow(Data_cal, Row = Row )
    DiffData <- Data[['DiffData']]
    DiffData <- subset(DiffData, Wavelength >= 500)
    DiffData_All <- rbind(DiffData_All, DiffData)
    rm(Data, DiffData)
  }
}
Plot <- qplot() + geom_line(aes(x = Wavelength, y = Diff, color = species), data = DiffData_All)
#Plot <- Plot + ggtitle(label = DiffData_All$Spectra[1])
Plot <- Plot + ylab(label = 'Difference')
Plot <- Plot + theme(legend.position = 'top')
Plot <- Plot + geom_hline(yintercept = 0, lty = 2)
# Plot <- Plot + facet_wrap(~ SpeciesCat)
Plot

Filename <- paste0(PlotPath, 'Diff_byspecies.pdf')
pdf(file = Filename)
print(Plot)
dev.off()
