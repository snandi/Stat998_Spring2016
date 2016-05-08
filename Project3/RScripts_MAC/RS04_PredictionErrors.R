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
RScriptPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/RScripts_MAC/'
DataPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/Data/'
PlotPath <- '~/Documents/snandi/Stat998_Spring2016/Project3/Plots/'
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
Scores_All <- Scores_All[,-2]
########################################################################
## Load calibration data
########################################################################
Filename <- 'Common_SE_ASD_no_meta.csv'
Data_cal <- read.csv(file = paste0(DataPath, Filename), header = T)


Data_cal$species <- mapvalues(
  x = Data_cal$Species, 
  from = c("ABCO", "ARCTO3", "CAAN", "CAANsweet", "CADE27", "CECU", "Grape", 
           "LOPE", "MatureOat", "PEAM", "PHOEN2", "PICO3", "PILA", "PIPO", 
           "PISA2", "QUCH2", "QUDO", "QUKE", "QUWI2", "SAME3"), 
  to = c("Others", "ARCTO3", "CAAN", "Others", "CADE27", "Others", "Grape",
         "Others", "Others", "PEAM", "Others", "PICO3", "PILA", "Others", 
         "Others", "QUCH2", "Others", "QUKE", "QUWI2", "Others")
)

colnames_SE <- colnames(Data_cal)[grep(pattern = "Wave_.*x", x = colnames(Data_cal))]
colnames_ASD <- colnames(Data_cal)[grep(pattern = "Wave_.*y", x = colnames(Data_cal))]
colnames_Other <- colnames(Data_cal) %w/o% c(colnames_SE, colnames_ASD)

Data_cal <- Data_cal[-c(209:212),]
Table_Spectra <- table(Data_cal$Spectra)
Table_Name.x <- table(Data_cal$Name_long.x)
Table_Name.y <- table(Data_cal$Name_long.y)

table(Data_cal$Species)
table(Data_cal$SpeciesCat)

Comp <- 'Nitrogen'
Instr.y <- 'FS3'
Rows <- 1:nrow(Data_cal)

########################################################################
## Prediction table for Nitrogen, FS3 vs SE
########################################################################
PredictionTable_Nitrogen_FS3 <- fn_returnPredictionTable(
  Data_cal = Data_cal, 
  colnames_Other = colnames_Other, 
  Comp = 'Nitrogen',
  Instr.y = 'FS3'
)

MSPE_Orig_Nitrogen_FS3 <- sum( (PredictionTable_Nitrogen_FS3$Error_Orig)^2 ) / 
  nrow(PredictionTable_Nitrogen_FS3)
MAPctE_Orig_Nitrogen_FS3 <- mean(PredictionTable_Nitrogen_FS3$PctError_Orig)

Plot_Nitrogen_FS3 <- qplot() + 
  geom_histogram(aes(x = Error_Orig), data = PredictionTable_Nitrogen_FS3)
Plot_Nitrogen_FS3 <- Plot_Nitrogen_FS3 + xlab(label = 'Prediction error with SE data')
Plot_Nitrogen_FS3 <- Plot_Nitrogen_FS3 + ylab(label = 'Frequency')
Plot_Nitrogen_FS3 <- Plot_Nitrogen_FS3 + ggtitle(label = 'Nitrogen')

qplot() + geom_histogram(aes(x = Error_Orig), data = PredictionTable_Nitrogen_FS3)
qplot() + geom_histogram(aes(x = PctError_Orig), data = PredictionTable_Nitrogen_FS3)

PredictionTable_Nitrogen_FS4 <- fn_returnPredictionTable(
  Data_cal = Data_cal, 
  colnames_Other = colnames_Other, 
  Comp = 'Nitrogen',
  Instr.y = 'FS4'
)
MSPE_Orig_Nitrogen_FS4 <- sum( (PredictionTable_Nitrogen_FS4$Error_Orig)^2 ) / 
  nrow(PredictionTable_Nitrogen_FS4)
MAPctE_Orig_Nitrogen_FS4 <- mean(PredictionTable_Nitrogen_FS4$PctError_Orig)

qplot() + geom_histogram(aes(x = Error_Orig), data = PredictionTable_Nitrogen_FS4)
qplot() + geom_histogram(aes(x = PctError_Orig), data = PredictionTable_Nitrogen_FS4)

########################################################################
## Prediction table for Carbon, FS3 vs SE
########################################################################
PredictionTable_Carbon_FS3 <- fn_returnPredictionTable(
  Data_cal = Data_cal, 
  colnames_Other = colnames_Other, 
  Comp = 'Carbon',
  Instr.y = 'FS3'
)
MSPE_Orig_Carbon_FS3 <- sum( (PredictionTable_Carbon_FS3$Error_Orig)^2 ) / 
  nrow(PredictionTable_Carbon_FS3)
MAPctE_Orig_Carbon_FS3 <- mean(PredictionTable_Carbon_FS3$PctError_Orig)

Plot_Carbon_FS3 <- qplot() + 
  geom_histogram(aes(x = Error_Orig), data = PredictionTable_Carbon_FS3)
Plot_Carbon_FS3 <- Plot_Carbon_FS3 + xlab(label = 'Prediction error with SE data')
Plot_Carbon_FS3 <- Plot_Carbon_FS3 + ylab(label = 'Frequency')
Plot_Carbon_FS3 <- Plot_Carbon_FS3 + ggtitle(label = 'Carbon')

qplot() + geom_histogram(aes(x = PctError_Orig), data = PredictionTable_Carbon_FS3)


Box_Carbon <- qplot() + geom_boxplot(aes(x = species, y = PctError_Orig, fill = SpeciesCat), 
                       data = subset(PredictionTable_Carbon_FS3, species != 'Others'))
Box_Carbon <- Box_Carbon + theme(legend.position = 'top')
Box_Carbon <- Box_Carbon + xlab(label = '') + ylab(label = 'Prediction error')
Box_Carbon <- Box_Carbon + ggtitle(label = 'Carbon')

Table_species <- as.data.frame(table(PredictionTable_Carbon_FS3$species))
Table_species <- subset(Table_species, Freq > 0)
Table_species <- subset(Table_species, Var1 != 'Others')
Bar_Carbon <- qplot() + geom_bar(aes(x = Var1, y = Freq), stat = 'identity', data = as.data.frame(Table_species))
Bar_Carbon <- Bar_Carbon + xlab(label = '') + ylab(label = 'Frequency')


Filename <- paste0(PlotPath, 'PredictionError_FS3_SE.pdf')
pdf(file = Filename, onefile = T)
print(Plot_Nitrogen_FS3)
print(Plot_Carbon_FS3)
grid.arrange(Box_Carbon, Bar_Carbon, heights = c(2/3, 1/3))
dev.off()
