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
source('~/Project_CurveReg/RScripts_CurveReg/fn_Library_CurveReg.R')
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

Corr.Cow <- corr.test(Data1[,Cols_Cow])
View(round(Corr.Cow$r, 3))
View(round(Corr.Cow$p, 3))

xtable(round(Corr.Cow$r, 3))

Corr.Acid <- corr.test(Data1[,Cols_Acid])
View(round(Corr.Acid$r, 2))
View(round(Corr.Acid$p, 2))

xtable(round(Corr.Acid$r, 3))

C15_parity <- qplot() + geom_boxplot(aes(x = factor(Parity), y = C15_0), data = Data1) +
  geom_jitter(aes(x = factor(Parity), y = C15_0), data = Data1, size = 3, width = 0.2) +
  ggtitle('C15:0 vs parity') + ylab(label = 'C15:0 concentration') + xlab(label = 'Parity') +
  theme_gray(base_size = 14)

C18_parity <- qplot() + geom_boxplot(aes(x = factor(Parity), y = C18_1), data = Data1) +
  geom_jitter(aes(x = factor(Parity), y = C18_1), data = Data1, size = 3, width = 0.2) +
  ggtitle('C18:1 vs parity') + ylab(label = 'C18:1 concentration') + xlab(label = 'Parity') +
  theme_gray(base_size = 14)

Filename <- paste0(RPlotPath, 'CowPlots.pdf')
pdf(file = Filename, onefile = TRUE)
C15_parity
C18_parity
dev.off()

qplot() + 
  geom_point(aes(x = Intake, y = C18_1, col = factor(Parity)), 
             data = subset(Data1, Parity < 3), size = 5)

qplot() + 
  geom_point(aes(x = Days, y = C18_1, col = factor(Parity), shape = factor(Parity)), 
             data = subset(Data1, Parity < 3), size = 5)

Filename <- paste0(RPlotPath, 'Pairwiseplots.pdf')
pdf(file = Filename, onefile = T, pointsize = 6)
ggpairs(Data1[,Cols_Cow])
ggpairs(Data1[,Cols_Acid])

for(Col in Cols_Acid){
  print(ggpairs(Data1[,c(Cols_Cow, Col)]))
}
dev.off()

########################################################################
## Geom_smooth plot
########################################################################
Filename <- paste0(RScriptPath, 'Data.RData')
load(Filename)
Colnames_Wave <- colnames(Data)[grep(pattern = 'Wavelength', x = colnames(Data))]

Means <- colMeans(Data[,Colnames_Wave])
SDs <- colSD(Data[,Colnames_Wave])
SEs <- colSE(Data[,Colnames_Wave])

WaveLength <- as.numeric(gsub(pattern = 'Wavelength_', replacement = '', x = Colnames_Wave))

Plot_SE <- qplot() + 
  geom_smooth(aes(ymin = (Means - 2*SEs), ymax = (Means + 2*SEs), x = WaveLength, y = Means), 
              col = 'gray40', fill = 'gray10',
              stat = 'identity')

Plot_SD <- qplot() + 
  geom_smooth(aes(ymin = (Means - SDs), ymax = (Means + SDs), x = WaveLength, y = Means), 
              col = 'black', fill = 'gray20',
              stat = 'identity') +
  ggtitle(label = 'Mean +/- standard deviation of MIR data') +
  ylab(label = 'Mean +/- SD') +
  theme_gray(base_size = 14)

Filename <- paste0(RPlotPath, 'MIR_SDPlot.pdf')
pdf(file = Filename)
Plot_SD
dev.off()


Data_Wave <- as.data.frame(cbind(WaveLength = WaveLength, t(Data[,Colnames_Wave])))
colnames(Data_Wave) <- c('WaveLength', paste0('Cow', 1:59))
colnames_Cow <- paste0('Cow', 1:59)

# Plot1 <- fn_plotMultCurves(
#   Data = Data_Wave, 
#   ColsToPlot = colnames_Cow,
#   XVar = WaveLength,
#   Xlab = 'Wavelength', Ylab = 'MIR'
#   )
