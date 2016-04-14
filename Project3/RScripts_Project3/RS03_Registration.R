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
## Load calibration data
########################################################################
Filename <- 'Common_SE_ASD_no_meta.csv'
Data_cal <- read.csv(file = paste0(DataPath, Filename), header = T)

colnames_SE <- colnames(Data_cal)[grep(pattern = "Wave_.*x", x = colnames(Data_cal))]
colnames_ASD <- colnames(Data_cal)[grep(pattern = "Wave_.*y", x = colnames(Data_cal))]
colnames_Other <- colnames(Data_cal) %w/o% c(colnames_SE, colnames_ASD)

Row <- 57
Data <- fn_longData_byRow(Data_cal, Row = Row )
LongData <- Data[['LongData']]
LongData <- subset(LongData, Wavelength >= 500 & Wavelength <= 1200)
Wavelength <- 500:1200

########################################################################
## create FDA objects
########################################################################
asd <- subset(LongData, SimpleInstr == 'ASD')[,'Intensity']
se <- subset(LongData, SimpleInstr == 'SE')[,'Intensity']
asd.Sm <- fn_createCurve_FDObject(
  Curve = asd,
  Xaxis = Wavelength
)

se.Sm <- fn_createCurve_FDObject(
  Curve = se,
  Xaxis = Wavelength
)

########################################################################
## Registration
########################################################################
Lambda <- 0.02

BasisBreaks <- seq(
  from        = min(Wavelength), 
  to          = max(Wavelength), 
  length.out  = length(Wavelength)/3)

basisobj <- create.bspline.basis(
  rangeval  = range(Wavelength), 
  norder    = 3, 
  breaks    = BasisBreaks)

Wfd0 <- fd(matrix(data = 0, nrow = basisobj$nbasis, ncol = 1), basisobj)
WfdParobj <- fdPar(fdobj = Wfd0, lambda = Lambda)

Regfd <- register.fd(
  y0fd      = asd.Sm$fd, 
  yfd       = se.Sm$fd, 
  WfdParobj = WfdParobj, 
  dbglev    = 1,
  periodic  = FALSE, 
  crit      = 2) 

Regfd_eval <- eval.fd(evalarg = Wavelength, fdobj = Regfd$regfd)
Diff <- asd - se
Diff_reg <- asd - as.vector(Regfd_eval)

DiffPlot <- qplot() + geom_line(aes(x = Wavelength, y = Diff), size = 1)
DiffPlot <- DiffPlot + geom_line(aes(x = Wavelength, y = Diff_reg), size = 1.5, col = 'red')
#Plot <- Plot + ggtitle(label = LongData$Spectra[1])
DiffPlot <- DiffPlot + ylab(label = '')
DiffPlot <- DiffPlot + geom_hline(yintercept = 0, lty = 2)
DiffPlot

Plot <- qplot() + geom_line(aes(x = Wavelength, y = asd), col = 'orangered')
Plot <- Plot + geom_line(aes(x = Wavelength, y = se), col = 'black')
Plot <- Plot + geom_line(aes(x = Wavelength, y = Regfd_eval), size = 1.5, col = 'black', lty = 2)
Plot <- Plot + ylab(label = '')
Plot <- Plot + ggtitle(label = 'After registration')
Plot







