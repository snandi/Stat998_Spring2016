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
library(Registration)
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
#source(paste0(RScriptPath, 'fn_fRegress.R'))
########################################################################
## Load calibration data
########################################################################
Filename <- 'Common_SE_ASD_no_meta.csv'
Data_cal <- read.csv(file = paste0(DataPath, Filename), header = T)

colnames_SE <- colnames(Data_cal)[grep(pattern = "Wave_.*x", x = colnames(Data_cal))]
colnames_ASD <- colnames(Data_cal)[grep(pattern = "Wave_.*y", x = colnames(Data_cal))]
colnames_Other <- colnames(Data_cal) %w/o% c(colnames_SE, colnames_ASD)

Data_cal <- Data_cal[-c(209:212),]

########################################################################
## create training and testing data
########################################################################
Data_cal_FS3 <- subset(Data_cal, Instrument.y == 'FS3')
NRow_FS3 <- nrow(Data_cal_FS3)
KnotFreq <- 14
Lambda_roughness <- 1

Data_ASD <- t(as.matrix(Data_cal_FS3[,colnames_ASD]))
Data_SE <- t(as.matrix(Data_cal_FS3[,colnames_SE]))

Data_ASD.fd <- fn_Data_FDObject(
  KnotFreq = KnotFreq,
  Data = Data_ASD
)$Data.fd

Data_SE.fd <- fn_Data_FDObject(
  KnotFreq = KnotFreq,
  Data = Data_SE
)$Data.fd

## Create basis object for the coefficients
WfdParobj <- fn_createBetaBasis(
  KnotFreq = 14, 
  norder = 5,
  Lambda_roughness = Lambda_roughness
)
Wavelength <- c(350:2500)
conbasis <- create.constant.basis(range(Wavelength))
constfd  <- fd(matrix(1,1,ncol(Data_ASD)), conbasis)

betalist  <- list(WfdParobj, WfdParobj)
xfdlist <- list(constfd, Data_SE.fd)

## Run regression model
fRegressout <- fRegress(y = Data_ASD.fd, xfdlist = xfdlist, betalist = betalist)
names(fRegressout)

betaestlist <- fRegressout[['betaestlist']]
alpha_fd   <- betaestlist[[1]]$fd
SE_beta_fd <- betaestlist[[2]]$fd
betastderrlist <- fRegressout$betastderrlist
plot(SE_beta_fd)

SE_Alpha <- eval.fd(evalarg = Wavelength, fdobj = alpha_fd)
SE_Beta <- eval.fd(evalarg = Wavelength, fdobj = SE_beta_fd)

Plot_Beta <- qplot(x = Wavelength, y = SE_Beta, geom = 'line') +
  ylab(label = '') + ylim(c(0, 1)) + 
  ggtitle(label = paste('Coefficient function'))
Plot_Beta

Plot_Alpha <- qplot(x = Wavelength, y = SE_Alpha, geom = 'line') +
  ylab(label = '') + ylim(c(0, 1)) +
  ggtitle(label = paste('Intercept function'))
Plot_Alpha

Filename <- paste0(PlotPath, 'BetaPlots_FinalModel.pdf')
pdf(file = Filename, onefile = T)
plot(Plot_Alpha)
plot(Plot_Beta)
dev.off()


SE_hat_fdobj <- fRegressout[['yhatfdobj']]
SE_Fit <- eval.fd(evalarg = Wavelength, fdobj = SE_hat_fdobj$fd)

Filename <- paste0(PlotPath, 'Plots_SE_Fitted.pdf')
pdf(file = Filename, onefile = T)
for(Curve in 1:NRow_FS3){
  Plot <- qplot(x = Wavelength, y = SE_Fit[,Curve], geom = 'line') +
    geom_line(aes(y = Data_ASD[,Curve]), col = 'blue') +
    geom_line(aes(y = Data_SE[,Curve]), col = 'red') +
    ylab(label = '') + 
    xlab('Red: Original SE, Blue: ASD, Black: Fitted SE') +
    ggtitle(label = paste(as.vector(Data_cal_FS3[50, 'Spectra'])))
  plot(Plot)
  rm(Plot)
}
dev.off()


Diff_ASD <- diff(Data_ASD)
Diff_SE <- diff(Data_SE)
Diff_SE_Fit <- diff(SE_Fit)
Plot_D1 <- qplot(x = Wavelength[-1], y = Diff_SE_Fit[,Curve], geom = 'line') +
  geom_line(aes(y = Diff_ASD[,Curve]), col = 'green') +
  geom_line(aes(y = Diff_SE[,Curve]), col = 'blue') +
  ylab(label = '') + 
  ggtitle(label = paste('Curve', Curve))
Plot_D1

## Estimate prediction error between SE fit and ASD test
Pred_Error <- Data_ASD - SE_Fit
ISPE <- apply(X = Pred_Error^2, MARGIN = 2, FUN = sum)
MISPE <- round(sum(ISPE)/length(ISPE), 4)

## Estimate prediction error between SE test and ASD test
Error <- Data_ASD - Data_SE
ISPE_0 <- apply(X = Error^2, MARGIN = 2, FUN = sum)
MISPE_0 <- round(sum(ISPE_0)/length(ISPE_0), 4)

MISPE
MISPE_0
