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

### Load bootstrap results
Filename <- paste0(RScriptPath, 'Bootstrap_Results.RData')
load(Filename)
names(BootResults)
rowSE_Alpha <- BootResults$rowSE_Alpha
rowSE_Beta <- BootResults$rowSE_Beta

SE_Alpha_wBoot <- as.data.frame(cbind(
  Mean = SE_Alpha, 
  Upper = (SE_Alpha + 2*rowSE_Alpha),
  Lower = (SE_Alpha - 2*rowSE_Alpha)
))
colnames(SE_Alpha_wBoot) <- c('Mean', 'Upper', 'Lower')
SE_Alpha_wBoot$Wavelength <- Wavelength

Plot_Alpha <- qplot() + 
  geom_smooth(aes(x = Wavelength, y = Mean, ymin = Lower, ymax = Upper), 
              col = 'gray40', data = SE_Alpha_wBoot, stat = "identity", size = 0.5) +
  ylab(label = '') + ylim(c(0, 1)) + 
  ggtitle(label = paste('Intercept function with confidence interval')) +
  theme_bw()

Plot_Alpha

SE_Beta_wBoot <- as.data.frame(cbind(
  Mean = SE_Beta, 
  Upper = (SE_Beta + 2*rowSE_Beta),
  Lower = (SE_Beta - 2*rowSE_Beta)
))
colnames(SE_Beta_wBoot) <- c('Mean', 'Upper', 'Lower')
SE_Beta_wBoot$Wavelength <- Wavelength

Plot_Beta <- qplot() + 
  geom_smooth(aes(x = Wavelength, y = Mean, ymin = Lower, ymax = Upper), 
              col = 'gray60', data = SE_Beta_wBoot, stat = "identity", size = 0.5) +
  ylab(label = '') + ylim(c(0, 1)) + 
  ggtitle(label = paste('Intercept function with confidence interval')) +
  theme_bw()

Plot_Beta

Filename <- paste0(PlotPath, 'BetaPlots_Bootstrap.pdf')
pdf(file = Filename, onefile = T)
plot(Plot_Alpha)
plot(Plot_Beta)
dev.off()
