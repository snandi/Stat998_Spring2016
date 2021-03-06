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
RScriptPath <- '~/Courses/Stat998_Spring2016/Project3/RScripts_Project3/'
DataPath <- '~/Courses/Stat998_Spring2016/Project3/Data/'
PlotPath <- '~/Courses/Stat998_Spring2016/Project3/Plots/'
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

## Steps for testing the number of knots

## Randomly split the data into train and test
Data <- Data_cal_FS3

fn_returnCV_MISPE <- function(Data_cal_FS3, Seed, Lambda_roughness, KnotFreq){
  TestTrain <- fn_splitTrainTest(
    Data = Data_cal_FS3, 
    colnames_ASD, 
    colnames_SE, 
    Seed = Seed
  )
  ASD_Train <- TestTrain[['ASD_Train']]
  SE_Train <- TestTrain[['SE_Train']]
  ## Create FDA objects of train set
  ASD_Train.fd <- fn_Data_FDObject(
    KnotFreq = KnotFreq,
    Data = TestTrain[['ASD_Train']]
  )$Data.fd
  
  SE_Train.fd <- fn_Data_FDObject(
    KnotFreq = KnotFreq,
    Data = TestTrain[['SE_Train']]
  )$Data.fd
  
  ## Create basis object for the coefficients
  WfdParobj <- fn_createBetaBasis(KnotFreq = KnotFreq/2, Lambda_roughness = Lambda_roughness)
  Wavelength <- c(350:2500)
  conbasis <- create.constant.basis(range(Wavelength))
  constfd  <- fd(matrix(1,1,ncol(ASD_Train)), conbasis)
  
  betalist  <- list(WfdParobj, WfdParobj)
  xfdlist <- list(constfd, SE_Train.fd)
  
  ## Run regression model
  fRegressout <- fRegress(y = ASD_Train.fd, xfdlist = xfdlist, betalist = betalist)
  names(fRegressout)
  
  betaestlist <- fRegressout[['betaestlist']]
  alpha_fd   <- betaestlist[[1]]$fd
  SE_beta_fd <- betaestlist[[2]]$fd
  
  ## Extract coefficients from model
  SE_Alpha <- eval.fd(evalarg = Wavelength, fdobj = alpha_fd)
  SE_Beta <- eval.fd(evalarg = Wavelength, fdobj = SE_beta_fd)
  
  ## Predict SE fit from SE test data
  SE_Test <- TestTrain[['SE_Test']]
  ASD_Test <- TestTrain[['ASD_Test']]
  SE_Fit <- apply(X = SE_Test, MARGIN = 2, FUN = function(v){v * SE_Beta})
  SE_Fit <- SE_Fit + SE_Alpha[,1]
  
  ## Estimate prediction error between SE fit and ASD test
  Pred_Error <- ASD_Test - SE_Fit
  ISPE <- apply(X = Pred_Error^2, MARGIN = 2, FUN = sum)
  MISPE <- round(sum(ISPE)/length(ISPE), 4)
  
  ## Estimate prediction error between SE fit and ASD test
  Error <- ASD_Test - SE_Test
  ISPE_0 <- apply(X = Error^2, MARGIN = 2, FUN = sum)
  MISPE_0 <- round(sum(ISPE_0)/length(ISPE_0), 4)

    Curve <- 10
  Plot <- qplot(x = Wavelength, y = SE_Fit[,Curve], geom = 'line') +
    geom_line(aes(y = ASD_Test[,Curve]), col = 'green') +
    geom_line(aes(y = SE_Test[,Curve]), col = 'blue') +
    ylab(label = '') + xlab(label = paste('MISPE', MISPE)) +
    ggtitle(label = paste('KnotFreq', KnotFreq, 'Lambda', Lambda_roughness))
  print(Plot)
  return(list(MISPE_0 = MISPE_0, MISPE = MISPE))
  
}

#fn_returnCV_MISPE(Data_cal_FS3, Seed = 15, Lambda_roughness = 0.01, KnotFreq = 8)

Seeds <- c(1:50)
KnotFreq <- c(6, 10, 14, 16, 20)
CVData <- NULL
for(Knot in KnotFreq){
  CVData <- rbind(CVData, cbind(Seeds, Knot))
}
CVData <- as.data.frame(CVData)
CVData$MISPE_Before = CVData$MISPE_After <- 0

for(Row in 1:nrow(CVData)){
  MISPE_Output <- fn_returnCV_MISPE(
    Data_cal_FS3, 
    Seed = CVData[Row,'Seed'], 
    Lambda_roughness = 0.01, 
    KnotFreq = CVData[Row, 'Knot']
  )
  CVData[Row,'MISPE_Before'] <- MISPE_Output[[1]]
  CVData[Row,'MISPE_After'] <- MISPE_Output[[2]]
}

Filename <- paste0(RScriptPath, 'CVResults.RData')
save(CVData, file = Filename)

Box1 <- qplot() + geom_boxplot(aes(x = Knot, y = MISPE_After, fill = factor(Knot)), data = CVData) +
xlab(lab = 'Knot frequency') + ylab(lab = 'MISPE')


