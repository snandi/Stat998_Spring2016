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
source(paste0(RScriptPath, 'fn_fRegress.R'))
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

set.seed(11)

NRow_Train <- round(0.8*NRow_FS3, 0)
NRow_Train <- 40

Rows_Train <- sample(x = (1:NRow_FS3), size = NRow_Train, replace = FALSE)
Rows_Test <- (1:NRow_FS3) %w/o% Rows_Train

Rows_Test

ASD_FS3_Train <- t(as.matrix(Data_cal_FS3[Rows_Train,colnames_ASD]))
SE_Train <- t(as.matrix(Data_cal_FS3[Rows_Train,colnames_SE]))

ASD_FS3_Test <- t(as.matrix(Data_cal_FS3[Rows_Test,colnames_ASD]))
SE_Test <- t(as.matrix(Data_cal_FS3[Rows_Test,colnames_SE]))

########################################################################
## create fda objects
########################################################################
Wavelength <- c(350:2500)
NWaves <- length(Wavelength)
BasisBreaks <- seq(
  from        = min(Wavelength), 
  to          = max(Wavelength), 
  length.out  = NWaves/10)
norder <- 4
nbasis <- length(BasisBreaks) + norder - 2

basisobj <- create.bspline.basis(
  rangeval  = range(Wavelength), 
  norder    = norder, 
  breaks    = BasisBreaks
)

### ASD_FS3_Train
ASD_FS3_Train_FDO <- fn_createCurve_FDObject(
  lambdas = exp(seq(-5,1,0.5)), 
  Curve = ASD_FS3_Train,
  Xaxis = Wavelength, 
  pbasis = basisobj
)

ASD_FS3_Train.fd <- ASD_FS3_Train_FDO[['Curve.Sm']]
gcvs_ASD_FS3_Train <- ASD_FS3_Train_FDO[['gcvs']]
Lambda_ASD_FS3_Train <- ASD_FS3_Train_FDO[['Lambda_best']]
plot(gcvs_ASD_FS3_Train)

ASD_FS3_Train.fit <- eval.fd(evalarg = Wavelength, fdobj = ASD_FS3_Train.fd$fd)

### SE_Train
SE_Train_FDO <- fn_createCurve_FDObject(
  lambdas = exp(seq(-5,1,0.5)), 
  Curve = SE_Train,
  Xaxis = Wavelength, 
  pbasis = basisobj
)

SE_Train.fd <- SE_Train_FDO[['Curve.Sm']]
gcvs_SE_Train <- SE_Train_FDO[['gcvs']]
Lambda_SE_Train <- SE_Train_FDO[['Lambda_best']]

plot(gcvs_SE_Train)
SE_Train.fit <- eval.fd(evalarg = Wavelength, fdobj = SE_Train.fd$fd)


### Create wavelength basis
Wavelength <- c(350:2500)
NWaves <- length(Wavelength)
BasisBreaks <- seq(
  from        = min(Wavelength), 
  to          = max(Wavelength), 
  length.out  = NWaves/10)
norder <- 6
nbasis <- length(BasisBreaks) + norder - 2
Lambda <- 0.0001
basisobj <- create.bspline.basis(
  rangeval  = range(Wavelength), 
  norder    = norder, 
  breaks    = BasisBreaks
)
Wfd0 <- fd(matrix(data = 0, nrow = basisobj$nbasis, ncol = 1), basisobj)
WfdParobj <- fdPar(fdobj = Wfd0, Lfdobj = 3, lambda = Lambda)

### Constant basis
conbasis <- create.constant.basis(range(Wavelength))
constfd  <- fd(matrix(1,1,NRow_Train), conbasis)

betalist  <- list(WfdParobj, WfdParobj)
xfdlist <- list(constfd, SE_Train.fd$fd)

### Concurrent regression 
source(paste0(RScriptPath, 'fn_fRegress.R'))
fRegressout <- fRegress_SN(y = ASD_FS3_Train.fd$fd, xfdlist = xfdlist, betalist = betalist)

