########################################################################## 
## Create a fd object of a curve, after smoothing it with bsplies
## This works even if Curve is a matrix
########################################################################## 
fn_createCurve_FDObject <- function(lambdas = exp(-5:5), Curve = Median, Xaxis = PixelPos, 
                                    pbasis = NULL){
  if(is.null(pbasis)){
    BasisBreaks <- seq(from = min(Xaxis), to = max(Xaxis), length.out = length(Xaxis)/3)
    pbasis <- create.bspline.basis(rangeval = range(Xaxis), norder = 4, breaks = BasisBreaks) 
  }
  gcvs <- rep(0,length(lambdas))
  for(i in 1:length(lambdas)){ 
    pPar <- fdPar(pbasis,int2Lfd(2),lambdas[i])
    gcvs[i] <- mean(smooth.basis(argvals = Xaxis,y = Curve,fdParobj = pPar)$gcv)
  }
  best <- which.min(gcvs)
  Lambda_forMedian <- lambdas[best]
  #print(Lambda_forMedian)
  pPar <- fdPar(pbasis,int2Lfd(2),lambda = Lambda_forMedian)
  Median_ToReg_fd <- smooth.basis(argvals = Xaxis,y = Curve,fdParobj = pPar)
  return(Median_ToReg_fd)
}

###########################################################################
## This function plots ASD and SE of same leaf together
###########################################################################
plot_pairs <- function(LongData){
  Plot <- qplot() + geom_line(aes(x = Wavelength, y = Intensity, col = factor(SimpleInstr)), data = LongData, size = 1.5) +
    ggtitle(label = LongData$Spectra[1]) + 
    ylab(label = '') + 
    theme(legend.position = 'top') +
    guides(color = guide_legend(title = ''))
  
  return(Plot)
}

###########################################################################
## This function plots the difference, one leaf at a time
###########################################################################
plot_diff <- function(DiffData){
  Plot <- qplot() + geom_line(aes(x = Wavelength, y = Diff), data = DiffData, size = 1.5)
  #Plot <- Plot + ggtitle(label = LongData$Spectra[1])
  Plot <- Plot + ylab(label = '')
  Plot <- Plot + theme(legend.position = 'top')
  Plot <- Plot + geom_hline(yintercept = 0, lty = 2)

  return(Plot)
}

###########################################################################
## This function prepares the data in the long format, for 1 leaf at a time
###########################################################################
fn_longData_byRow <- function(Data_cal, Row){
  Wavelength <- 350:2500
  Intensity.ASD <- as.numeric(as.vector(Data_cal[Row, colnames_ASD]))
  
  LongData.ASD <- as.data.frame(
    cbind(
      Wavelength = Wavelength, 
      Intensity = as.numeric(Intensity.ASD), 
      SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.y']),
      Instr = as.vector(Data_cal[Row, 'Instrument.y']),
      Instrument = paste(as.vector(Data_cal[Row, 'SimpleInstrument.y']), as.vector(Data_cal[Row, 'Instrument.y']), sep='_'),
      Date = Data_cal[Row, 'Date.y'],
      Name = as.vector(Data_cal[Row, 'Name_long.y']),
      Species = as.vector(Data_cal[Row, 'Species']),
      SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
      Spectra = as.vector(Data_cal[Row, 'Spectra'])
  ), stringsAsFactors = F)
  
  Intensity.SE <- as.numeric(as.vector(Data_cal[Row, colnames_SE]))
  LongData.SE <- as.data.frame(
    cbind(
      Wavelength = Wavelength, 
      Intensity = as.numeric(Intensity.SE), 
      SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.x']),
      Instr = as.vector(Data_cal[Row, 'Instrument.x']),
      Instrument = as.vector(Data_cal[Row, 'SimpleInstrument.x']),
      Date = Data_cal[Row, 'Date.x'],
      Name = as.vector(Data_cal[Row, 'Name_long.x']),
      Species = as.vector(Data_cal[Row, 'Species']),
      SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
      Spectra = as.vector(Data_cal[Row, 'Spectra'])
    ), stringsAsFactors = F)
  
  LongData <- rbind(LongData.ASD, LongData.SE)
  LongData <- within(data = LongData,{
    Intensity <- as.numeric(Intensity)
    Wavelength <- as.numeric(Wavelength)
  })

  ######## Difference between the two instruments
  Intensity.Diff <- Intensity.ASD - Intensity.SE
  DiffData <- as.data.frame(cbind(Wavelength = Wavelength, 
                                  Diff = as.numeric(Intensity.Diff), 
                                  Name = as.vector(Data_cal[Row, 'Name_long.x']),
                                  Species = as.vector(Data_cal[Row, 'Species']),
                                  SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
                                  Spectra = as.vector(Data_cal[Row, 'Spectra'])
  ), stringsAsFactors = F)
  DiffData <- within(data = DiffData,{
    Diff <- as.numeric(Diff)
    Wavelength <- as.numeric(Wavelength)
  })
  
  return(list(DiffData = DiffData, LongData = LongData))  
}

########################################################################## 
## Returns the Filename containing the coefficients of the PLS models
########################################################################## 
fn_getFilename <- function(Comp = c('ADF', 'Carbon', 'LMA', 'ADL', 'Cellulose', 'Nitrogen')){
  Filename <- c(
    'FFT_Leaf_ADF_PLSR_Coefficients_13comp.csv', 
    'FFT_Leaf_Carbon_PLSR_Coefficients_11comp.csv',
    'FFT_Leaf_LMA_PLSR_Coefficients_7comp.csv',
    'FFT_Leaf_ADL_PLSR_Coefficients_13comp.csv',
    'FFT_Leaf_Cellulose_PLSR_Coefficients_13comp.csv',
    'FFT_Leaf_Nitrogen_PLSR_Coefficients_11comp.csv'
  )
  
  Component <- c('ADF', 'Carbon', 'LMA', 'ADL', 'Cellulose', 'Nitrogen')
  
  FileMap <- as.data.frame(
    cbind(Component, Filename), 
    stringsAsFactors = FALSE
  )
  
  File.Comp <- FileMap$Filename[FileMap$Component == Comp]
  return(File.Comp)
}

########################################################################## 
## Returns the File containing the coefficients of the PLS models
########################################################################## 
fn_loadScoresByComp <- function(Comp, DataPath){
  File.Comp <- fn_getFilename(Comp = Comp)  
  Scores.Comp <- read.table(file = paste0(DataPath, File.Comp), header = T, sep = ',')
  Scores.Comp$X <- as.vector(Scores.Comp$X)
  Scores.Comp$Wavelength <- as.numeric(gsub(pattern = 'Wave_', replacement = '', x = Scores.Comp$X))
  Scores.Comp[1,'Wavelength'] <- 1
  names(Scores.Comp)[2] <- Comp
  return(Scores.Comp[,c('Wavelength', Comp)])
  
}

########################################################################## 
## Returns data with two columns, one with ASD and other with SE
## Other predictions will be added to this dataset
########################################################################## 
fn_longTowide <- function(LongData){
  
  Instrument <- unique(LongData$Instrument)[1]
  
  WideData_ASD <- (subset(LongData, Instrument == Instrument))[,c('Spectra', 'Wavelength', 'Intensity')]
  names(WideData_ASD)[3] <- Instrument
  
  WideData_SE <- (subset(LongData, Instrument == 'SE'))[,c('Spectra', 'Wavelength', 'Intensity')]
  names(WideData_SE)[3] <- 'SE'
  
  WideData <- merge(x = WideData_ASD, y = WideData_SE[,c('Wavelength', 'SE')], by = 'Wavelength')
  
}
