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
  
  LongData.ASD <- as.data.frame(cbind(Wavelength = Wavelength, 
                                      Intensity = as.numeric(Intensity.ASD), 
                                      SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.y']),
                                      Instr = as.vector(Data_cal[Row, 'Instrument.y']),
                                      Date = Data_cal[Row, 'Date.y'],
                                      Name = as.vector(Data_cal[Row, 'Name_long.y']),
                                      Species = as.vector(Data_cal[Row, 'Species']),
                                      SpeciesCat = as.vector(Data_cal[Row, 'SpeciesCat']),
                                      Spectra = as.vector(Data_cal[Row, 'Spectra'])
  ), stringsAsFactors = F)
  
  Intensity.SE <- as.numeric(as.vector(Data_cal[Row, colnames_SE]))
  LongData.SE <- as.data.frame(cbind(Wavelength = Wavelength, 
                                     Intensity = as.numeric(Intensity.SE), 
                                     SimpleInstr = as.vector(Data_cal[Row, 'SimpleInstrument.x']),
                                     Instr = as.vector(Data_cal[Row, 'Instrument.x']),
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
