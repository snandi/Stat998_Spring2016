rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is Prob 1, HW 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)

########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Diesel/'

Filename <- paste0(RScriptPath, 'diesel.dat')
Data <- read.table(Filename, header = T, sep = '\t')

str(Data)

########################################################################
## Summary Plots
########################################################################
Plot1 <- qplot() + geom_boxplot(aes(x = factor(speed), y = delay, fill = factor(speed)), data = Data) + ggtitle('Delay vs speed') +
  xlab(label = 'Speed') + ylab(label = 'Ignition Delay') +
  theme(legend.position = '')

Plot2 <- qplot() + geom_boxplot(aes(x = factor(load), y = delay, fill = factor(load)),
                                data = Data) +
  ggtitle('Delay vs load') +
  xlab(label = 'Load') + ylab(label = 'Ignition Delay') +
  theme(legend.position = '')

Plot3 <- qplot() + geom_boxplot(aes(x = factor(timing), y = delay, fill = factor(timing)), data = Data) + ggtitle('Delay vs timing') +
  xlab(label = 'Timing') + ylab(label = 'Ignition Delay') +
  theme(legend.position = '')

Plot4 <- qplot() + geom_point(aes(x = alcohol, y = delay), data = Data) +
  ggtitle('Delay vs alcohol') +
  xlab(label = 'Alcohol') + ylab(label = 'Ignition Delay')

Plot5 <- qplot() + geom_point(aes(x = 1/temp, y = log(delay)), data = Data) +
  ggtitle('log(Delay) vs 1/temp') +
  xlab(label = '1/Temp') + ylab(label = 'log(Ignition Delay)')
  
Plot6 <- qplot() + geom_point(aes(x = press, y = log(delay)), data = Data) +
  ggtitle('log(Delay) vs pressure') +
  xlab(label = 'Pressure') + ylab(label = 'log(Ignition Delay)')

Plot7 <- qplot() + geom_boxplot(aes(x = factor(load), y = press), data = Data) +
  ggtitle('pressure vs load') +
  xlab(label = 'Load') + ylab(label = 'Pressure')

Plot8 <- qplot() + geom_point(aes(x = temp, y = press), data = Data) +
  ggtitle('pressure vs temperature') +
  xlab(label = 'Temperature') + ylab(label = 'Pressure')

Filename.Plot <- paste0(RScriptPath, 'Plots.pdf')
pdf(file = Filename.Plot, onefile = T)
Plot1
Plot2
Plot3
Plot4
Plot5
Plot6
Plot7
Plot8
dev.off()



