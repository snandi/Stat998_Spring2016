rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is Prob 2, HW 2, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/HoneyBee/'

Filename <- paste0(RScriptPath, 'HoneyBee.csv')
Data <- read.csv(Filename, header = T)

Data <- na.is.zero(Data)

str(Data)

Plot1 <- qplot() + geom_point(aes(x = Treatment, y = Mortality_4to6_Pct, pch = Pattern),
                     data = Data, size = 3) +
  geom_jitter() +
  facet_grid(. ~ PER) +
  ggtitle(label = 'Mortality between day 4 and day 6') +
  ylab(label = 'Mortality (% dead)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'top')

Plot2 <- qplot() + geom_point(aes(x = Treatment, y = Mortality_1to10_Pct, pch = Pattern),
                     data = Data, size = 3) +
  geom_jitter() +
  facet_grid(. ~ PER) +
  ggtitle(label = 'Mortality upto day 9') +
  ylab(label = 'Mortality (% dead)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'top')

Filename.Plot <- paste0(RScriptPath, 'Plots.pdf')
pdf(file = Filename.Plot, onefile = T)
Plot1
Plot2
dev.off()

qplot() + geom_histogram(aes(x = Initial), data = Data)
