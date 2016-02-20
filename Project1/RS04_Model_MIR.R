m(list = ls(all.names = TRUE))
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
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'
RPlotPath <- '~/Courses/Stat998_Spring2016/Project1/Plots/'

Filename <- paste0(RScriptPath, 'Data.RData')
load(Filename)
head(colnames(Data), 30)

########################################################################
## Univariate Analysis
########################################################################
Colnames_Wave <- colnames(Data)[grep(pattern = 'Wavelength', x = colnames(Data))]

## Model1 <- lm(C18_0 ~ get(Colnames_Wave[1]), data = Data)
## Model2 <- lm(C15_1 ~ get(Colnames_Wave[1]), data = Data)

Univariate <- as.data.frame(cbind(WaveLengths = Colnames_Wave,
                    pValue18 = rep(1, length(Colnames_Wave)),
                    pValue15 = rep(1, length(Colnames_Wave))
                    ))
Univariate$pValue18 <- as.numeric(as.vector(Univariate$pValue18))
Univariate$pValue15 <- as.numeric(as.vector(Univariate$pValue15))

for(i in 1:length(Colnames_Wave)){
Model1 <- lm(C18_1 ~ get(Colnames_Wave[i]), data = Data)
Univariate[i,'pValue18'] <- fn_get_pValue(lmobject = Model1)

Model2 <- lm(C15_0 ~ get(Colnames_Wave[i]), data = Data)
Univariate[i,'pValue15'] <- fn_get_pValue(lmobject = Model2)

rm(Model1, Model2)
}
                       
#qplot() + geom_line(aes(y = pValue15, x = rownames(Univariate)), data = Univariate)

plot(x = rownames(Univariate), y = Univariate$pValue15, type = 'l')
