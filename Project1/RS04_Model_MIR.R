rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(GGally)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(pls)
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
tail(colnames(Data))
########################################################################
## Univariate Analysis
########################################################################
Colnames_Wave <- colnames(Data)[grep(pattern = 'Wavelength', x = colnames(Data))]

## Model1 <- lm(C18_0 ~ get(Colnames_Wave[1]), data = Data)
## Model2 <- lm(C15_1 ~ get(Colnames_Wave[1]), data = Data)

Univariate <- as.data.frame(cbind(WaveLengths = Colnames_Wave,
                                  pValue18 = rep(1, length(Colnames_Wave)),
                                  pValue18_res = rep(1, length(Colnames_Wave)),
                                  pValue15 = rep(1, length(Colnames_Wave)),
                                  pValue15_res = rep(1, length(Colnames_Wave))
))

Univariate$pValue18 <- as.numeric(as.vector(Univariate$pValue18))
Univariate$pValue15 <- as.numeric(as.vector(Univariate$pValue15))
Univariate$pValue18_res <- as.numeric(as.vector(Univariate$pValue18_res))
Univariate$pValue15_res <- as.numeric(as.vector(Univariate$pValue15_res))

for(i in 1:length(Colnames_Wave)){
  Model1 <- lm(C18_1 ~ get(Colnames_Wave[i]), data = Data)
  Univariate[i,'pValue18'] <- fn_get_pValue(lmobject = Model1)
  
  Model1a <- lm(Resid18 ~ get(Colnames_Wave[i]), data = Data)
  Univariate[i,'pValue18_res'] <- fn_get_pValue(lmobject = Model1a)
  
  Model2 <- lm(C15_0 ~ get(Colnames_Wave[i]), data = Data)
  Univariate[i,'pValue15'] <- fn_get_pValue(lmobject = Model2)
  
  Model2a <- lm(Resid15 ~ get(Colnames_Wave[i]), data = Data)
  Univariate[i,'pValue15_res'] <- fn_get_pValue(lmobject = Model2a)

  rm(Model1, Model2, Model1a, Model2a)
}

P18 <- qplot() + geom_histogram(aes(x = pValue18), data = Univariate)
P18_res <- qplot() + geom_histogram(aes(x = pValue18_res), data = Univariate)
P15 <- qplot() + geom_histogram(aes(x = pValue15), data = Univariate)
P15_res <- qplot() + geom_histogram(aes(x = pValue15_res), data = Univariate)

grid.arrange(P18, P18_res, P15, P15_res, ncol = 2)
#plot(x = rownames(Univariate), y = Univariate$pValue15, type = 'l')

Univariate$colSDs <- colSD(Data = Data[,Colnames_Wave])

Univariate <- Univariate[order(Univariate$colSDs, decreasing = T),]

Plot15 <- qplot() + geom_point(aes(y = pValue15, x = colSDs), data = Univariate) +
  ggtitle(label = 'pvalue15 with SD') +
  xlab(label = 'SD') + ylab(label = 'p-value of C15_0')

Plot18 <- qplot() + geom_point(aes(y = pValue18, x = colSDs), data = Univariate) +
  ggtitle(label = 'pvalue18 with SD') +
  xlab(label = 'SD') + ylab(label = 'p-value of C18_1')

Univariate$Select15 <- (Univariate$pValue15 < 0.1 | Univariate$colSDs > 0.15)
Univariate$Select18 <- (Univariate$pValue18 < 0.25 | Univariate$colSDs > 0.15)

sum(Univariate$Select15)
sum(Univariate$Select18)

Colnames15 <- as.vector(Univariate$WaveLengths[Univariate$Select15 == T])

########################################################################
## pls for C15
########################################################################
Data15 <- Data[,c(Colnames15, 'C15_0')]

M15_1 <- plsr(C15_0 ~ ., ncomp = 50, data = Data15)
plot(RMSEP(M15_1), legendpos = 'topright')

plot(M15_1, plottype = "scores", comps = 1:10)

explvar(object = M15_1)


########################################################################
## pls for C18
########################################################################
Colnames18 <- as.vector(Univariate$WaveLengths[Univariate$Select18 == T])
Data18 <- Data[,c(Colnames18, 'C18_1')]

M18_1 <- plsr(C18_1 ~ ., ncomp = 25, data = Data18)
plot(RMSEP(M18_1), legendpos = 'topright')

explvar(object = M18_1)

qplot() + geom_histogram(aes(x = C18_1), data = Data18)
qplot() + geom_histogram(aes(x = sqrt(C15_0)), data = Data15)
qplot() + geom_boxplot(aes(x = factor(Parity), y = C15_0), data = Data) + coord_flip()

qplot() + geom_boxplot(aes(x = factor(Parity), y = C18_1), data = Data) +
  geom_boxplot(aes(x = factor(Parity), y = C15_0), data = Data)

