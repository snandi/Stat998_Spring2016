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

P18 <- qplot() + geom_histogram(aes(x = pValue18), data = Univariate) +
  xlab(label = 'p value') + ylab(label = 'frequency') + 
  ggtitle(label = 'Univariate p-values of fitting C18:1 with MIR data')

P18_res <- qplot() + geom_histogram(aes(x = pValue18_res), data = Univariate) +
  xlab(label = 'p value') + ylab(label = 'frequency') + 
  ggtitle(label = 'Univariate p-values of fitting residuals of Model1 with MIR data')


P15 <- qplot() + geom_histogram(aes(x = pValue15), data = Univariate) +
xlab(label = 'p value') + ylab(label = 'frequency') + 
  ggtitle(label = 'Univariate p-values of fitting C15:0 with MIR data')
P15_res <- qplot() + geom_histogram(aes(x = pValue15_res), data = Univariate) +
  xlab(label = 'p value') + ylab(label = 'frequency') + 
  ggtitle(label = 'Univariate p-values of fitting residuals of Model2 with MIR data')

Filename <- paste0(RPlotPath, 'pValue_MIR.pdf')
pdf(file = Filename, onefile = T)
plot(P18)
plot(P18_res)
plot(P15)
plot(P15_res)
dev.off()


Univariate$colSDs <- colSD(Data = Data[,Colnames_Wave])

Univariate <- Univariate[order(Univariate$colSDs, decreasing = T),]

Plot15 <- qplot() + geom_point(aes(y = pValue15, x = colSDs), data = Univariate) +
  ggtitle(label = 'Selection of Wavelengths for C15:0') +
  xlab(label = 'SD') + ylab(label = 'p-value of C15_0') +
  annotate("rect", xmin = range(Univariate$pValue15_res)[1], 
           xmax = range(Univariate$pValue15_res)[2], 
           ymin = 0, ymax = 0.1, alpha = .2, fill = 'blue') +
  annotate("rect", xmin = quantile(Univariate$colSDs, probs = 0.90),
           xmax = range(Univariate$pValue15_res)[2], 
           ymin = 0, ymax = 1, alpha = .2, fill = 'blue')
Plot15

Plot18 <- qplot() + geom_point(aes(y = pValue18_res, x = colSDs), data = Univariate) +
  ggtitle(label = 'Selection of Wavelengths for C18:1') +
  xlab(label = 'Std Dev') + ylab(label = 'p-value of residual vs wavelength') +
  geom_hline(yintercept = 0.15) +
  annotate("rect", xmin = range(Univariate$pValue18_res)[1], 
           xmax = range(Univariate$pValue18_res)[2], 
           ymin = 0, ymax = 0.1, alpha = .2, fill = 'blue') +
  annotate("rect", xmin = quantile(Univariate$colSDs, probs = 0.90),
           xmax = range(Univariate$pValue18_res)[2], 
           ymin = 0, ymax = 1, alpha = .2, fill = 'blue')
Plot18

Filename <- paste0(RPlotPath, 'pValueSD.pdf')
pdf(file = Filename, onefile = T )
plot(Plot18)
plot(Plot15)
dev.off()


Univariate$Select15 <- (Univariate$pValue15 < 0.1 | Univariate$colSDs > quantile(Univariate$colSDs, probs = 0.90))
Univariate$Select18 <- (Univariate$pValue18 < 0.1 | Univariate$colSDs > quantile(Univariate$colSDs, probs = 0.90))

sum(Univariate$Select15)
sum(Univariate$Select18)

Colnames15 <- as.vector(Univariate$WaveLengths[Univariate$Select15 == T])

########################################################################
## pls for C15
########################################################################
Colnames15 <- as.vector(Univariate$WaveLengths[Univariate$Select15 == T])
Data15 <- Data[,c(Colnames15, 'Resid15')]
Seed <- 11
set.seed(Seed)
inTrain <- caret::createDataPartition(y = 1:nrow(Data15), p = 0.75, list = FALSE)
Train15 <- Data15[ inTrain, ]
Test15 <- Data15[ -inTrain, ]

M15_1 <- plsr(Resid15 ~ ., ncomp = 30, data = Train15)
summary(M15_1)

M15_2 <- plsr(Resid15 ~ ., ncomp = 12, data = Train15)
summary(M15_2)

Predict15 <- predict(M15_2, ncomp = 12, newdata = Test15)
PredictPlot15 <- qplot() + 
  geom_point(aes(x = Test15$Resid15, y = Predict15[,,])) +
  ggtitle(label = 'Prediction Plot after plsr, C15:0') +
  stat_smooth(aes(x = Test15$Resid15, y = Predict15[,,]), method = 'lm') +
  xlab(label = 'True value') + ylab('Predicted value') +
  theme_gray(base_size = 16)

########################################################################
## pls for C18
########################################################################
Colnames18 <- as.vector(Univariate$WaveLengths[Univariate$Select18 == T])
Data18 <- Data[,c(Colnames18, 'Resid18')]
Seed <- 11
set.seed(Seed)
inTrain <- caret::createDataPartition(y = 1:nrow(Data18), p = 0.75, list = FALSE)
Train18 <- Data18[ inTrain, ]
Test18 <- Data18[ -inTrain, ]

M18_1 <- plsr(Resid18 ~ ., ncomp = 25, data = Train18, validation = "LOO")
summary(M18_1)

M18_2 <- plsr(Resid18 ~ ., ncomp = 12, data = Train18, validation = "LOO")
summary(M18_2)

Predict18 <- predict(M18_2, ncomp = 12, newdata = Test18)
PredictPlot18 <- qplot() + 
  geom_point(aes(x = Test18$Resid18, y = Predict18[,,])) +
  ggtitle(label = 'Prediction Plot after plsr, C18:1') +
  stat_smooth(aes(x = Test18$Resid18, y = Predict18[,,]), method = 'lm') +
  xlab(label = 'True value') + ylab('Predicted value') +
  theme_gray(base_size = 16)

Filename <- paste0(RPlotPath, 'PredictionPlots.pdf')
pdf(file = Filename, onefile = T)
PredictPlot18
PredictPlot15
dev.off()


