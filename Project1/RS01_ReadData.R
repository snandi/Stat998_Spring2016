rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)
library(psych)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'

Filename <- paste0(RScriptPath, 'data_set1_sn.csv')
Data <- read.csv(Filename, header = T)

dim(Data)
## 59 observations 1083 covariates
head(names(Data), 30)

########################################################################
## Some basic diagnostics
########################################################################
length(unique(Data$SampleID))

#matplot(t(Data[,22:300]), type = 'l')
#matplot(t(Data[,301:600]), type = 'l')
#matplot(t(Data[,601:1081]), type = 'l')
            
colSDs <- colSD(Data = Data[,22:1081])

Hist_colSDs <- qplot() + geom_histogram(aes(x = colSDs), binwidth = 0.01) +
  ggtitle(label = 'Histogram of Wavelength SDs') +
  xlab(label = 'Standard deviations') + ylab(label = 'Frequency')
          
length(colSDs[colSDs > 0.2])

########################################################################
## Modeling cow variables with fat pct
########################################################################
Data1 <- Data[,1:21]
str(Data1)

Corr.Test <- corr.test(Data1[,3:21])
#View(round(Corr.Test$r, 2))
#View(round(Corr.Test$p, 2))

########################################################################
## Impute missing data of Dry_Matter_Intake
########################################################################
Model_DMI <- lm(Dry_Matter_Intake ~ BodyWeight + Body_Condition_Score + Parity + Milk_Yield,
                data = Data1)
summary(Model_DMI)
xtable(summary(Model_DMI))
#plot(residuals(Model_DMI))

Data[4,'Dry_Matter_Intake'] <- predict(Model_DMI, newdata = Data1[4,c('BodyWeight', 'Body_Condition_Score', 'Parity', 'Milk_Yield')])

attributes(Data)$old_column_names <- colnames(Data)[3:12]
colnames(Data)[3:12] <- c('Wt', 'BC_Score', 'Parity', 'Days', 'Intake', 'Milk_Yield', 'Fat', 'Protein', 'Lactose', 
                     'Nitrogen')

Filename1 <- paste0(RScriptPath, 'Data1.RData')
Data1 <- Data[,1:21]
save(Data1, file = Filename1)

Filename2 <- paste0(RScriptPath, 'Data.RData')
save(Data, file = Filename2)

