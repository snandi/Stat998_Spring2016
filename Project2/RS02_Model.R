rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 2, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)
library(psych)
library(lme4)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project2/'

Filename <- paste0(RScriptPath, 'Data_Combined.RData')
load(Filename)

dim(Data)
str(Data)

########################################################################
## Correlation between different response variables
########################################################################
Responses <- c('Yield_tonperac', 'Ht', 'StemCount', 'CP', 'NDF', 'IVTD', 'NDFD')
Corr.Test <- corr.test(Data[,Responses])
round(Corr.Test$r, 4)
round(Corr.Test$p, 4)

########################################################################
## Model 1: Yield ~ Location + Year + Sorghum_Type + Veg_Type
########################################################################
Model1 <- lm(Yield_tonperac ~ Location + Year + Sorghum_Type + Veg_Type, data = Data)
summary(Model1)
anova(Model1)

########################################################################
## Model 2: Yield ~ Location + Year + Sorghum_Type + Veg_Type
########################################################################
Model2 <- lm(Yield_tonperac ~ Location + Year + Veg_Type + Sorghum_SubType +
               Location*Veg_Type, data = Data)
summary(Model2)
anova(Model2)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_Type*Sorghum_SubType + (1|Location) + (1|Year), 
               data = Data)
summary(Model3)
anova(Model3)

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|LocYear), 
               data = Data)
summary(Model4)
anova(Model4)
qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))
