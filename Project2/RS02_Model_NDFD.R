rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 2, Stat 998, Spring 2016
## Modeling Yield as the response
########################################################################
library(ggplot2)
library(lme4)
library(psych)
library(reshape2)
library(stargazer)
library(survival)
library(xtable)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project2/'

Filename <- paste0(RScriptPath, 'Data_Combined_2.RData')
load(Filename)

dim(Data)
str(Data)

########################################################################
## Model 2: Yield ~ 
########################################################################
Model2 <- lmer(NDFD ~ 1 + Veg_Type + Sorghum_SubType + (1|Year), 
               data = Data)
summary(Model2)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(NDFD ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
summary(Model3)
anova(Model2, Model3)
qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(NDFD ~ 1 + Veg_Type + Sorghum_SubType + (1|Year/Rep) + (1|Location), 
               data = Data)
summary(Model4)
anova(Model3, Model4)
qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))

########################################################################
## Model 5: Yield ~ 
########################################################################
Model5 <- lmer(NDFD ~ 1 + Veg_Type + Sorghum_SubType + (1|Year/Rep) + (1 + Veg_Type|Location), 
               data = Data)
summary(Model5)
anova(Model4, Model5)
qplot() + geom_point(aes(y = residuals(Model5), x = fitted.values(Model5)))


