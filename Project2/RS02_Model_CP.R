rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 2, Stat 998, Spring 2016
## Modeling CP as the response
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
Model1 <- lm(CP ~ Location + Year + Sorghum_Type + Veg_Type, data = Data)
summary(Model1)
anova(Model1)

########################################################################
## Model 2: Yield ~ Location + Year + Sorghum_Type + Veg_Type
########################################################################
Model2 <- lm(CP ~ Location + Year + Veg_Type + Sorghum_SubType +
               Location*Veg_Type, data = Data)
summary(Model2)
anova(Model2)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(CP ~ 1 + Veg_Type + Sorghum_Type*Sorghum_SubType + (1|Location) + (1|Year), 
               data = Data)
summary(Model3)
anova(Model3)
qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(sqrt(CP) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|LocYear), 
               data = Data)
summary(Model4)
anova(Model4)
qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))

########################################################################
## Model 5: adding random effect of replication
########################################################################
Model5 <- lmer(sqrt(CP) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Location) +
                 (1|Year/Rep), 
               data = Data)
summary(Model5)
AIC(Model5)
anova(Model5)

########################################################################
## Model 5: adding random slope for year
########################################################################
Model6 <- lmer(sqrt(CP) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Location) + 
                 (1|Year/Rep), 
               data = Data)
summary(Model6)
AIC(Model6)

########################################################################
## Model 6: 2014
########################################################################
Model6_2014 <- lmer(CP ~ 1 + Veg_Type + Sorghum_SubType + (1|Location) + 
                      (1|Location:Rep), 
                    data = subset(Data, Year == '2014')
)
summary(Model6_2014)
qplot() + geom_point(aes(y = residuals(Model6_2014), x = fitted.values(Model6_2014)))

Model6_2015 <- lmer(CP ~ 1 + Veg_Type + Sorghum_SubType + (1|Location) + 
                      (1|Location:Rep), 
                    data = subset(Data, Year == '2015')
)
summary(Model6_2015)
qplot() + geom_point(aes(y = residuals(Model6_2015), x = fitted.values(Model6_2015)))

########################################################################
## Model 7: adding 
########################################################################
Model7 <- lmer(sqrt(CP) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + Ht + StemCount + 
                 (1|Location) + (1|Year/Rep), 
               data = Data)
summary(Model7)
AIC(Model7)

summary(lm(sqrt(CP) ~ 1 + Ht + StemCount, data = Data))
