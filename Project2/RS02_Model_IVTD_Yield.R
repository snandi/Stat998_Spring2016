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
source('stargazer_lme4.R')

########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project2/'

Filename <- paste0(RScriptPath, 'Data_Combined_2.RData')
load(Filename)

dim(Data)
str(Data)

Data$IVTD_Yield <- Data$Yield_tonperac * Data$IVTD
Data$TX08001 <- Data$Sorghum_SubType == 'PS-TX08001'
########################################################################
## Model 1: Yield ~ 
########################################################################
Model1 <- lm(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX,  
             data = Data)
summary(Model1)
logLik(Model1)
IVTD_Tukey <- TukeyHSD(aov(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + Year + Location, 
                           data = Data))
IVTD_Tukey$Sorghum_SubType

########################################################################
## Model 2: Yield ~ 
########################################################################
Model2 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year), 
               data = Data)
summary(Model2)
anova(Model2)
logLik(Model2)

X2 <- -2*(logLik(Model1) - logLik(Model2, REML = F))
pchisq(q = X2, df = 1, lower.tail = F)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year) + (1|Location), 
               data = Data)
summary(Model3)
anova(Model2, Model3)

qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year) + (1|Location) +
                 (1|Year/Rep) , 
               data = Data)
summary(Model4)
anova(Model3, Model4)

qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))

########################################################################
## Model 5: Yield ~ 
########################################################################
Model5 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year) + (1 + Veg_Type|Location) +
                 (1|Year/Rep) , 
               data = Data)
summary(Model5)
anova(Model4, Model5)

qplot() + geom_point(aes(y = residuals(Model5), x = fitted.values(Model5)))

Model5.1 <- lmer(sqrt(IVTD_Yield) ~ 1 + Sorghum_Type + TX + (1|Year) + (1 + Veg_Type|Location) +
                 (1|Year/Rep) , 
               data = Data)
anova(Model5.1, Model5)

Model5.2 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + TX + (1|Year) + (1 + Veg_Type|Location) +
                 (1|Year/Rep) , 
               data = Data)
anova(Model5.2, Model5)

Model5.3 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + (1|Year) + (1 + Veg_Type|Location) +
                 (1|Year/Rep) , 
               data = Data)
anova(Model5.3, Model5)

Model5.4 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1 + Veg_Type|Location) +
                 (1|Location/Rep) , 
               data = Data)
anova(Model5.4, Model5)

Model5.5 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year) + (1 + Veg_Type|Location), 
               data = Data)
anova(Model5.5, Model5)

Model5.6 <- lmer(sqrt(IVTD_Yield) ~ 1 + Veg_Type + Sorghum_Type + TX + (1|Year) + 
                 (1|Year/Rep) , 
               data = Data)
anova(Model5.6, Model5)

###################################################################################
## IVTD*Yield
###################################################################################
Plot1a_ivtd <- qplot() + geom_boxplot(aes(y = IVTD_Yield, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'IVTD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot1b_ivtd <- qplot() + geom_boxplot(aes(y = IVTD_Yield, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'IVTD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Filename <- paste0(RScriptPath, 'Plot_IVTD_Yield.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Plot1a_ivtd, Plot1b_ivtd, nrow=1), 
       device = 'pdf', width = 8, height = 4, units = 'in')

