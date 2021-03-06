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

Data$Sor

Model1 <- lm(log(IVTD) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType, 
               data = Data)
summary(Model1)
logLik(Model1)
IVTD_Tukey <- TukeyHSD(aov(log(IVTD) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + Year, 
             data = Data))
IVTD_Tukey$Sorghum_SubType
xtable(IVTD_Tukey$Sorghum_SubType, digits = c(0, 4, 4, 4, 4))

########################################################################
## Model 2: Yield ~ 
########################################################################
Model2 <- lmer(log(IVTD) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Year), 
               data = Data)
summary(Model2)
anova(Model1, Model2)
logLik(Model2)

X2 <- -2*(logLik(Model1) - logLik(Model2, REML = F))
pchisq(q = X2, df = 1, lower.tail = F)
qplot() + geom_point(aes(y = residuals(Model2), x = fitted.values(Model2)))

Model2.1 <- lmer(log(IVTD) ~ 1 + Sorghum_Type + Sorghum_SubType + (1|Year), 
               data = Data)
anova(Model2.1, Model2)

Model2.2 <- lmer(log(IVTD) ~ 1 + Veg_Type + Sorghum_SubType + (1|Year), 
                 data = Data)
anova(Model2.2, Model2)

Model2.3 <- lmer(log(IVTD) ~ 1 + Veg_Type + Sorghum_Type + (1|Year), 
                 data = Data)
anova(Model2.3, Model2)

###################################################################################
## IVTD
###################################################################################
Plot1a_ivtd <- qplot() + geom_boxplot(aes(y = IVTD, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'IVTD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot1b_ivtd <- qplot() + geom_boxplot(aes(y = IVTD, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'IVTD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Filename <- paste0(RScriptPath, 'Plot_IVTD.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Plot1a_ivtd, Plot1b_ivtd, nrow=1), 
       device = 'pdf', width = 8, height = 4, units = 'in')
