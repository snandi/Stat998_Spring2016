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

Model1 <- lm(log(StemCount) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType, 
               data = Data)
summary(Model1)

TukeyHSD(aov(StemCount ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType, 
             data = Data))

########################################################################
## Model 2: Yield ~ 
########################################################################
Model2 <- lmer(log(StemCount) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Year), 
               data = Data)
summary(Model2)
X2 <- -2*(logLik(Model1) - logLik(Model2, REML = F))
pchisq(q = X2, df = 1, lower.tail = F)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(log(StemCount) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
summary(Model3)
anova(Model2, Model3)
qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

Model3.1 <- lmer(log(StemCount) ~ 1 + Sorghum_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
anova(Model3.1, Model3)

Model3.2 <- lmer(log(StemCount) ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
                 data = Data)
anova(Model3.2, Model3)

Model3.3 <- lmer(log(StemCount) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Location), 
                 data = Data)
anova(Model3.3, Model3)

Model3.4 <- lmer(log(StemCount) ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType + (1|Year), 
                 data = Data)
anova(Model3.4, Model3)
###################################################################################
## StemCount
###################################################################################
Plot1a_StemCount <- qplot() + geom_boxplot(aes(y = StemCount, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'StemCount') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot1b_StemCount <- qplot() + geom_boxplot(aes(y = StemCount, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'StemCount') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot_StemCount.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Plot1a_StemCount, Plot1b_StemCount, nrow=1), 
       device = 'pdf', width = 8, height = 4, units = 'in')
