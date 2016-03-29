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

Model1 <- lm(Ht ~ 1 + Veg_Type + Sorghum_SubType, 
               data = Data)
summary(Model1)

TukeyHSD(aov(Ht ~ 1 + Veg_Type + Sorghum_Type + Sorghum_SubType, 
             data = Data))

########################################################################
## Model 2: Yield ~ 
########################################################################
Model2 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Year), 
               data = Data)
summary(Model2)
X2 <- -2*(logLik(Model1) - logLik(Model2, REML = F))
pchisq(q = X2, df = 1, lower.tail = F)

########################################################################
## Model 3: Yield ~ 
########################################################################
Model3 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
summary(Model3)
anova(Model2, Model3)
qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Year/Rep) + (1|Location), 
               data = Data)
summary(Model4)
anova(Model3, Model4)
qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))

Model4.1 <- lmer(Ht ~ 1 + Sorghum_SubType + (1|Year/Rep) + (1|Location), 
               data = Data)
anova(Model4.1, Model4)

Model4.2 <- lmer(Ht ~ 1 + Veg_Type + (1|Year/Rep) + (1|Location), 
                 data = Data)
anova(Model4.2, Model4)

Model4.3 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Location/Rep), 
                 data = Data)
anova(Model4.3, Model4)

Model4.4 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
                 data = Data)
anova(Model4.4, Model4)

Model4.5 <- lmer(Ht ~ 1 + Veg_Type + Sorghum_SubType + (1|Year/Rep), 
                 data = Data)
anova(Model4.5, Model4)

###################################################################################
## Ht
###################################################################################
Plot1a_Ht <- qplot() + geom_boxplot(aes(y = Ht, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'Ht') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot1b_Ht <- qplot() + geom_boxplot(aes(y = Ht, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'Ht') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot_Ht.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Plot1a_Ht, Plot1b_Ht, nrow=1), 
       device = 'pdf', width = 8, height = 4, units = 'in')
