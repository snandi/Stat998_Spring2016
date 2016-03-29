rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 2, Stat 998, Spring 2016
## Modeling Yield as the response
########################################################################
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
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
## Correlation between different response variables
########################################################################
Responses <- c('Yield_tonperac', 'Ht', 'StemCount', 'CP', 'NDF', 'IVTD', 'NDFD')
Corr.Test <- corr.test(Data[,Responses])
round(Corr.Test$r, 4)
round(Corr.Test$p, 4)

stargazer(Corr.Test$r, 4)
xtable(round(Corr.Test$r, 4), digits = c(0, 4, 4, 4, 4, 4, 4, 4))


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
Model3 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_SubType + (1|Year),
               data = Data)
summary(Model3)
anova(Model3)
qplot() + geom_point(aes(y = residuals(Model3), x = fitted.values(Model3)))

Model3b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + (1|Year),
               data = Data)

########################################################################
## Model 4: Yield ~ 
########################################################################
Model4 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
summary(Model4)
anova(Model3, Model4)
qplot() + geom_point(aes(y = residuals(Model4), x = fitted.values(Model4)))

Model4b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + (1|Year) + (1|Location), 
               data = Data)
summary(Model4b)
anova(Model3b, Model4b)
qplot() + geom_point(aes(y = residuals(Model4b), x = fitted.values(Model4b)))

########################################################################
## Model 5: adding random effect of replication
########################################################################
Model5 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_SubType + (1|LocYear/Rep), 
                data = Data)
summary(Model5)

Model5b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + (1|Year/Rep) + (1|Location), 
               data = Data)
summary(Model5b)
AIC(Model5b)
anova(Model4b, Model5b)
qplot() + geom_point(aes(y = residuals(Model5b), x = fitted.values(Model5b)))

########################################################################
## Model 6: adding random slope for veg type & location
########################################################################
Model6 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_SubType + 
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
                data = Data)

Model6b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + 
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
               data = Data)
summary(Model6b)
AIC(Model6b)
anova(Model5b, Model6b)
qplot() + geom_point(aes(y = residuals(Model6b), x = fitted.values(Model6b)))

source('stargazer_lme4.R')
stargazer(Model4b, Model5b, Model6b)
stargazer_lme4(Model4b, Model5b, Model6b)

Resid_Yield_6b <- qplot() + geom_point(aes(y = residuals(Model6b), x = fitted.values(Model6b)), size = 2) +
  ylab(label = 'Residuals') + xlab(label = 'Fitted Values') +
  ggtitle(label = 'Residuals with sqrt(Yield)') +
  theme(strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )  

Resid_Yield_6 <- qplot() + geom_point(aes(y = residuals(Model6), x = fitted.values(Model6)), size = 2) +
  ggtitle(label = 'Residuals with Yield') +
  ylab(label = 'Residuals') + xlab(label = 'Fitted Values') +
  theme(strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )  

Filename <- paste0(RScriptPath, 'Resid_Yield.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Resid_Yield_6, Resid_Yield_6b, nrow=1), 
       device = 'pdf', width = 7, height = 4, units = 'in')

########################################################################
## Model 7: adding stem count and height 
########################################################################
Model7 <- lmer(Yield_tonperac ~ 1 + Veg_Type + Sorghum_SubType + Ht + StemCount +
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
                data = Data)

Model7b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + Ht + StemCount +
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
               data = Data)
summary(Model7b)
AIC(Model7b)
Resid_Yield_7b <- qplot() + geom_point(aes(y = residuals(Model7b), x = fitted.values(Model7b)), size = 2) +
  ylab(label = 'Residuals') + xlab(label = 'Fitted Values') +
  ggtitle(label = 'Residuals with sqrt(Yield)') +
  theme(strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )  

Resid_Yield_7 <- qplot() + geom_point(aes(y = residuals(Model7), x = fitted.values(Model7)), size = 2) +
  ggtitle(label = 'Residuals with Yield') +
  ylab(label = 'Residuals') + xlab(label = 'Fitted Values') +
  theme(strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )  

Model8b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_Type + Ht + StemCount +
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
                data = Data)

stargazer_lme4(Model4b, Model5b, Model6b, Model8b)

Plot_Ht <- qplot() + geom_boxplot(aes(y = Ht, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  ylab(label = 'Height') + xlab(label = 'Sorghum Type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot_Stem <- qplot() + geom_boxplot(aes(y = StemCount, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  ylab(label = 'Stem Count') + xlab(label = 'Sorghum Type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Filename <- paste0(RScriptPath, 'Plot_Ht_Stem.pdf') 
ggsave(filename = Filename, plot = grid.arrange(Plot_Ht, Plot_Stem, nrow=1), 
       device = 'pdf', width = 7, height = 4, units = 'in')

################## FINAL MODEL ##################
Model6b <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + 
                  (1 + Veg_Type | Location ) + (1|Year/Rep) , 
                data = Data)
summary(Model6b)

Model6b.1 <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + 
                  (1 + Veg_Type | Location ) + (1|Location/Rep) , 
                data = Data)
anova(Model6b.1, Model6b)

Model6b.2 <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + 
                  (1 + Veg_Type | Location ) + (1|Year) , 
                data = Data)
anova(Model6b.2, Model6b)

Model6b.3 <- lmer(sqrt(Yield_tonperac) ~ 1 + Veg_Type + Sorghum_SubType + 
                     + (1|Year/Rep) , 
                  data = Data)
anova(Model6b.3, Model6b)
