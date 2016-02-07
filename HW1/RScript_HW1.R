rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is Prob 1, HW 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)

########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/HW1/'

Filename <- paste0(RScriptPath, 'Data1.csv')
Data1 <- read.csv(Filename)

str(Data1)

########################################################################
## Summary Plots
########################################################################
Plot1a <- qplot() + geom_boxplot(aes(x = Crop, y = Gr_Rate, fill = Crop), data = Data1) +
  ylab(label = 'Growth Rate') +
  ggtitle(label = 'Growth rate by previous year\'s crop')

Plot1b <- qplot() + geom_boxplot(aes(x = factor(Fert), y = Gr_Rate, fill = factor(Fert)), data = Data1) +
  ylab(label = 'Growth Rate') + xlab(label = 'Fertilizer') +
  ggtitle(label = 'Growth rate by Fertilizer')

Plot1c <- qplot() + geom_boxplot(aes(x = factor(Irr), y = Gr_Rate, fill = factor(Irr)), data = Data1) +
  ylab(label = 'Growth Rate') + xlab(label = 'Irrigation') + 
  ggtitle(label = 'Growth rate by Irrigation')

Plot2 <- qplot() + geom_boxplot(aes(x = Crop, y = Gr_Rate, fill = Crop), data = Data1) +
  facet_grid(. ~ Fert) +
  ylab(label = 'Growth Rate') +
  ggtitle(label = 'Growth rate by fertilizer and previous year\'s crop')


Plot3 <- qplot() + geom_boxplot(aes(x = Crop, y = Gr_Rate, fill = Crop), data = Data1) +
  facet_grid(. ~ Irr) +
  ylab(label = 'Growth Rate') +
  ggtitle(label = 'Growth rate by irrigation and previous year\'s crop')

Data1$Crop <- factor(Data1$Crop, levels = c('Soy beans', 'Corn', 'Oats'))
########################################################################
## Anova
########################################################################
Model1 <- lm(Gr_Rate ~ factor(Fert) + factor(Irr) + Crop + Crop*factor(Fert), data = Data1)
anova(Model1)
summary(Model1)
xtable(anova(Model1))
xtable(summary(Model1))
Plot4 <- qplot() + geom_point(aes(y = residuals(Model1), x = 1:36), size = 3) + 
  ggtitle('Residuals of the model') +
  xlab(label = 'Observations') + ylab(label = 'Residuals') + 
  theme(legend.position = 'none', 
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14) 
  )

Filename.plot <- paste0(RScriptPath, 'Plot1.pdf')
pdf(file = Filename.plot, onefile = T)
Plot1a
Plot1b
Plot1c
Plot2
Plot3
Plot4
dev.off()

Model2 <- lm(Gr_Rate ~ Fert + Irr + Crop + Crop*Fert, data = Data1)
anova(Model2)
summary(Model2)

Model3 <- lm(Gr_Rate ~ Fert + Irr + Crop + Crop*Fert + Irr*Fert, data = Data1)
anova(Model3)
summary(Model3)
