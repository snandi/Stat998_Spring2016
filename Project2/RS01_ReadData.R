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

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project2/'

Filename <- paste0(RScriptPath, 'Data_Combined_2.txt')
Data <- read.table(file = Filename, header = T, sep = '\t')

Data$Year <- as.factor(Data$Year)
Data$LocYear <- with(Data, interaction(Location,  Year))

dim(Data)
str(Data)

Filename <- paste0(RScriptPath, 'Data_Combined_2.RData')
save(Data, file = Filename)

qplot() + geom_point(aes(x = Ht, y = Yield_tonperac), data = Data)

Plot0 <- qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = Sorghum_Type), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'Total yield (ton per acre)') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot0.pdf') 
ggsave(filename = Filename, plot = Plot0, device = 'pdf', width = 6, height = 4, units = 'in')

Plot1a <- qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'Total yield (ton per acre)') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot1a.pdf') 
ggsave(filename = Filename, plot = Plot1a, device = 'pdf', width = 6, height = 4, units = 'in')

Plot1b <- qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'Total yield (ton per acre)') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot1b.pdf') 
ggsave(filename = Filename, plot = Plot1b, device = 'pdf', width = 6, height = 4, units = 'in')

Plot1c <- qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Location) + ylab(label = 'Total yield (ton per acre)') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
Filename <- paste0(RScriptPath, 'Plot1c.pdf') 
ggsave(filename = Filename, plot = Plot1c, device = 'pdf', width = 6, height = 4, units = 'in')

Plot2 <- qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_SubType, fill = Sorghum_SubType), data = Data) +
  facet_wrap( Location ~ Veg_Type) + #coord_flip() +
  ylab(label = 'Total yield (ton per acre)') + xlab(label = 'Sorghum sub-type') +
#  theme(legend.position = 'top')
        theme(legend.position = '', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
        )
Filename <- paste0(RScriptPath, 'Plot2.pdf') 
ggsave(filename = Filename, plot = Plot2, device = 'pdf', width = 6, height = 6, units = 'in')

qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = Location), data = Data) +
  facet_wrap(~ Veg_Type)

qplot() + geom_boxplot(aes(y = Yield_tonperac, x = Sorghum_Type, fill = LocYear), data = Data) +
  facet_wrap(~ Veg_Type) 

###################################################################################
## NDFD
###################################################################################
Plot1a_ndfd <- qplot() + geom_boxplot(aes(y = NDFD, x = Sorghum_Type, fill = Year), data = Data) +
  facet_wrap(~ Veg_Type) + ylab(label = 'NDFD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

Plot1b_ndfd <- qplot() + geom_boxplot(aes(y = NDFD, x = Sorghum_Type, fill = Sorghum_SubType), data = Data) +
  facet_wrap(~ Year) + ylab(label = 'NDFD') + xlab(label = 'Sorghum type') +
  theme(legend.position = 'top', 
        strip.text.x = element_text(size = 9), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
