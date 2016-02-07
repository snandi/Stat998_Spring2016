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
library(lme4)
library(nlme)
library(lmerTest)

########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/HW1/'

Filename <- paste0(RScriptPath, 'Data2.csv')
Data2 <- read.csv(Filename, header = F)
colnames(Data2) <- c('Pot', 'Additive', 'Conc')

Data2 <- within(data = Data2,{
  Pot <- factor(Pot)
  Additive <- factor(Additive)
})

str(Data2)

Plot1 <- qplot() + geom_point(aes(x = Additive, y = Conc, shape = Pot), size = 5, data = Data2) +
  scale_shape_manual(values=1:nlevels(Data2$Pot)) + 
  theme(legend.position = 'none', 
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14) 
  ) +
  ggtitle('Concentration vs Additives') +
  xlab(label = 'Additive') + ylab(label = 'Concentration')

Model1 <- lmer(Conc ~ Additive + (1 | Pot) , data = Data2)
summary(Model1)

Model2 <- lmer(log(Conc) ~ Additive + (1 | Pot) , data = Data2)
summary(Model2)
PostHoc <- step(Model2, reduce.random=FALSE, reduce.fixed=FALSE, test.effs = 'Additive')
lmerTest::step(Model2, reduce.random = FALSE)

Plot2 <- qplot() + geom_point(aes(y = residuals(Model2), x = 1:24), size = 3) + 
  ggtitle('Residuals of the model') +
  xlab(label = 'Observations') + ylab(label = 'Residuals') + 
  theme(legend.position = 'none', 
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14) 
  )

Filename.plot <- paste0(RScriptPath, 'Plot2.pdf')
pdf(file = Filename.plot, onefile = T)
Plot1
Plot2
dev.off()
