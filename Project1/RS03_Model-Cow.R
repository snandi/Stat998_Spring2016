rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for Project 1, Stat 998, Spring 2016
########################################################################
library(xtable)
library(survival)
library(GGally)
library(ggfortify)
library(ggplot2)
library(reshape2)
library(psych)

source('~/RScripts/fn_Library_SN.R')
########################################################################
## Load header files and source functions
########################################################################
RScriptPath <- '~/Courses/Stat998_Spring2016/Project1/'
RPlotPath <- '~/Courses/Stat998_Spring2016/Project1/Plots/'

Filename <- paste0(RScriptPath, 'Data1.RData')
load(Filename)
colnames(Data1)

########################################################################
## Fit C15 with cow characteristics
########################################################################
Cols_Cow <- c("Wt",
              "BC_Score",
              "Parity",
              "Days",
              "Intake",
              "Milk_Yield"
)
Cols_Milk <- c(
  "Fat",
  "Protein",
  "Lactose" ,
  "Nitrogen"
)

Data1$Parity2 <- (Data1$Parity)^2
Data1$UniqueCow <- (Data1$Parity == 4)

M15_1 <- lm(C15_0 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield, data = Data1)
summary(M15_1)

M15_2 <- lm(C15_0 ~ Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_2)

M15_3 <- lm(C15_0 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield + 
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_3)

M15_4 <- lm(C15_0 ~ BC_Score + Parity + Parity2 + Days + Milk_Yield + 
              Fat + Protein + Lactose, data = Data1)
summary(M15_4)
#plot(M15_4)

M15_5 <- lm(C15_0 ~ Wt + BC_Score + Parity + Parity2 + Days + Intake + Milk_Yield + UniqueCow + UniqueCow31 +
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_5)
#plot(M15_5)
ggplot2::autoplot(object = M15_5, which = 1:6, ncol = 3, label.size = 3)

anova(M15_4, M15_5)

#attr (Data1$Parity, "contrasts") <- contr.poly (2) 

step(object = M15_5, direction = 'backward', k = log(nrow(Data1)))

Data1$Cow31 <- rownames(Data1) == "31"
Data1$Cow56 <- rownames(Data1) == "56"
Data1$Cow58 <- rownames(Data1) == "58"
Final15 <- lm(C15_0 ~ Parity + Days + Milk_Yield + UniqueCow +
                         Protein + Lactose + Cow31 + Cow56 + Cow58, data = Data1)
summary(Final15)
xtable(Final15)
ggplot2::autoplot(object = Final15, which = 1:6, ncol = 3, label.size = 3)

Filename <- paste0(RPlotPath, 'ModelC15.pdf')
pdf(file = Filename, onefile = T)
ggplot2::autoplot(object = Final15, which = 1, ncol = 3, label.size = 3)
ggplot2::autoplot(object = Final15, which = 2, ncol = 3, label.size = 3)
ggplot2::autoplot(object = Final15, which = 4, ncol = 3, label.size = 3)
dev.off()

Resid15 <- residuals(Final15)

########################################################################
## Fit C18 with cow characteristics
########################################################################
M18_1 <- lm(C18_1 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield, data = Data1)
summary(M18_1)

M18_2 <- lm(C18_1 ~ Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M15_2)

M18_3 <- lm(C18_1 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield + 
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M18_3)
ggplot2::autoplot(object = M18_3, which = 1:6, ncol = 3, label.size = 3)

M18_4 <- lm(C18_1 ~ Wt + BC_Score + Parity + Days + Intake + Milk_Yield + UniqueCow +
              Fat + Protein + Lactose + Nitrogen, data = Data1)
summary(M18_4)
ggplot2::autoplot(object = M18_4, which = 1:6, ncol = 3, label.size = 3)

step(object = M18_4, direction = 'backward', k = log(nrow(Data1)))

M18_5 <- lm(C18_1 ~ Intake + Milk_Yield + Nitrogen, data = Data1)
summary(M18_5)
ggplot2::autoplot(object = M18_5, which = 1:6, ncol = 3, label.size = 3)
ggplot2::autoplot(object = M18_5, which = 4, main = 'Model before adding indicator', ylim = c(0, 0.4))
Data1$UniqueCow54 <- rownames(Data1) == "54"

Final18 <- lm(C18_1 ~ Parity + Days + Intake + Milk_Yield + Nitrogen + UniqueCow54 + UniqueCow, data = Data1)
summary(Final18)

anova(M18_5, Final18)

xtable(summary(Final18))
Filename <- paste0(RPlotPath, 'ModelC18.pdf')
pdf(file = Filename, onefile = T)
ggplot2::autoplot(object = Final18, which = 1, ncol = 3, label.size = 3)
ggplot2::autoplot(object = Final18, which = 2, ncol = 3, label.size = 3)
ggplot2::autoplot(object = Final18, which = 4, ncol = 3, label.size = 3)
dev.off()

Resid18 <- residuals(Final18)

Data1$Resid18 <- Resid18
Data1$Resid15 <- Resid15

########################################################################
## Save residuals to large dataset
########################################################################
Filename <- paste0(RScriptPath, 'Data.RData')
load(Filename)
Data$Resid18 <- Resid18
Data$Resid15 <- Resid15
save(Data, file = Filename)


