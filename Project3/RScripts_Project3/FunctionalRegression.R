rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script is for trying function on function regression
########################################################################
library(fda)
library(gridExtra)
library(xtable)
library(survival)
library(ggplot2)
library(reshape2)
library(RFunctionsSN)
library(psych)

data(gait)
str(gait)

demo('gait', package='fda')

gaittime <- as.numeric(dimnames(gait)[[1]])*20

gaitrange <- c(0,20)

#  display ranges of gait for each variable
apply(X = gait, MARGIN = 3, FUN = range)

# -----------  set up the harmonic acceleration operator  ----------
harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)

