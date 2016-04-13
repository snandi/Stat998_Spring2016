demo(gait)
---- ~~~~
  
  Type  <Return>	 to start : 
  
  > #  --------------------------------------------------------------------
> #                        Gait data
  > #  --------------------------------------------------------------------
> 
  > #  --------------------------------------------------------------------
> #
  > #                          Overview of the analyses
  > #
  > #  The gait data were chosen for these sample analyses because they are
  > #  bivariate:  consisting of both hip and knee angles observed over a
  > #  gait cycle for 39 children.  The bivariate nature of the data implies
  > #  certain displays and analyses that are not usually considered, and
  > #  especially the use of canonical correlation analysis.
  > #
  > #  As with the daily weather data, the harmonic acceleration roughness
  > #  penalty is used throughout since the data are periodic with a strong
  > #  sinusoidal component of variation.
  > #
  > #  After setting up the data, smoothing the data using GCV (generalized
  > #  cross-validation) to select a smoothing parameter, and displaying
  > #  various descriptive results, the data are subjected to a principal
  > #  components analysis, followed by a canonical correlation analysis of
  > #  thejoint variation of hip and knee angle, and finally a registration
  > #  of the curves.  The registration is included here especially because
  > #  the registering of periodic data requires the estimation of a phase
  > #  shift constant for each curve in addition to possible nonlinear
  > #  transformations of time.
  > #
  > #  --------------------------------------------------------------------
> 
  > #  Last modified 10 November 2010 by Jim Ramsay
  > 
  > #  attach the FDA functions
  > 
  > library(fda)

> #  Set up the argument values: equally spaced over circle of
  > #  circumference 20.  Earlier  analyses of the gait data used time
  > #  values over [0,1], but led to singularity problems in the use of
  > #  function fRegress.  In general, it is better use a time interval
  > #  that assigns roughly one time unit to each inter-knot interval.
  > 
  > (gaittime <- as.numeric(dimnames(gait)[[1]])*20)
[1]  0.5  1.5  2.5  3.5  4.5  5.5  6.5  7.5  8.5  9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5

> gaitrange <- c(0,20)

> #  display ranges of gait for each variable
  > 
  > apply(gait, 3, range)
Hip Angle Knee Angle
[1,]       -12          0
[2,]        64         82

> # -----------  set up the harmonic acceleration operator  ----------
> 
  > harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)


> #  Set up basis for representing gait data.  The basis is saturated
  > #  since there are 20 data points per curve, and this set up defines
  > #  21 basis functions.  Recall that a fourier basis has an odd number
  > #  of basis functions.
  > 
  > gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)

> #  -------------------------------------------------------------------
> #                 Choose level of smoothing using
  > #          the generalized cross-validation criterion
  > #  -------------------------------------------------------------------
> 
  > #  set up range of smoothing parameters in log_10 units
  > 
  > gaitLoglam <- seq(-4,0,0.25)

> nglam   <- length(gaitLoglam)

> gaitSmoothStats <- array(NA, dim=c(nglam, 3),
                           +       dimnames=list(gaitLoglam, c("log10.lambda", "df", "gcv") ) )

> gaitSmoothStats[, 1] <- gaitLoglam

> #  loop through smoothing parameters
  > 
  > for (ilam in 1:nglam) {
    +   gaitSmooth <- smooth.basisPar(gaittime, gait, gaitbasis,
                                      +                    Lfdobj=harmaccelLfd, lambda=10^gaitLoglam[ilam])
    +   gaitSmoothStats[ilam, "df"]  <- gaitSmooth$df
    +   gaitSmoothStats[ilam, "gcv"] <- sum(gaitSmooth$gcv)
    +   # note: gcv is a matrix in this case
      + }

> #  display and plot GCV criterion and degrees of freedom
  > 
  > gaitSmoothStats
log10.lambda        df      gcv
-4           -4.00 19.778892 399.1533
-3.75        -3.75 19.617447 395.6074
-3.5         -3.50 19.350561 389.7444
-3.25        -3.25 18.929624 380.5158
-3           -3.00 18.309419 367.0378
-2.75        -2.75 17.472700 349.3024
-2.5         -2.50 16.449787 328.7959
-2.25        -2.25 15.310385 308.2647
-2           -2.00 14.132132 290.6809
-1.75        -1.75 12.974831 278.3934
-1.5         -1.50 11.874319 272.9774
-1.25        -1.25 10.847984 275.5117
-1           -1.00  9.901944 287.1021
-0.75        -0.75  9.036088 310.0641
-0.5         -0.50  8.247388 349.8805
-0.25        -0.25  7.531467 417.2408
0             0.00  6.881984 528.4306

> plot(gaitSmoothStats[, 1], gaitSmoothStats[, 3])

  
  > #  set up plotting arrangements for one and two panel displays
  > #  allowing for larger fonts
  > 
  > op <- par(mfrow=c(2,1))

> plot(gaitLoglam, gaitSmoothStats[, "gcv"], type="b",
       +      xlab="Log_10 lambda", ylab="GCV Criterion",
       +      main="Gait Smoothing", log="y")

  
  > plot(gaitLoglam, gaitSmoothStats[, "df"], type="b",
         +      xlab="Log_10 lambda", ylab="Degrees of freedom",
         +      main="Gait Smoothing")

> par(op)

> # With gaittime <- (1:20)/21,
  > #    GCV is minimized with lambda = 10^(-2).
  > 
  > gaitfd <- smooth.basisPar(gaittime, gait,
                              +        gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd

> names(gaitfd$fdnames) <- c("Normalized time", "Child", "Angle")

> gaitfd$fdnames[[3]] <- c("Hip", "Knee")

> str(gaitfd)
List of 3
$ coefs  : num [1:21, 1:39, 1:2] 110.46 -17.29 53.8 -6.94 -11.76 ...
..- attr(*, "dimnames")=List of 3
.. ..$ : NULL
.. ..$ : chr [1:39] "boy1" "boy2" "boy3" "boy4" ...
.. ..$ : chr [1:2] "Hip Angle" "Knee Angle"
$ basis  :List of 10
..$ call       : language basisfd(type = type, rangeval = rangeval, nbasis = nbasis, params = params, dropind = dropind, quadvals = quadvals,      values = values, basisvalues = basisvalues)
..$ type       : chr "fourier"
..$ rangeval   : num [1:2] 0 20
..$ nbasis     : num 21
..$ params     : num 20
..$ dropind    : num(0) 
..$ quadvals   : NULL
..$ values     : list()
..$ basisvalues: list()
..$ names      : chr [1:21] "const" "sin1" "cos1" "sin2" ...
..- attr(*, "class")= chr "basisfd"
$ fdnames:List of 3
..$ Normalized time: chr [1:20] "0.025" "0.075" "0.125" "0.175" ...
..$ Child          : chr [1:39] "boy1" "boy2" "boy3" "boy4" ...
..$ Angle          : chr [1:2] "Hip" "Knee"
- attr(*, "class")= chr "fd"

> #  --------  plot curves and their first derivatives  ----------------
> 
  > #par(mfrow=c(1,2), mar=c(3,4,2,1), pty="s")
  > op <- par(mfrow=c(2,1))

> plot(gaitfd, cex=1.2)

  [1] "done"

> par(op)

> #  plot each pair of curves interactively
  > 
  > plotfit.fd(gait, gaittime, gaitfd, cex=1.2, ask=FALSE)

  
  > #  plot the residuals, sorting cases by residual sum of squares
  > #  this produces 39 plots for each of knee and hip angle
  > 
  > plotfit.fd(gait, gaittime, gaitfd, residual=TRUE, sort=TRUE, cex=1.2)
Multiple plots:  Click in the plot to advance to the next plot

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > #  plot first derivative of all curves
  > 
  > op <- par(mfrow=c(2,1))

> plot(gaitfd, Lfdobj=1)

  [1] "done"

> par(op)

> #  -----------------------------------------------------------------
> #            Display the mean, variance and covariance functions
  > #  -----------------------------------------------------------------
> 
  > #  ------------  compute the mean functions  --------------------
> 
  > gaitmeanfd <- mean.fd(gaitfd)

> #  plot these functions and their first two derivatives
  > 
  > op <- par(mfcol=2:3)

> plot(gaitmeanfd)

  [1] "done"

> plot(gaitmeanfd, Lfdobj=1)
[1] "done"

> plot(gaitmeanfd, Lfdobj=2)
[1] "done"

> par(op)

> #  --------------  Compute the variance functions  -------------
> 
  > gaitvarbifd <- var.fd(gaitfd)

> str(gaitvarbifd)
List of 4
$ coefs    : num [1:21, 1:21, 1, 1:3] 624.93 20 111.69 -15.65 -3.41 ...
$ sbasis   :List of 10
..$ call       : language basisfd(type = type, rangeval = rangeval, nbasis = nbasis, params = params, dropind = dropind, quadvals = quadvals,      values = values, basisvalues = basisvalues)
..$ type       : chr "fourier"
..$ rangeval   : num [1:2] 0 20
..$ nbasis     : num 21
..$ params     : num 20
..$ dropind    : num(0) 
..$ quadvals   : NULL
..$ values     : list()
..$ basisvalues: list()
..$ names      : chr [1:21] "const" "sin1" "cos1" "sin2" ...
..- attr(*, "class")= chr "basisfd"
$ tbasis   :List of 10
..$ call       : language basisfd(type = type, rangeval = rangeval, nbasis = nbasis, params = params, dropind = dropind, quadvals = quadvals,      values = values, basisvalues = basisvalues)
..$ type       : chr "fourier"
..$ rangeval   : num [1:2] 0 20
..$ nbasis     : num 21
..$ params     : num 20
..$ dropind    : num(0) 
..$ quadvals   : NULL
..$ values     : list()
..$ basisvalues: list()
..$ names      : chr [1:21] "const" "sin1" "cos1" "sin2" ...
..- attr(*, "class")= chr "basisfd"
$ bifdnames:List of 3
..$ Normalized time: chr [1:20] "0.025" "0.075" "0.125" "0.175" ...
..$ Child          : chr [1:39] "boy1" "boy2" "boy3" "boy4" ...
..$ Angle          :List of 3
.. ..$ : chr "Hip vs Hip"
.. ..$ : chr "Knee vs Hip"
.. ..$ : chr "Knee vs Knee"
- attr(*, "class")= chr "bifd"

> gaitvararray <- eval.bifd(gaittime, gaittime, gaitvarbifd)

> #  plot variance and covariance functions as contours
  > 
  > filled.contour(gaittime, gaittime, gaitvararray[,,1,1], cex=1.2)

  
  > title("Knee - Knee")

> filled.contour(gaittime, gaittime, gaitvararray[,,1,2], cex=1.2)

  
  > title("Knee - Hip")

> filled.contour(gaittime, gaittime, gaitvararray[,,1,3], cex=1.2)

  
  > title("Hip - Hip")

> #  plot variance and covariance functions as surfaces
  > 
  > persp(gaittime, gaittime, gaitvararray[,,1,1], cex=1.2)

  
  > title("Knee - Knee")

> persp(gaittime, gaittime, gaitvararray[,,1,2], cex=1.2)

  
  > title("Knee - Hip")

> persp(gaittime, gaittime, gaitvararray[,,1,3], cex=1.2)

  
  > title("Hip - Hip")

> #  plot correlation functions as contours
  > 
  > gaitCorArray <- cor.fd(gaittime, gaitfd)

> quantile(gaitCorArray)
0%         25%         50%         75%        100% 
-0.42685407  0.09446618  0.41698173  0.65847538  1.00000000 

> contour(gaittime, gaittime, gaitCorArray[,,1,1], cex=1.2)

  
  > title("Knee - Knee")

> contour(gaittime, gaittime, gaitCorArray[,,1,2], cex=1.2)

  
  > title("Knee - Hip")

> contour(gaittime, gaittime, gaitCorArray[,,1,3], cex=1.2)

  
  > title("Hip - Hip")

> #  --------------------------------------------------------------
> #            Principal components analysis
  > #  --------------------------------------------------------------
> 
  > #  do the PCA with varimax rotation
  > 
  > # Smooth with lambda as determined above
  > 
  > gaitfdPar  <- fdPar(gaitbasis, harmaccelLfd, lambda=1e-2)

> gaitpca.fd <- pca.fd(gaitfd, nharm=4, gaitfdPar)

> gaitpca.fd <- varmx.pca.fd(gaitpca.fd)

> #  plot harmonics using cycle plots
  > 
  > op <- par(mfrow=c(2,2))

> plot.pca.fd(gaitpca.fd, cycle=TRUE)

  
  > par(op)

> #  compute proportions of variance associated with each angle
  > 
  > #  compute the harmonic scores associated with each angle
  > 
  > gaitscores = gaitpca.fd$scores

> #  compute the values of the harmonics at time values for each angle
  > 
  > gaitharmmat = eval.fd(gaittime, gaitpca.fd$harmonics)

> hipharmmat  = gaitharmmat[,,1]

> kneeharmmat = gaitharmmat[,,2]

> #  we need the values of the two mean functions also
  > 
  > gaitmeanvec = eval.fd(gaittime, gaitmeanfd)

> hipmeanvec  = gaitmeanvec[,,1]

> kneemeanvec = gaitmeanvec[,,2]

> #  the values of the smooths of each angle less each mean function
  > 
  > gaitsmtharray = eval.fd(gaittime, gaitfd)

> hipresmat  = gaitsmtharray[,,1] - outer( hipmeanvec,rep(1,39))

> kneeresmat = gaitsmtharray[,,2] - outer(kneemeanvec,rep(1,39))

> #  the variances of the residuals of the smooth angles from their means
  > 
  > hipvar  = mean( hipresmat^2)

> kneevar = mean(kneeresmat^2)

> print(paste("Variances of fits by the means:",
              +             round(c(hipvar, kneevar),1)))
[1] "Variances of fits by the means: 44.3" "Variances of fits by the means: 35.4"

> #  compute the fits to the residual from the mean achieved by the PCA
  > 
  > hipfitarray  = array(NA, c(nrow(hipharmmat ),nrow(gaitscores),ncol(gaitscores)))

> kneefitarray = array(NA, c(nrow(kneeharmmat),nrow(gaitscores),ncol(gaitscores)))

> for (isc in 1:2) {
  +                hipfitarray[,,isc]  = hipharmmat  %*% t(gaitscores[,,isc])
  +                kneefitarray[,,isc] = kneeharmmat %*% t(gaitscores[,,isc])
  + }

> #  compute the variances of the PCA fits
  > 
  > hipfitvar  = c()

> kneefitvar = c()

> for (isc in 1:2) {
  +             hipfitvar  = c(hipfitvar,  mean( hipfitarray[,,isc]^2))
  +             kneefitvar = c(kneefitvar, mean(kneefitarray[,,isc]^2))
  + }

> #  compute percentages relative to the total PCA fit variance
  > #  these percentages will add to 100
  > 
  > hippropvar1 = c()

> kneepropvar1 = c()

> for (isc in 2) {
  +             hippropvar1  = c(hippropvar1,  hipfitvar[isc]/(hipfitvar[isc] +
                                                                 +                                            kneefitvar[isc]))
  +             kneepropvar1 = c(kneepropvar1, kneefitvar[isc]/(hipfitvar[isc]+
                                                                  +                                            kneefitvar[isc]))
  + }

> print(paste("Percentages of fits for the PCA:",
              +             round(100*c(hippropvar1, kneepropvar1),1)))
[1] "Percentages of fits for the PCA: 18.7" "Percentages of fits for the PCA: 81.3"

> #  compute percentages relative to the total mean fit variance
  > #  these percentages will add to the total percentage of fit
  > #  accounted for by the pca, which will typically be less than 100
  > 
  > hippropvar2  = c()

> kneepropvar2 = c()

> for (isc in 1:2) {
  +             hippropvar2  = c(hippropvar2,  hipfitvar[isc] /(hipvar+kneevar))
  +             kneepropvar2 = c(kneepropvar2, kneefitvar[isc]/(hipvar+kneevar))
  + }

> print((paste("Percentages of fits for the PCA:",
               +              round(100*c(hippropvar2, kneepropvar2),1))))
[1] "Percentages of fits for the PCA: 40.3" "Percentages of fits for the PCA: 5.4" 
[3] "Percentages of fits for the PCA: 2.6"  "Percentages of fits for the PCA: 23.5"

> #  --------------------------------------------------------------
> #           Canonical correlation analysis
  > #  --------------------------------------------------------------
> 
  > hipfd  <- gaitfd[,1]

> kneefd <- gaitfd[,2]

> hipfdPar  <- fdPar(hipfd,  harmaccelLfd, 1e2)

> kneefdPar <- fdPar(kneefd, harmaccelLfd, 1e2)

> ccafd    <- cca.fd(hipfd, kneefd, ncan=3, hipfdPar, kneefdPar)

> #  plot the canonical weight functions
  > 
  > op <- par(mfrow=c(2,1), mar=c(3,4,2,1), pty="m")

> plot.cca.fd(ccafd, cex=1.2)

  
  
  
  > par(op)

> #  display the canonical correlations
  > 
  > round(ccafd$ccacorr[1:6],3)
[1] 0.915 0.880 0.578 0.503 0.268 0.093

> plot(1:6, ccafd$ccacorr[1:6], type="b")

> #  --------------------------------------------------------------
> #         Register the angular acceleration of the gait data
  > #  --------------------------------------------------------------
> 
  > #  compute the acceleration and mean acceleration
  > 
  > D2gaitfd      <- deriv.fd(gaitfd,2)

> names(D2gaitfd$fdnames)[[3]] <- "Angular acceleration"

> D2gaitfd$fdnames[[3]] <- c("Hip", "Knee")

> D2gaitmeanfd  <- mean.fd(D2gaitfd)

> names(D2gaitmeanfd$fdnames)[[3]] <- "Mean angular acceleration"

> D2gaitmeanfd$fdnames[[3]] <- c("Hip", "Knee")

> #  set up basis for warping function
  > 
  > nwbasis   <- 7

> wbasis    <- create.bspline.basis(gaitrange,nwbasis,3)

> Warpfd    <- fd(matrix(0,nwbasis,5),wbasis)

> WarpfdPar <- fdPar(Warpfd)

> #  register the functions
  > 
  > gaitreglist <- register.fd(D2gaitmeanfd, D2gaitfd[1:5,], WarpfdPar, periodic=TRUE)


-------  Curve  1   --------
  
  Iter.    Criterion   Grad Length
0        1.3232      2.1911
1        1.2428      0.8337
2        1.1464      1.6862
3        0.961      0.8819
4        0.6983      0.2283
5        0.6864      0.7752
6        0.6798      0.9646
7        0.6483      0.6175
8        0.6207      0.0918
9        0.6204      0.0517
10        0.6204      0.0496

-------  Curve  2   --------
  
  Iter.    Criterion   Grad Length
0        1.7598      5.0698
1        1.4926      0.8933
2        1.4759      0.5794
3        1.3195      0.8225
4        1.3106      0.2128
5        1.2419      1.011
6        1.1895      0.112
7        1.1739      0.1465
8        1.1727      0.0674
9        1.1727      0.0674

-------  Curve  3   --------
  
  Iter.    Criterion   Grad Length
0        2.286      1.441
1        2.2214      1.5756
2        1.8154      0.6299
3        1.777      1.1382
4        1.7442      0.6348
5        1.7396      0.9934
6        1.7151      0.6029
7        1.7151      0.602

-------  Curve  4   --------
  
  Iter.    Criterion   Grad Length
0        7.5515      1.2216
1        6.8617      1.368
2        6.7634      0.406
3        6.7379      0.4406
4        6.7238      0.2243
5        6.7153      0.278
6        6.7125      0.1621
7        6.7073      0.1709
8        6.7062      0.1787
9        6.7021      0.1765
10        6.7015      0.1627
11        6.6986      0.1585
12        6.6821      0.6661
13        6.6767      0.1583
14        6.602      0.3518
15        6.5949      0.1837
16        6.5899      0.3476
17        6.5859      0.1828
18        6.5823      0.2245
19        6.5803      0.176
20        6.578      0.3279

-------  Curve  5   --------
  
  Iter.    Criterion   Grad Length
0        8.2408      4.0537
1        7.6118      3.6046
2        7.099      4.1164
3        6.7433      2.9578
4        6.3767      3.9006
5        6.1559      2.4197
6        5.9484      4.4851
7        5.6898      1.9491
8        5.6842      1.9435
9        5.2938      1.7102
10        5.045      1.8152
11        4.9746      0.9998
12        4.9746      0.9998

> plotreg.fd(gaitreglist)

  
  
  
  
  
  
  
  
  
  
  > #  display horizonal shift values
  > print(round(gaitreglist$shift,1))
reps 1 reps 2 reps 3 reps 4 reps 5 
0.4   -0.1   -1.0    0.1    1.4 

> #  histogram of horizontal shift values
  > par(mfrow=c(1,1))

> hist(gaitreglist$shift,xlab="Normalized time")

  
  > #  --------------------------------------------------------------
> #              Predict knee angle from hip angle
  > #             for angle and angular acceleration
  > #  --------------------------------------------------------------
> 
  > #  set up the data
  > 
  > hipfd  <- gaitfd[,1]

> kneefd <- gaitfd[,2]

> ncurve <- dim(kneefd$coefs)[2]

> kneemeanfd <- mean(kneefd)

> #  define the functional parameter object for regression functions
  > 
  > betafdPar <- fdPar(gaitbasis, harmaccelLfd)

> betalist  <- list(betafdPar,betafdPar)

> #  ----------  predict knee angle from hip angle --------
> 
  > conbasis <- create.constant.basis(c(0,20))

> constfd  <- fd(matrix(1,1,ncurve), conbasis)

> #  set up the list of covariate objects
  > 
  > xfdlist  <- list(constfd, hipfd)

> #  fit the current functional linear model
  > 
  > fRegressout <- fRegress(kneefd, xfdlist, betalist)

> #  set up and plot the fit functions and the regression functions
  > 
  > kneehatfd   <- fRegressout$yhatfd

> betaestlist <- fRegressout$betaestlist

> alphafd   <- betaestlist[[1]]$fd

> hipbetafd <- betaestlist[[2]]$fd

> op <- par(mfrow=c(2,1), ask=FALSE)

> plot(alphafd,   ylab="Intercept")
[1] "done"

> plot(hipbetafd, ylab="Hip coefficient")
[1] "done"

> par(op)

> #  compute and plot squared multiple correlation function
  > 
  > gaitfine    <- seq(0,20,len=101)

> kneemat     <- eval.fd(gaitfine, kneefd)

> kneehatmat  <- predict(kneehatfd, gaitfine)

> kneemeanvec <- as.vector(eval.fd(gaitfine, kneemeanfd))

> SSE0 <- apply((kneemat - outer(kneemeanvec, rep(1,ncurve)))^2, 1, sum)

> SSE1 <- apply((kneemat - kneehatmat)^2, 1, sum)

> Rsqr <- (SSE0-SSE1)/SSE0

> op <- par(mfrow=c(1,1),ask=FALSE)

> plot(gaitfine, Rsqr, type="l", ylim=c(0,0.4))

> #  for each case plot the function being fit, the fit,
  > #                     and the mean function
  > 
  > op <- par(mfrow=c(1,1),ask=TRUE)

> for (i in 1:ncurve) {
  +   plot( gaitfine, kneemat[,i], type="l", lty=1, col=4, ylim=c(0,80))
  +   lines(gaitfine, kneemeanvec,           lty=2, col=2)
  +   lines(gaitfine, kneehatmat[,i],        lty=3, col=4)
  +   title(paste("Case",i))
  + }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > par(op)

> #  ----------  predict knee acceleration from hip acceleration --------
> 
  > D2kneefd     <- deriv(kneefd, 2)

> D2hipfd      <- deriv(hipfd, 2)

> D2kneemeanfd <- mean(D2kneefd)

> #  set up the list of covariate objects
  > 
  > D2xfdlist  <- list(constfd,D2hipfd)

> #  fit the current functional linear model
  > 
  > D2fRegressout <- fRegress(D2kneefd, D2xfdlist, betalist)

> #  set up and plot the fit functions and the regression functions
  > 
  > D2kneehatfd   <- D2fRegressout$yhatfd

> D2betaestlist <- D2fRegressout$betaestlist

> D2alphafd   <- D2betaestlist[[1]]$fd

> D2hipbetafd <- D2betaestlist[[2]]$fd

> op <- par(mfrow=c(2,1), ask=FALSE)

> plot(D2alphafd,   ylab="D2Intercept")
[1] "done"

> plot(D2hipbetafd, ylab="D2Hip coefficient")
[1] "done"

> par(op)

> #  compute and plot squared multiple correlation function
  > 
  > D2kneemat     <- eval.fd(gaitfine, D2kneefd)

> D2kneehatmat  <- predict(D2kneehatfd, gaitfine)

> D2kneemeanvec <- as.vector(eval.fd(gaitfine, D2kneemeanfd))

> D2SSE0 <- apply((D2kneemat - outer(D2kneemeanvec, rep(1,ncurve)))^2, 1, sum)

> D2SSE1 <- apply((D2kneemat - D2kneehatmat)^2, 1, sum)

> D2Rsqr <- (D2SSE0-D2SSE1)/D2SSE0

> par(mfrow=c(1,1),ask=FALSE)

> plot(gaitfine, D2Rsqr, type="l", ylim=c(0,0.5))

> #  for each case plot the function being fit, the fit, and the mean function
  > 
  > op <- par(mfrow=c(1,1),ask=TRUE)

> for (i in 1:ncurve) {
  +   plot( gaitfine, D2kneemat[,i], type="l", lty=1, col=4, ylim=c(-20,20))
  +   lines(gaitfine, D2kneemeanvec,           lty=2, col=2)
  +   lines(gaitfine, D2kneehatmat[,i],        lty=3, col=4)
  +   lines(c(0,20), c(0,0), lty=2, col=2)
  +   title(paste("Case",i))
  + }
