# Coursera Data Science Specialization
# Statistic Inference Course Project
# Author: John James
# Date: March 25, 2016
# experiment.R

## ---- environment
library(ggplot2)
library(gridExtra)
library(reshape2)

## ---- end

########################################################################################
##                        EXPONENTIAL DISTRIBUTION                                    ##
########################################################################################
## ---- exponentialDistribution
set.seed(505)
n               <- 40  
lambda          <- 0.2
expDist40         <- rexp(n = n, rate = lambda)
## ---- end

## ---- theoreticalEDStats
theoreticalMean40 <- 1/lambda
theoreticalSD40   <- 1/lambda
theoreticalVar40  <- 1/(lambda^2)
## ---- end

## ---- expDist40Stats
empiricalMean40   <- mean(expDist40)
empiricalSD40     <- sd(expDist40)
empiricalVar40    <- var(expDist40)
## ---- end

## ---- plotEDdensities
edDensities <- ggplot() +
  aes(expDist40) +
  xlab("Exponentials") +
  ylab("Density") +
  ggtitle("Theoretical & Empirical Exponential Distribution Densities") +
  theme_bw() +
  stat_function(fun = dnorm, args = list(mean = theoreticalMean40, sd = theoreticalSD40), aes(colour = "Theoretical")) +
  stat_function(fun = dnorm, args = list(mean = empiricalMean40, sd = empiricalSD40), aes(colour = "Empirical")) +
  scale_colour_manual("Distribution", values = c("orange", "blue"))                
print(edDensities)
## ---- end

## ---- expDist40CI
mu    <- empiricalMean40
s     <- empiricalSD40
N     <- 40
error <- qnorm(0.975)*s/sqrt(N)
edcill <- mu - error
edciul <- mu + error
## ---- end


########################################################################################
##                          SAMPLE DISTRIBUTION                                       ##
########################################################################################
## ---- sampleDistribution
set.seed(505)
noSims        <- 1000
expDist       <- rexp(n = n * noSims, rate = lambda)
expDistMatrix <- matrix(rexp(n = n * noSims, rate = lambda), nrow = noSims)
expDistDF     <- data.frame(expDistMatrix)
expDistMeans  <- apply(expDistMatrix,1, mean)  
## ---- end

## ---- expDistStats
empiricalMean <- mean(expDistMeans)
empiricalVar  <- var(expDistMeans)
empiricalSD   <- sd(expDistMeans)
## ---- end

## ---- theoreticalStats
theoreticalMean <- 1/lambda
theoreticalSD   <- 1/(lambda*sqrt(n))
theoreticalVar  <- 1/(lambda^2*n)
## ---- end


########################################################################################
##                             DATA DIAGNOSTICS                                       ##
########################################################################################
## ---- plotExpDist40
expDist40Plot <- ggplot() +
  aes(expDist40) + 
  geom_histogram(aes(fill = ..count..))  +
  scale_fill_gradient("Count", low = "orange", high = "red") +
  xlab("X") +
  ylab("Counts") +
  ggtitle(expression(paste("Exponential Distribution (n=40 | ",lambda,"=0.2)"))) +
  theme_bw()
print(expDist40Plot)
## ---- end

## ---- EDsummaryStats
theoreticalMean40 <- 1/lambda
theoreticalSD40   <- 1/lambda
theoreticalVar40  <- 1/(lambda^2)


empiricalMean40   <- mean(expDist40)
empiricalSD40     <- sd(expDist40)
empiricalVar40    <- var(expDist40)
## ---- end

## ---- sampleSummaryStats
theoreticalMean <- 1/lambda
theoreticalSD   <- 1/(lambda*sqrt(n))
theoreticalVar  <- 1/(lambda^2*n)

empiricalMean <- mean(expDistMeans)
empiricalVar  <- var(expDistMeans)
empiricalSD   <- sd(expDistMeans)
## ---- end


########################################################################################
##                              SAMPLE MEAN                                           ##
########################################################################################
## ---- sampleAveragesCI
mu    <- empiricalMean
s     <- empiricalSD
N     <- 1000
error <- qnorm(0.975)*s/sqrt(N)
sacill <- mu - error
saciul <- mu + error
## ---- end

# ---- meansPlot
meansPlot <- ggplot() +
  aes(expDistMeans) + 
  geom_histogram(aes(y = ..count..,fill = ..count..))  +
  scale_fill_gradient("Count", low = "orange", high = "red") +
  xlab("X") +
  ylab("Count") +
  ggtitle("Distribution of Sample Means of Averages (Sample Size = 40)") +
  theme_bw() +
  geom_vline(xintercept = theoreticalMean, colour = "blue")
print(meansPlot)
## ---- end

########################################################################################
##                              SAMPLE VARIANCE                                       ##
########################################################################################
## RenderSampleDistribution
expDistPlot <- ggplot() +
  aes(expDistMeans) + 
  geom_histogram(aes(fill = ..count..))  +
  scale_fill_gradient("Count", low = "orange", high = "red") +
  xlab("X") +
  ylab("Counts") +
  ggtitle(expression(paste("Distribution of Empirical Averages Drawn From Exponential Distribution (n=40 | ",lambda,"=0.2)"))) +
  theme_bw() +
  geom_vline(xintercept = theoreticalMean, colour = "blue")
## ---- end

########################################################################################
##                    NORMALITY OF SAMPLE DISTRIBUTION                                ##
########################################################################################
## ---- renderNQP
qqnorm(expDistMeans,col = "red", pch = 16, main = "Normal Quartile Plot of Empirical Means (n=1000)")
qqline(expDistMeans,col = "blue")
legend("bottomright", legend = c("Empirical Means", "Theoretical Normal"),
       col = c("red","blue"), pch = c(16,NA),
       lwd = c(NA,2), lty = c(NA,1), cex = .8, bty = "n")
## ---- end

# ---- renderDensity
densityPlot <- ggplot() +
  aes(expDistMeans) + 
  geom_histogram(aes(y = ..density..,fill = ..density..))  +
  scale_fill_gradient("Density", low = "orange", high = "red") +
  xlab("X") +
  ylab("Density") +
  ggtitle("Density of Sample Means (Sample Size = 40)") +
  theme_bw() +
  geom_vline(xintercept = theoreticalMean, colour = "blue") +
  stat_function(fun = dnorm, args = list(mean = theoreticalMean, sd = theoreticalSD))
print(densityPlot)
## ---- end


########################################################################################
##                         SAMPLES VERSUS AVERAGES OF SAMPLES                         ##
########################################################################################
## ---- expDist1000
set.seed(505)
n               <- 1000
lambda          <- 0.2
expDist1000         <- rexp(n = n, rate = lambda)
## ---- end

## ---- expDist1000Plot
expDist1000Plot <- ggplot() +
  aes(expDist1000) + 
  geom_histogram(aes(fill = ..count..))  +
  scale_fill_gradient("Count", low = "orange", high = "red") +
  xlab("X") +
  ylab("Counts") +
  ggtitle(expression(paste("Exponential Distribution (n=1000 | ",lambda,"=0.2)"))) +
  theme_bw()
print(expDist1000Plot)
## ---- end

## ---- expDist1000Stats
em1000    <- mean(expDist1000)
esd1000   <- sd(expDist1000)
ev1000  <- var(expDist1000)
## ---- end

## ---- plotDualDensities
dist <- melt(data.frame(expDistMeans, expDist1000))
colnames(dist)  <- c("Distribution", "Value")

dualDensities <- ggplot(dist) +
  aes(x = Value, fill = Distribution) + 
  geom_density(alpha = .3) +
  xlab("X") +
  ylab("Density") +
  ggtitle("Density Exponentials versus Averages of Exponentials") +
  scale_fill_manual(values=c("red", "orange"),
                      breaks=c("expDistMeans", "expDist1000"),
                      labels=c("Averages","Exponentials")) +
  theme_bw() 
print(dualDensities)

## ---- end



########################################################################################
##                    DATA AND OBSERVATION SECTION VARIABLES                          ##
########################################################################################
## ---- dataObservations
tm40  <- theoreticalMean40
tsd40 <- theoreticalSD40   
tv40  <- theoreticalVar40  

em40  <- empiricalMean40   
esd40 <- empiricalSD40     
ev40  <- empiricalVar40    

tm  <- theoreticalMean 
tsd <- theoreticalSD   
tv  <- theoreticalVar  

em  <- empiricalMean
ev  <- empiricalVar 
esd <- empiricalSD  

## ---- end

