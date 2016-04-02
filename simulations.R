# Coursera Data Science Specialization
# Statistic Inference Course Project
# Author: John James
# Date: March 25, 2016
# simulations.R

## ---- environment
library(ggplot2)
library(gridExtra)
## ---- end

## ---- simulationParameters
samples   <- 40
noSims    <- 1000
lambda    <- 0.2
## ---- end

## ---- theoreticalStatistics
theoreticalMean     <- 1/lambda
theoreticalSD       <- 1/lambda
theoreticalVar      <- 1/(lambda^2)
## ---- end

## ---- simulations
expDist = NULL
expDist         <- rexp(n = samples * noSims, rate = lambda)
empiricalMean   <- mean(expDist)
empiricalVar    <- var(expDist)
empiricalSD     <- sd(expDist)
cumulativeMean  <- cumsum(expDist) / seq_along(expDist)
cumulativeVar   <- cumsum((expDist - empiricalMean)^2) / (seq_along(expDist) -1)
## ---- end

## ---- sampleMeansStatistics
expDistMatrix   <- matrix(expDist, nrow = noSims)
sampleMean      <- apply(expDistMatrix, 1, mean)   #calculate the sample mean of the 40 exponentials for each simulation
sampleVariance  <- apply(expDistMatrix, 1, var)    #calculate the sample variance of the means of the 40 exponentials for each simulation 
## ---- end

## ---- plotMeansHistogram
a <- ggplot() +
      aes(sampleMean) + 
      geom_histogram(aes(fill = ..count..))  +
      scale_fill_gradient("Count", low = "orange", high = "red") +
      xlab("Sample Means") +
      ylab("Counts") +
      ggtitle("Distribution of 1000 Sample Means of Sample Size 40") +
      theme_bw() +
      geom_vline(xintercept = theoreticalMean, colour = "blue")

print(a)

## ---- end

## ---- plotVarHistogram
s <- ggplot() +
      aes(sampleVariance) + 
      geom_histogram(aes(fill = ..count..))  +
      scale_fill_gradient("Count", low = "orange", high = "red") +
      xlab("Sample Variance") +
      ylab("Counts") +
      theme_bw() +
      ggtitle("Variance of 1000 Sample Means of Sample Size 40") +
      geom_vline(xintercept = theoreticalVar, colour = "BLUE")

print(s)
## ---- end

## ---- plotCumulativeMean
cm <- ggplot(data.frame(x = 1:noSims, y = cumulativeMean), aes(x = x, y = y)) +
        geom_hline(yintercept = theoreticalMean) +
        geom_line(colour = "DARK GREEN", size = 2) +
        xlab("Number of Simulations") +
        ylab("Cumulative Mean") +
        theme_bw() +
        ggtitle("Cumulative Mean of 40 Exponents ")
print(cm)
## ---- end



## ---- plotCumulativeVar
cm <- ggplot(data.frame(x = 1:noSims, y = cumulativeVar), aes(x = x, y = y)) +
        geom_hline(yintercept = theoreticalMean) +
        geom_line(colour = "DARK BLUE", size = 2) +
        xlab("Number of Simulations") +
        ylab("Cumulative Variance") +
        theme_bw() +
        ggtitle("Cumulative Variance of 40 Exponents ")
print(cm)
## ---- end


## ---- plotBothDistributions
e <- ggplot() +
        aes(expDist) +
        geom_histogram(aes(fill = ..density..))  +
        scale_fill_gradient("Count", low = "orange", high = "red") +
        xlab("Exponents") +
        ylab("Count") +
        theme_bw() +
        ggtitle("Distribution of Exponents")

d <- ggplot() +
      aes(sampleMean) + 
      geom_histogram(aes(fill = ..density..))  +
      scale_fill_gradient("Count", low = "orange", high = "red") +
      xlab("Simulation") +
      ylab("Count") +
      theme_bw() +
      ggtitle("Sample Mean Distribution") +
      stat_function(fun = dnorm, 
                args = list(mean = theoreticalMean, sd = theoreticalSD), 
                lwd = 2, 
                col = 'blue')

grid.arrange(e, d, ncol = 2)
## ---- end