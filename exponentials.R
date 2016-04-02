# Coursera Data Science Specialization
# Statistic Inference Course Project
# Author: John James
# Date: April 1, 2016
# Exponentials.R

## ---- environment
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(pastecs)
library(RColorBrewer)

## ---- end

########################################################################################
##                             THEORETICAL VALUES                                     ##
########################################################################################
## ---- theoreticalValues
n               <- 40  
lambda          <- 0.2

## Theoretical Distribution of Exponents
theoreticalMean <- 1/lambda
theoreticalSD   <- 1/(lambda*sqrt(n))
theoreticalVar  <- 1/(lambda^2*n)
theoreticalNorm <- rnorm(1000, theoreticalMean, theoreticalSD)
## ---- end

########################################################################################
##                                SAMPLE MEANS                                        ##
########################################################################################
## ---- sampleMeans
set.seed(505)
noSims        <- 1000
sampleDist       <- rexp(n = n * noSims, rate = lambda)
sampleDistMatrix <- matrix(rexp(n = n * noSims, 
                                rate = lambda), 
                                nrow = noSims)
sampleDistDF     <- data.frame(sampleDistMatrix)
sampleDistMeans  <- apply(sampleDistMatrix,1, mean)  


empiricalMean <- mean(sampleDistMeans)
empiricalVar  <- var(sampleDistMeans)
empiricalSD   <- sd(sampleDistMeans)
## ---- end

########################################################################################
##                            SAMPLE ExponentS                                        ##
########################################################################################
## ---- sample1000
noSims        <- 1000
sample1000    <- rexp(n = n, rate = lambda)
## ---- end



########################################################################################
##                                   ANALYSIS                                         ##
########################################################################################

## ---- compare
stat        <- c("Mean", "Variance", "Standard Error / Standard Deviation")
empirical   <- c(empiricalMean, empiricalVar, empiricalSD)
theoretical <- c(theoreticalMean, theoreticalVar,theoreticalSD)
compare     <- data.frame(stat,empirical,theoretical)
colnames(compare) <- c("Statistic", "Sample", "Theoretical")
## ---- end

## ---- t.test
meanTest <- t.test(sampleDistMeans,mu = theoreticalMean)
meanTest <- data.frame(meanTest$estimate, 
                       meanTest$conf.int[1], 
                       meanTest$conf.int[2], 
                       meanTest$p.value, 
                       row.names = NULL)
colnames(meanTest) <- c("Mean", "Lower CI", "Upper CI", "p.Value")
## ---- end

########################################################################################
##                                   PLOTS                                            ##
########################################################################################
## ---- densityPlot
densityPlot <- ggplot() +
  aes(sampleDistMeans) + 
  geom_density(aes(y = ..density..), fill = "orange", alpha = 0.5)  +
  scale_fill_gradient("Density", low = "orange", high = "red") +
  xlab("X") +
  ylab("Density") +
  ggtitle("Density of Sampling Distribution of Means vis-a-vis Population (Theoretical) Mean") +
  geom_vline(xintercept = theoreticalMean,colour = "BLUE") +
  geom_text(aes(x=theoreticalMean+1, y = 0.09, label = "Theoretical Mean of Exponentical Distribution"), colour = "BLUE") +
  geom_text(aes(x=theoreticalMean+0.8, y = 0.06, label = "with Rate Parameter lambda = 0.2"), colour = "BLUE") +
  theme_bw()
print(densityPlot)
## ---- end

## ---- dualDensities1
combined    <- melt(data.frame(theoreticalNorm, sampleDistMeans))
colnames(combined)  <- c("Distribution", "Value")
dualDensities1 <- ggplot(combined) +
  aes(x = Value, fill = Distribution) + 
  geom_density(alpha = .5) +
  xlab("X") +
  ylab("Density") +
  ggtitle("Theoretical versus Sample Distribution") +
  scale_fill_manual(values=c("blue", "orange"),
                    breaks=c("theoreticalNorm", "sampleDistMeans"),
                    labels=c("Theoretical","Sample")) +
  theme_bw() 
print(dualDensities1)
## ---- end

## ---- nqp
qqnorm(sampleDistMeans,col = "red", pch = 16, main = "Normal Quartile Plot of Sample Means (n=1000)")
qqline(sampleDistMeans,col = "blue")
legend("bottomright", legend = c("Empirical Means", "Theoretical Normal"),
       col = c("red","blue"), pch = c(16,NA),
       lwd = c(NA,2), lty = c(NA,1), cex = .8, bty = "n")
## ---- end

## ---- dualDensities2
combined    <- melt(data.frame(sample1000, sampleDistMeans))
colnames(combined)  <- c("Distribution", "Value")
dualDensities2 <- ggplot(combined) +
  aes(x = Value, fill = Distribution) + 
  geom_density(alpha = .5) +
  xlab("X") +
  ylab("Density") +
  ggtitle("Exponentials vis-a-vis Means") +
  scale_fill_manual(values=c("blue", "orange"),
                    breaks=c("sample1000", "sampleDistMeans"),
                    labels=c("Exponentials","Means")) +
  theme_bw() 
print(dualDensities2)
## ---- end
