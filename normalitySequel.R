# Include packages
library(pastecs)
library(ggplot2)


results <- read.csv2("C:/Users/Rumpel/Source/Repos/R/evalMensuralign/normalityOutput/results.csv")

# Kreuztabellen für reldist.norm < 0.05
allNotNormal <- subset(results, reldist.norm < 0.05)

# Subsets on sample size
size400 <- subset(allNotNormal, sample.size == "400")
size900 <- subset(allNotNormal, sample.size == "900")
size1600 <- subset(allNotNormal, sample.size == "1600")
size2500 <- subset(allNotNormal, sample.size == "2500")

# Set
bla <- table(allNotNormal$set)
blubb <- table(allNotNormal$params, allNotNormal$set)
bloo <- table(allNotNormal$params,allNotNormal$example)
# Params


# Example


plot <- ggplot(allNotNormal, aes(factor(params), fill = factor(example))) + geom_bar()
plot

plot2 <- ggplot(allNotNormal, aes(factor(sample.size), fill = factor(params))) + geom_bar()
plot2

plot3 <- ggplot(allNotNormal, aes(factor(params), fill = factor(set))) + geom_bar()
plot3
