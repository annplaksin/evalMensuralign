# Include packages
library(pastecs)
library(ggplot2)
library(gmodels)


results <- read.csv2("C:/Users/Rumpel/Source/Repos/R/evalMensuralign/normalityOutput/results.csv")

# Subset of reldist.norm < 0.05
allNotNormal <- subset(results, reldist.norm < 0.05)

# Subsets on sample size
size400 <- subset(allNotNormal, sample.size == "400")
size900 <- subset(allNotNormal, sample.size == "900")
size1600 <- subset(allNotNormal, sample.size == "1600")
size2500 <- subset(allNotNormal, sample.size == "2500")

# Easy contingency tables
bla <- table(allNotNormal$set)
blubb <- table(allNotNormal$params, allNotNormal$set)
bloo <- table(allNotNormal$params,allNotNormal$example)

# Some plots on factors
plot <- ggplot(allNotNormal, aes(factor(params), fill = factor(example))) + geom_bar()
plot

plot2 <- ggplot(allNotNormal, aes(factor(sample.size), fill = factor(params))) + geom_bar()
plot2

plot3 <- ggplot(allNotNormal, aes(factor(params), fill = factor(set))) + geom_bar()
plot3


# Contigency table on reldist.norm < 0.05 and params
results$normal <- results$reldist.norm < 0.05

params2normal <- table(results$params, results$normal)

CrossTable(params2normal, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# CrossTable with Fisher test throws error, try on better machine
# CrossTable(params2normal, chisq = TRUE, fisher = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Fisher test with simulated p (otherwise workspace too small)
fisher.test(params2normal, simulate.p.value = TRUE)
# ... with greater workspace
fisher.test(params2normal, workspace = 2e+07)


# Still need to calculate effect size, because odds ratio works only on 2x2 tables