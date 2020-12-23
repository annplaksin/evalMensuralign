# Include packages
library(pastecs)
library(ggplot2)
library(gmodels) # CrossTable()
# library(lsr) (other package with Cramers V)
library(rcompanion)

results <- read.csv2("/normalityOutput/results.csv")

# Throw out superlogical sets of withoutSB, because they're not a used parameter set
results <- results[!(results$set == "withoutSB" & (results$params == "Superlogical" | results$params == "Superlogical.gap")), ]


# Contigency table on reldist.norm < 0.05 and params
results$nonNormal <- results$reldist.norm < 0.05
results$highlyNonNormal <- results$reldist.norm < 0.001

# Chi² and fisher tests with effect sizes
# Use Cramer's V from rcompanion package

# Chi² / Fisher on sample size to normality
sample2nonNormal <- table(results$sample.size, results$nonNormal)
sample2highlyNon <- table(results$sample.size, results$highlyNonNormal)

CrossTable(sample2nonNormal, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(sample2nonNormal, workspace = 2e+07)
cramerV(sample2nonNormal)

CrossTable(sample2highlyNon, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(sample2highlyNon, workspace = 2e+07)
cramerV(sample2highlyNon)


# Chi² / Fisher test on params to normality
params2nonNormal <- table(results$params, results$nonNormal)
params2highlyNonNormal <- table(results$params, results$highlyNonNormal)

# CrossTable with Fisher test throws error, try on better machine
# CrossTable(params2normal, chisq = TRUE, fisher = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Fisher test with simulated p (otherwise workspace too small): fisher.test(params2normal, simulate.p.value = TRUE)

CrossTable(params2nonNormal, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
# ... with greater workspace
fisher.test(params2nonNormal, workspace = 2e+08)
cramerV(params2nonNormal)

CrossTable(params2highlyNonNormal, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(params2highlyNonNormal, workspace = 2e+07)
cramerV(params2highlyNonNormal)
cohenW(params2highlyNonNormal)

# Plot params to normality
params2nonPlot <- ggplot(results, aes(factor(params), fill = factor(nonNormal))) + geom_bar(position = "stack")
params2nonPlot + scale_color_grey() + theme_bw()

params2highPlot <- ggplot(results, aes(factor(params), fill = factor(highlyNonNormal))) + geom_bar(position = "stack")
params2highPlot + scale_color_grey() + theme_bw()

# Why aren't the plots in grey scale?!

# Chi² / Fisher on example to normality
exam2nonNormal <- table(results$example, results$nonNormal)
exam2highlyNon <- table(results$example, results$highlyNonNormal)


CrossTable(exam2nonNormal, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(exam2nonNormal, workspace = 2e+07)
cramerV(exam2nonNormal)

CrossTable(exam2highlyNon, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(exam2highlyNon, workspace = 2e+07)
cramerV(exam2highlyNon)

# chi.highly <- chisq.test(exam2highlyNon)

# Chi² / Fisher to file set
set2nonNormal <- table(results$set, results$nonNormal)
set2highlyNon <- table(results$set, results$highlyNonNormal)

CrossTable(set2nonNormal, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(set2nonNormal, workspace = 2e+07)
cramerV(set2nonNormal)

CrossTable(set2highlyNon, chisq = TRUE, expected = TRUE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, sresid = TRUE, format = "SPSS")
fisher.test(set2highlyNon, workspace = 2e+07)
cramerV(set2highlyNon)
