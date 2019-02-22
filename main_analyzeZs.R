#Includes
library(reshape)
library(ggplot2)
library(car)
library(pastecs)
library(pgirmess)
library(clinfun)

zData <- read.csv2("mainOutput/resultZscores.csv")

# Declare Groups as factors
zData$Group <- factor(zData$Group, levels = c(1:7), labels = c(1:7))

# Check normality per group
# Ignore Signbased.all.gap, withoutSB.all.gap, Signbased.vis, wuthoutSB.vis
stat.desc(zData[zData$Group=="1",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="2",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="3",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="4",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="5",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="6",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

stat.desc(zData[zData$Group=="7",c("Signbased.all","Signbased.log","Signbased.log.gap",
                                   "Signbased.vis.gap","Superlogical","Superlogical.gap",
                                   "withoutSB.all","withoutSB.log","withoutSB.log.gap","withoutSB.vis.gap")], 
          norm = TRUE)

# Levene's test on homogeneity of variance per parameter set
leveneTest(zData$Signbased.all, zData$Group, center = median)
#leveneTest(zData$Signbased.all.gap, zData$Group, center = median)
leveneTest(zData$Signbased.log, zData$Group, center = median)
leveneTest(zData$Signbased.log.gap, zData$Group, center = median)
#leveneTest(zData$Signbased.vis, zData$Group, center = median)
leveneTest(zData$Signbased.vis.gap, zData$Group, center = median)
leveneTest(zData$Superlogical, zData$Group, center = median)
leveneTest(zData$Superlogical.gap, zData$Group, center = median)
leveneTest(zData$withoutSB.all, zData$Group, center = median)
#leveneTest(zData$withoutSB.all.gap, zData$Group, center = median)
leveneTest(zData$withoutSB.log, zData$Group, center = median)
leveneTest(zData$withoutSB.log.gap, zData$Group, center = median)
#leveneTest(zData$withoutSB.vis, zData$Group, center = median)
leveneTest(zData$withoutSB.vis.gap, zData$Group, center = median)

# Then, do the Kruskal-Wallis test with every relevant set
kruskal.sign.all <- kruskal.test(Signbased.all ~ Group, data = zData)
#kruskal.sign.all.gap <- kruskal.test(Signbased.all.gap ~ Group, data = zData)
kruskal.sign.log <- kruskal.test(Signbased.log ~ Group, data = zData)
kruskal.sign.log.gap <- kruskal.test(Signbased.log.gap ~ Group, data = zData)
#kruskal.sign.vis <- kruskal.test(Signbased.vis ~ Group, data = zData)
kruskal.sign.vis.gap <- kruskal.test(Signbased.vis.gap ~ Group, data = zData)
kruskal.super <- kruskal.test(Superlogical ~ Group, data = zData)
kruskal.super.gap <- kruskal.test(Superlogical.gap ~ Group, data = zData)

kruskal.without.all <- kruskal.test(withoutSB.all ~ Group, data = zData)
#kruskal.without.all.gap <- kruskal.test(withoutSB.all.gap ~ Group, data = zData)
kruskal.without.log <- kruskal.test(withoutSB.log ~ Group, data = zData)
kruskal.without.log.gap <- kruskal.test(withoutSB.log.gap ~ Group, data = zData)
#kruskal.without.vis <- kruskal.test(withoutSB.vis ~ Group, data = zData)
kruskal.without.vis.gap <- kruskal.test(withoutSB.vis.gap ~ Group, data = zData)

kruskal.sign.all;kruskal.sign.log;kruskal.sign.log.gap;kruskal.sign.vis.gap
kruskal.super;kruskal.super.gap
kruskal.without.all;kruskal.without.log;kruskal.without.log.gap;kruskal.without.vis.gap

# Then we need also tons of rank means to report
# First, melt wide format into long
meltedZ <- melt(zData, id = c("Group", "Case"), measured = c("Signbased.all","Signbased.all.gap","Signbased.log","Signbased.log.gap",
                                                             "Signbased.vis","Signbased.vis.gap","Superlogical","Superlogical.gap",
                                                             "withoutSB.all","withoutSB.all.gap","withoutSB.log","withoutSB.log.gap",
                                                             "withoutSB.vis","withoutSB.vis.gap"))
# Rename columns to have meaningful names
colnames(meltedZ)[3] <- "Set"
colnames(meltedZ)[4] <- "Z"

meltedZ$Ranks <-rank(meltedZ$Z)
rankmeans <- by(meltedZ$Ranks, meltedZ$Group, mean)

# Mean ranks are wanted by set, so get subsets and then rank them

#Signbased.all
signAll <- meltedZ[meltedZ$Set == "Signbased.all", ]
signAll$Ranks <-rank(signAll$Z)
signAll.rankmeans <- by(signAll$Ranks, signAll$Group, mean)

#Signbased.log
signLog <- meltedZ[meltedZ$Set == "Signbased.log", ]
signLog$Ranks <-rank(signLog$Z)
signLog.rankmeans <- by(signLog$Ranks, signLog$Group, mean)

#Signbased.log.gap
signLogGap <- meltedZ[meltedZ$Set == "Signbased.log.gap", ]
signLogGap$Ranks <-rank(signLogGap$Z)
signLogGap.rankmeans <- by(signLogGap$Ranks, signLogGap$Group, mean)

#Signbased.vis.gap
signVisGap <- meltedZ[meltedZ$Set == "Signbased.vis.gap", ]
signVisGap$Ranks <-rank(signVisGap$Z)
signVisGap.rankmeans <- by(signVisGap$Ranks, signVisGap$Group, mean)

#Superlogical
super <- meltedZ[meltedZ$Set == "Superlogical", ]
super$Ranks <-rank(super$Z)
super.rankmeans <- by(super$Ranks, super$Group, mean)

#Superlogical.gap
superGap <- meltedZ[meltedZ$Set == "Superlogical.gap", ]
superGap$Ranks <-rank(superGap$Z)
superGap.rankmeans <- by(superGap$Ranks, superGap$Group, mean)

#withoutSB.all
withoutAll <- meltedZ[meltedZ$Set == "withoutSB.all", ]
withoutAll$Ranks <-rank(withoutAll$Z)
withoutAll.rankmeans <- by(withoutAll$Ranks, withoutAll$Group, mean)

#withoutSB.log
withoutLog <- meltedZ[meltedZ$Set == "withoutSB.log", ]
withoutLog$Ranks <-rank(withoutLog$Z)
withoutLog.rankmeans <- by(withoutLog$Ranks, withoutLog$Group, mean)

#withoutSB.log.gap
withoutLogGap <- meltedZ[meltedZ$Set == "withoutSB.log.gap", ]
withoutLogGap$Ranks <-rank(withoutLogGap$Z)
withoutLogGap.rankmeans <- by(withoutLogGap$Ranks, withoutLogGap$Group, mean)

#withoutSB.vis.gap
withoutVisGap <- meltedZ[meltedZ$Set == "withoutSB.vis.gap", ]
withoutVisGap$Ranks <-rank(withoutVisGap$Z)
withoutVisGap.rankmeans <- by(withoutVisGap$Ranks, withoutVisGap$Group, mean)

signAll.rankmeans
signLog.rankmeans; signLogGap.rankmeans; 
signVisGap.rankmeans
super.rankmeans; superGap.rankmeans
withoutAll.rankmeans
withoutLog.rankmeans; withoutLogGap.rankmeans; 
withoutVisGap.rankmeans

# Also, we want to know where a difference lies... therefore we perform a fancy post-hoc analysis with kruskalmc()
# The Group containing the compares to Quis dabit capiti serves as control group in a two-tailed test with only 6 comparisons 
# (a little fishing for effects must be allowed in that case, because we ordered the groups according a certain expectation)
# But we still want a good alpha

# Remember... groups need to be turned around to make more sense
kruskalmc(Z ~ Group, data = signAll, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signLog, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signLogGap, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signVisGap, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = super, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = superGap, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutAll, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutLog, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutLogGap, probs = 0.001, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutVisGap, probs = 0.001, cont = 'two-tailed')

# Again with lower prob level
kruskalmc(Z ~ Group, data = signAll, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signLog, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signLogGap, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = signVisGap, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = super, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = superGap, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutAll, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutLog, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutLogGap, probs = 0.01, cont = 'two-tailed')
kruskalmc(Z ~ Group, data = withoutVisGap, probs = 0.01, cont = 'two-tailed')


# Then we want to find the set with the most significant trend. The Z-Scores should increase with the expected similarity
# A Jonckheere-Terpstra test will help us#
jonckheere.test(signAll$Z, as.numeric(signAll$Group))
jonckheere.test(signLog$Z, as.numeric(signLog$Group))
jonckheere.test(signLogGap$Z, as.numeric(signLogGap$Group))
jonckheere.test(signVisGap$Z, as.numeric(signVisGap$Group))
jonckheere.test(super$Z, as.numeric(super$Group))
jonckheere.test(superGap$Z, as.numeric(superGap$Group))
jonckheere.test(withoutAll$Z, as.numeric(withoutAll$Group))
jonckheere.test(withoutLog$Z, as.numeric(withoutLog$Group))
jonckheere.test(withoutLogGap$Z, as.numeric(withoutLogGap$Group))
jonckheere.test(withoutVisGap$Z, as.numeric(withoutVisGap$Group))

# And now, we can draw some fancy plots of our data to explain our interesting findings!
fancy.plot <- ggplot(meltedZ, aes(Group, Z, color = factor(Set))) + geom_boxplot()
fancy.plot

interestingZs <- meltedZ[meltedZ$Set == "Signbased.vis.gap"|meltedZ$Set == "Signbased.log.gap"|
                           meltedZ$Set == "Superlogical"| meltedZ$Set == "Superlogical.gap"|
                           meltedZ$Set == "withoutSB.log"|meltedZ$Set == "withoutSB.log.gap"|
                           meltedZ$Set == "withoutSB.vis.gap", ]
interestingPlot <- ggplot(interestingZs, aes(Group, Z, color = factor(Set))) + geom_boxplot() + labs(color = "Parameter sets")
interestingPlot + theme_bw() + scale_color_brewer(palette = "Paired")
ggsave("mainOutput\\highlightZplots.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\highlightZplots.pdf", width = 20, height = 15, units = "cm")

interestingZs$numGroup <- as.numeric(interestingZs$Group)

mdnLinePlot <- ggplot(interestingZs, aes(numGroup, Z, color = factor(Set))) + geom_point(size=2) +
  stat_summary(aes(y = Z), fun.y=median, geom="smooth", size = 1) +
  labs(x = "Groups (numeric)", y = "Z-Value", title = "Z-Values per Group", subtitle = "With Median lines", color = "Parameter sets")
mdnLinePlot + theme_bw() + scale_color_brewer(palette = "Paired")
ggsave("mainOutput\\trendlinePlot.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\trendlinePlot.pdf", width = 20, height = 15, units = "cm")
