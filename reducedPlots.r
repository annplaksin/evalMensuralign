#Includes
library(reshape)
library(ggplot2)


zData <- read.csv2("mainOutput/resultZscores.csv")

# Declare Groups as factors
zData$Group <- factor(zData$Group, levels = c(1:7), labels = c(1:7))


# Then we need also tons of rank means to report
# First, melt wide format into long
meltedZ <- melt(zData, id = c("Group", "Case"), measured = c("Signbased.all","Signbased.all.gap","Signbased.log","Signbased.log.gap",
                                                             "Signbased.vis","Signbased.vis.gap","Superlogical","Superlogical.gap",
                                                             "withoutSB.all","withoutSB.all.gap","withoutSB.log","withoutSB.log.gap",
                                                             "withoutSB.vis","withoutSB.vis.gap"))
# Rename columns to have meaningful names
colnames(meltedZ)[3] <- "Set"
colnames(meltedZ)[4] <- "Z"

# Get interesting parameter sets for plotting
interestingZs <- meltedZ[meltedZ$Set == "Signbased.vis.gap"|
                           meltedZ$Set == "Signbased.log.gap"|
                           meltedZ$Set == "Signbased.all.gap"|
                           meltedZ$Set == "Superlogical.gap", ]

interestingZs$numGroup <- as.numeric(interestingZs$Group)

mdnLinePlot <- ggplot(interestingZs, aes(numGroup, Z, color = factor(Set))) + geom_point(size=2) +
  stat_summary(aes(y = Z), fun.y=median, geom="smooth", size = 1) +
  labs(x = "Groups (numeric)", y = expression(paste(sigma)), title = expression(paste(sigma," per Group with Median lines")), subtitle = "With Median lines", color = "Parameter sets")
mdnLinePlot + theme_bw() + scale_color_brewer(palette = "Spectral")
ggsave("mainOutput\\trendlinePlot-4.png", width = 20, height = 15, units = "cm")
