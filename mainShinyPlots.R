# Include
library(ggplot2)

# Do fancy comparison plots

# 1. Load everything
everything <- read.csv2("mainOutput/everything.csv")
everything$group <- factor(everything$group, levels=c(1:7), labels=c(1:7))

sets <- levels(everything$set)

# 2. Plot comparison orig vs. random
intersting <- everything[everything$set == "Signbased.log.gap"|
                         everything$set == "Signbased.vis.gap"|
                         everything$set == "Superlogical.gap", ]
interesting.plot <- ggplot(intersting[intersting$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = intersting[intersting$source == "original", ], shape=18, size=1, na.rm = TRUE) +
  labs(x = "Group",y = "Relative Distance", color = "Scenario")
interesting.plot + theme_bw() + scale_color_brewer(palette = "Set1")
ggsave("mainOutput\\compareInterestingDists.png", width = 20, height = 15, units = "cm")

visVsSup <- everything[everything$set == "Signbased.vis.gap"|
                   everything$set == "Superlogical.gap", ]
visVsSup.plot <- ggplot(visVsSup[visVsSup$source == "randomized", ], aes(x = group, y = reldist)) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = visVsSup[visVsSup$source == "original", ], shape=18, size=1, na.rm = TRUE) +
  labs(x = "Group",y = "Relative Distance", color = "Scenario") +
  facet_wrap(~ factor(set), nrow = 1)
visVsSup.plot + theme_bw() #+ scale_color_brewer(palette = "Set1")
ggsave("mainOutput\\compareVisVsSuperDists.png", width = 20, height = 15, units = "cm")

# 3. Plot comparison sets without gap vs. gap sets

#all
all <- everything[everything$set == "Signbased.all"|everything$set == "Signbased.all.gap"|
                    everything$set == "withoutSB.all"|everything$set == "withoutSB.all.gap", ]
all.plot <- ggplot(all[all$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = all[all$source == "original", ], shape=18, size=1, na.rm = TRUE)
all.plot + scale_color_grey() + theme_bw()


#vis
vis <- everything[everything$set == "Signbased.vis"|everything$set == "Signbased.vis.gap"|
                    everything$set == "withoutSB.vis"|everything$set == "withoutSB.vis.gap", ]
vis.plot <- ggplot(vis[vis$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = vis[vis$source == "original", ], shape=18, size=1, na.rm = TRUE) + 
  labs(x = "Group",y = "Relative Distance", color = "Scenario")
vis.plot + scale_color_grey() + theme_bw()
ggsave("mainOutput\\visPlots.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\visPlots.pdf", width = 20, height = 15, units = "cm")

#log
log <- everything[everything$set == "Signbased.log"|everything$set == "Signbased.log.gap"|
                    everything$set == "withoutSB.log"|everything$set == "withoutSB.log.gap", ]

log.plot <- ggplot(log[log$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = log[log$source == "original", ], shape=18, size=1, na.rm = TRUE) + 
  labs(x = "Group",y = "Relative Distance", color = "Scenario")
log.plot + scale_color_grey() + theme_bw()
ggsave("mainOutput\\logPlots.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\vlogPlots.pdf", width = 20, height = 15, units = "cm")

#superlogical
super <- everything[everything$set == "Superlogical"|everything$set == "Superlogical.gap", ]

super.plot <- ggplot(super[super$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = super[super$source == "original", ], shape=18, size=1, na.rm = TRUE) + 
  labs(x = "Group",y = "Relative Distance", color = "Scenario")
super.plot + scale_color_grey() + theme_bw()
ggsave("mainOutput\\superPlots.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\superPlots.pdf", width = 20, height = 15, units = "cm")

# Highlights
highlights <- everything[everything$set == "Signbased.log.gap" | everything$set == "Signbased.vis.gap" | everything$set == "Superlogical.gap", ]
highlight.plot <- ggplot(highlights[highlights$source == "randomized", ], aes(x = group, y = reldist, color=factor(set))) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(data = highlights[highlights$source == "original", ], shape=18, size=1, na.rm = TRUE) + 
  labs(x = "Group",y = "Relative Distance", color = "Scenario")
highlight.plot + scale_color_grey() + theme_bw()
ggsave("mainOutput\\highlightPlots.eps", width = 20, height = 15, units = "cm")
ggsave("mainOutput\\highlightPlots.pdf", width = 20, height = 15, units = "cm")

# Compare all original alignments --- naaaaaasty!
origs <- everything[everything$source == "original", ]
origs$group <- as.numeric(origs$group)
origplot <- ggplot(origs, aes(x = group, y = reldist, color=factor(set))) + geom_jitter()
origplot
