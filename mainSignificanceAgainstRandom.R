#Includes
library(ggplot2)
library(reshape)

# Self made function to calculate effect size
# Returns r
rFromWilcox <- function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z / sqrt(N)
  r
}

# Import data for monster loop
data <- read.csv2("mainOutput/cases.csv")

sets <- levels(data$Set)
groups <- c(1:7)
cases <- c(1:70)

# Loop through rows of cases and collect data accordingly
for (i in 1:980) {
  # Get data
  input <- paste("mainInput\\",cases[data[i,2]],"_",sets[data[i,3]],".csv", sep="")
  results <- read.csv(input, header = TRUE, sep = ";", dec = ",")
  results$RelDist <- results$Distance / results$LengthBacktrace
  
  source <- results$Source
  reldist <- results$RelDist
  group <- c(rep(data[i,1],2501))
  case <- c(rep(data[i,2],2501))
  set <- c(rep(data[i,3],2501))
  
  lengthA <- results$LengthSequenceA
  lengthB <- results$LengthSequenceB
  lengthTrace <- results$LengthBacktrace
  
  if(exists("res.source") & exists("res.reldist") & exists("res.group") & exists("res.case") & exists("res.set")) {
    res.source <- c(res.source, source)
    res.reldist <- c(res.reldist, reldist)
    res.group <- c(res.group, group)
    res.case <- c(res.case, case)
    res.set <- c(res.set, set)
    res.lengthA <- c(res.lengthA, lengthA)
    res.lengthB <- c(res.lengthB, lengthB)
    res.lengthTrace <- c(res.lengthTrace, lengthTrace)
  }
  else {
    res.source <- source
    res.reldist <- reldist
    res.group <- group
    res.case <- case
    res.set <- set
    res.lengthA <- lengthA
    res.lengthB <- lengthB
    res.lengthTrace <- lengthTrace
  }
}

# Get every relative distance with case, group, set & source into one dataframe
everything <- data.frame(case = res.case, group = res.group, set = res.set, source = res.source, reldist = res.reldist, lengthA = 
                           res.lengthA, lengthB = res.lengthB, lengthTrace = res.lengthTrace)

# DO THE THING!
# Loop through groups and sets, perform a wilcox test for every case
for (s in 1:14) {
  
  # Before testing the data, produce some nice fancy boxplots
  setdata <- everything[everything$set == s, ]
  setdata$group <- factor(setdata$group, groups)
  setboxplot <- ggplot(setdata[setdata$source == 2, ], aes(x = group, y = reldist)) + geom_boxplot(outlier.shape = 1) +
    stat_summary(data = setdata[setdata$source == 1, ], shape=18, size=0.8, na.rm = TRUE) +
    labs(x = "Groups", y = "Relative Distance", title = paste("Original alignment vs. surrogate data —", sets[s], sep = " "))
  setboxplot + scale_fill_grey() + theme_bw()
  
  # Save plot
  ggsave(paste("mainOutput\\boxplot_",sets[s],".pdf",sep=""), width = 15, height = 10, units = "cm")
  
  for (gr in 1:7) {
    # Select subset by group & set
    testdata <- everything[everything$group == gr & everything$set == s, ]
    # Perform Wilcox test
    model <- wilcox.test(reldist ~ source, data = testdata)
    
    # Get p-value and effect size
    p <- model$p.value
    w <- model$statistic
    r <- rFromWilcox(model, 25010)
    mdnOrig <- median(testdata[testdata$source == 1, ]$reldist)
    mdnRand <- median(testdata[testdata$source == 2, ]$reldist)
    
    if(exists("res.p") & exists("res.w")) {
      res.bla_group <- c(res.bla_group, groups[gr])
      res.bla_set <- c(res.bla_set, sets[s])
      res.p <- c(res.p, p)
      res.w <- c(res.w, w)
      res.r <-c(res.r, r)
      res.mdOrig <- c(res.mdOrig, mdnOrig)
      res.mdRand <- c(res.mdRand, mdnRand)
    }
    else {
      res.bla_group <- groups[gr]
      res.bla_set <- sets[s]
      res.p <- p
      res.w <- w
      res.r <- r
      res.mdOrig <- mdnOrig
      res.mdRand <- mdnRand
    }
  }
}

res.bla_group <- factor(res.bla_group, groups)
res.bla_set <- factor(res.bla_set, sets)

wilcoxRes <- data.frame(Group = res.bla_group, Set = res.bla_set, P = res.p, W = res.w, R = res.r, mdOrig = res.mdOrig, mdRand = res.mdRand)

write.csv2(wilcoxRes, "mainOutput\\wilcoxResults.csv", row.names = FALSE)

wilcoxPs <- wilcoxRes[, c("Group","Set","P")]
wilcoxPs_wide <- cast(wilcoxPs, Group ~ Set, value="P")
write.csv2(wilcoxPs_wide, "mainOutput\\wilcoxPs.csv", row.names = FALSE)

# At last, write everything beautifully in a file
everything$set <- factor(everything$set, levels = c(1:14), labels=sets)
everything$group <- factor(everything$group, groups)
everything$source <- factor(everything$source, levels=c(1,2), labels = c("original","randomized"))
everything$case <- factor(everything$case, levels = c(1:70), labels = c(1:70))
write.csv2(everything, "mainOutput\\everything.csv", row.names = FALSE)
