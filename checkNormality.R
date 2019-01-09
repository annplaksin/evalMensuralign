# Include packages
library(pastecs)
library(ggplot2)

# Try looping to process multiple data files
for(i in 1:1)
{
  # Set filenames
  input <- paste("normalityInput\\", i, ".csv", sep = "")
  outQQ <- paste("normalityOutput\\", i, "-QQ.pdf", sep = "")
  outHist <- paste("normalityOutput\\", i, "-Hist.pdf", sep = "")
  outStat <- paste("normalityOutput\\", i, "-Stat.txt", sep = "")
  
  # Load data
  data <- read.csv(input, header = TRUE, sep = ";", dec = ",")
  
  # Get rid of original alignment results
  randoms <- subset(data, Source=="randomized")
  
  # Calculate relative distance and number of changes
  randoms$dist.length <- randoms$Distance / randoms$LengthBacktrace
  randoms$changes.length <- randoms$Number.of.Changes / randoms$LengthBacktrace
  
  # Histogram with fitted normality distribution (ggplot2)
  hist.dist <- ggplot(randoms, aes(dist.length)) + 
    geom_histogram(aes(y= ..density..), binwidth = 0.004, color = "black", fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(randoms$dist.length), 
                                           sd = sd(randoms$dist.length)), size = 1) + 
    labs(y = "Density", x = "Distance (per Alignment Length)")
  hist.dist + theme_bw()
  ggsave(outHist, width = 15, height = 10, units = "cm")
  
  # Q-Q plot with line (ggplot2)
  p <- ggplot(randoms, aes(sample = dist.length))
  p + stat_qq() + stat_qq_line() + theme_bw()
  ggsave(outQQ, width = 15, height = 10, units = "cm")
  
  # pastecs::stat.desc() mit norm = TRUE return beneath a lot of other things skewness, 
  # kurtosis and shapiro-wilk test
  stat <- stat.desc(randoms$dist.length, norm = TRUE)
  
  # Save results to file
  write.table(stat, outStat, sep = "\t", row.names = TRUE)
 }

