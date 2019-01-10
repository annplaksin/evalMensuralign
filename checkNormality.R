# Include packages
library(pastecs)
library(ggplot2)

# Collect stat.desc() results in this data frame
 results <- data.frame(size=numeric(),set=numeric(),params=numeric(),examp=numeric(),
                      dist.kurt=numeric(),dist.skew=numeric(),dist.normp=numeric(),
                      reldist.kurt=numeric(),reldist.skew=numeric(),reldist.normp=numeric(), stringsAsFactors = FALSE)

# Sample sizes
size <- c(400, 900, 1600, 2500)

# File set
set <- c("clean", "withoutSB")

# Parameter sets
parameters <- c("Signbased.vis", "Signbased.log", "Signbased.all", "Superlogical", 
                "Signbased.vis.gap", "Signbased.log.gap", "Signbased.all.gap", "Superlogical.gap")

# Looping various times to process all the data

# Sample size loop
for (si in 1:4) 
{
  # File set loop
  for (se in 1:1) 
  {
    # Set i according to number of examples
    for(i in 1:5)
    {
      # Looping trhough parameters
      for(p in 1:8)
      {
        # Set filenames
        input <- paste("normalityInput\\", size[si], "\\", set[se], "\\", i, "_", parameters[p], ".csv", sep = "")
        
        outQQ_relDist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-relDist-QQ.pdf", sep = "")
        outHist_relDist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-relDist-Hist.pdf", sep = "")
        outStat_relDist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-relDist-Stat.txt", sep = "")
        
        outQQ_dist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-dist-QQ.pdf", sep = "")
        outHist_dist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-dist-Hist.pdf", sep = "")
        outStat_dist <- paste("normalityOutput\\", size[si], "_", set[se], "_", i, "_", parameters[p], "-dist-Stat.txt", sep = "")
        
        # Load data
        data <- read.csv(input, header = TRUE, sep = ";", dec = ",")
        
        # Get rid of original alignment results
        randoms <- subset(data, Source=="randomized")
        
        # Check if rows fit to sample size
        stopifnot(nrow(randoms) == size[si])
        
        # Calculate relative distance and number of changes
        randoms$dist.length <- randoms$Distance / randoms$LengthBacktrace
        randoms$changes.length <- randoms$Number.of.Changes / randoms$LengthBacktrace
        
        # Histogram with fitted normality distribution (ggplot2)
        hist.reldist <- ggplot(randoms, aes(dist.length)) + 
          geom_histogram(aes(y= ..density..), binwidth = 0.004, color = "black", fill = "white") +
          stat_function(fun = dnorm, args = list(mean = mean(randoms$dist.length), 
                                                 sd = sd(randoms$dist.length)), size = 1) + 
          labs(y = "Density", x = "Distance (per Alignment Length)")
        hist.reldist + theme_bw()
        ggsave(outHist_relDist, width = 15, height = 10, units = "cm")
        
        # For Distance as well
        hist.dist <- ggplot(randoms, aes(Distance)) + 
          geom_histogram(aes(y= ..density..), binwidth = 1, color = "black", fill = "white") +
          stat_function(fun = dnorm, args = list(mean = mean(randoms$Distance), 
                                                 sd = sd(randoms$Distance)), size = 1) + 
          labs(y = "Density", x = "Distance")
        hist.dist + theme_bw()
        ggsave(outHist_dist, width = 15, height = 10, units = "cm")
        
        # Q-Q plot with line (ggplot2)
        p.reldist <- ggplot(randoms, aes(sample = dist.length))
        p.reldist + stat_qq() + stat_qq_line() + theme_bw()
        ggsave(outQQ_relDist, width = 15, height = 10, units = "cm")
        
        # For distance as well
        p.dist <- ggplot(randoms, aes(sample = dist.length))
        p.dist + stat_qq() + stat_qq_line() + theme_bw()
        ggsave(outQQ_dist, width = 15, height = 10, units = "cm")
        
        # pastecs::stat.desc() mit norm = TRUE return beneath a lot of other things skewness, 
        # kurtosis and shapiro-wilk test
        stat.reldist <- stat.desc(randoms$dist.length, norm = TRUE)
        stat.dist <- stat.desc(randoms$Distance, norm = TRUE)
        
        # Save results to file
        write.table(stat.reldist, outStat_relDist, sep = "\t", row.names = TRUE)
        write.table(stat.dist, outStat_dist, sep = "\t", row.names = TRUE)
        
        
        # Put everything in results
        results <- rbind(results, list(size=size[si],set=se,params=p,examp=i,
                   dist.kurt=stat.dist["kurt.2SE"],dist.skew=stat.dist["skew.2SE"],dist.normp=stat.dist["normtest.p"],
                   reldist.kurt=stat.reldist["kurt.2SE"],reldist.skew=stat.reldist["skew.2SE"],reldist.normp=stat.reldist["normtest.p"]))
        print(parameter)
      }
    }
  }
}

factor(results$set, levels = c(1:2), labels = set)
factor(results$params, levels = c(1:8), labels = parameters)

write.csv2(results, "normalityOutput\\results.csv", row.names = FALSE)
