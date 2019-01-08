# Include packages
library(pastecs)
library(ggplot2)

# Load data

# Loop: for(i in 1:10){x=x+1; print(x);}
# bla <- read.csv(filedir)

# Get rid of original alignment results

# Calculate relative distance and number of changes

# Histogram with fitted normality distribution (ggplot2)
hist.dist <- ggplot(test_Data, aes(Dist.Length)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.004, color = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(test_Data$Dist.Length), sd = sd(test_Data$Dist.Length)), size = 1) + 
  labs(y = "Density", x = "Distance (per Alignment Length)")
hist.dist + theme_bw()

# Q-Q plot with line (ggplot2)
p <- ggplot(test_Data, aes(sample = Dist.Length))
p + stat_qq() + stat_qq_line()

# pastecs::stat.desc() mit norm = TRUE return beneath a lot of other things skewness, kurtosis and shapiro-wilk test
stat.desc(data$Dist.Length, norm = TRUE)

# Save results