#Includes
library(reshape)

# Cases
cases <- c(1:70)

# Groups
groups <- c(rep(1,140),rep(2,140),rep(3,140),rep(4,140),rep(5,140),rep(6,140),rep(7,140))

# Sets
sets <- c("Signbased.all", "Signbased.all.gap", "Signbased.log", "Signbased.log.gap", "Signbased.vis", "Signbased.vis.gap", 
          "Superlogical", "Superlogical.gap",
          "withoutSB.all", "withoutSB.all.gap", "withoutSB.log", "withoutSB.log.gap", "withoutSB.vis", "withoutSB.vis.gap")


# Now, start working through the files

# Loop through cases
for (c in 1:70) {
  # Loop through sets
  for (s in 1:14) {
    # Generate filename
    input <- paste("mainInput\\",cases[c],"_",sets[s],".csv", sep="")
    
    # Open file
    data <- read.csv(input, header = TRUE, sep = ";", dec = ",")
    
    # Calculate relative distance
    data$RelDist <- data$Distance / data$LengthBacktrace
    
    # Get randoms and original
    randoms <- subset(data, Source=="randomized")
    original <- subset(data, Source=="original")
    
    # Calculate Z-Score
    zScore <- (original$RelDist - mean(randoms$RelDist))/ sd(randoms$RelDist)
    
    # Test normality of randoms
    normality <- shapiro.test(randoms$RelDist)
    
    if (exists("res.Case") & exists("res.Set") & exists("res.Z") & exists("res.Norm")) {
      res.Case <- c(res.Case,c)
      res.Set <- c(res.Set,s)
      res.Z <- c(res.Z,zScore)
      res.Norm <- c(res.Norm, normality[["p.value"]])
    }
    else{
      res.Case <- c
      res.Set <- s
      res.Z <- zScore
      res.Norm <- normality[["p.value"]]
    }
    
  }
}

# Write two result files... one for the normality tests and one for the z scores
res.Case <- factor(res.Case, levels = 1:70)
res.Set <-factor(res.Set, levels = c(1:14), labels = sets)
groups <- factor(groups, levels = c(1:7))

dataCases <- data.frame(Group=groups,Case=res.Case,Set=res.Set)

zResults <- data.frame(Group=groups,Case=res.Case,Set=res.Set,Z=res.Z)

normResults <- data.frame(Group=groups,Case=res.Case,Set=res.Set,Norm.p=res.Norm)

# Reshape data to wode format
zResults_wide <- cast(zResults, Group + Case ~ Set, value="Z")

normResults_wide <- cast(normResults, Group + Case ~ Set, value="Norm.p")

# Write results to file
write.csv2(zResults_wide, "mainOutput\\resultZscores.csv", row.names = FALSE)
write.csv2(normResults_wide, "mainOutput\\resultNormScores.csv", row.names = FALSE)

write.csv2(dataCases, "mainOutput\\cases.csv", row.names = FALSE)
