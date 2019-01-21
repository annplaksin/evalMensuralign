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
  
  if(exists("res.source") & exists("res.reldist") & exists("res.group") & exists("res.case") & exists("res.set")) {
    res.source <- c(res.source, source)
    res.reldist <- c(res.reldist, reldist)
    res.group <- c(res.group, group)
    res.case <- c(res.case, case)
    res.set <- c(res.set, set)
  }
  else {
    res.source <- source
    res.reldist <- reldist
    res.group <- group
    res.case <- case
    res.set <- set
  }
}

# Get every relative distance with case, group, set & source into one dataframe
everything <- data.frame(case = res.case, group = res.group, set = res.set, source = res.source, reldist = res.reldist)

# DO THE THING!
# Loop through groups and sets, perform a wilcox test for every case
for (gr in 1:7) {
  for (s in 1:14) {
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
    
    resultPerGroup <- cbind(p,w,r,mdnOrig,mdnRand)
    print(resultPerGroup)
  }
}



