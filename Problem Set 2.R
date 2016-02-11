## Jacob Hample
## Professor Montgomery
## Applied Statistical Programming
## February 11, 2016


## Problem Set 2 ##

## Question 1

#Creates a vector and matrix with identical data
test.vector <- c(0.1, 0.08, 0.14, 0.16, 0.09, 0.13, 0.12, 0.07, 0.11)

test.matrix <- matrix(
  c(0.1, 0.08, 0.14, 0.16, 0.09, 0.13, 0.12, 0.07, 0.11),
  nrow = 1,
  ncol = 9
)

# Calculates Leemis' m Statistic
MStat <- function (freq) {
  i <- 1:9
  m <- max(freq - log10(1 + (1 / i)))
  return (m)
}

# Calculates Cho-Gains' d Statistic
DStat <- function (freq) {
  i <- 1:9
  d <- sqrt(sum(freq - log10(1 + (1 / i)))^2)
  return (d)
}

# Takes in sample data from either a vector or matrix and displays the
# m statistic, d statistic, or both depending on user input
CalculatingViolations <- function (data, option=c("m","d","both")) {
  if (option == "m") {
    return (list(MStat(data), data))
  }
  if (option == "d") {
    return (list(DStat(data), data))
  }
  if (option == "both") {
    return (list("m" = MStat(data), "d" = DStat(data), "Data" = data))
  }
}

# Runs test.matrix through CaluclatingViolations, showing both the m
# and d statistic
CalculatingViolations(test.matrix, "both")


## Question 2

# Sets the parameters for the significance of a hyothesis test of the m or d
# statistic with a null hypothesis of 'no fraud'
SigLevel <- function (data) {
  if (MStat(data) > 0.851 & MStat(data) < 0.967) {
    return ("*")
  }
  if (DStat(data) > 1.212 & DStat(data) < 1.330) {
    return ("*")
  }
  if (MStat(data) >= 0.967 & MStat(data) < 1.212) {
    return ("**")
  }
  if (DStat(data) >= 1.330 & DStat(data) < 1.569) {
    return ("**")
  }
  if (MStat(data) >= 1.212) {
    return ("***")
  }
  if (DStat(data) >= 1.569) {
    return ("***")
  }
  else {
    return ("Insignificant")
  }
}

# Creates a table that shows the statistic, its significance, and a legend
# explaining the different significance codes
print.benfords <- function (data) {
  m.stat <- c("    Leemis' m = ", round(MStat(data), 2), SigLevel(data))
  d.stat <- c(" Cho-Gains' d = ", round(DStat(data), 2), SigLevel(data))
  sig.codes <- c("Signif. codes:","", "0.01 ‘***’ 0.05 ‘**’ 0.1 ‘*’")
  table <- rbind(m.stat, d.stat, sig.codes)
  return (table)
}

# Runs test.matrix through print.benfords
print.benfords(test.matrix)

# Sets directory
setwd("/Users/Jacob/Google Drive/Senior Year/Spring 2016/Statistical Programming/Problem Sets")

# Creates a csv file and diverts the output of print.benfords() into it
FileSink <- function (data) {
  sink(file = "BenfordsTable.csv")
  print.table(print.benfords(data))
  sink()
}

# Creates a csv file of the print.benfords table using the data from test.matrix
FileSink(test.matrix)
