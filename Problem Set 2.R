## Jacob Hample
## Professor Montgomery
## Applied Statistical Programming
## February 11, 2016


## Problem Set 2 ##

## Question 1

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

# Calculates Cho Gains' d
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
    return (list("m" = MStat(data), "d" = DStat(data), data))
  }
}

CalculatingViolations(test.matrix, "both")


# Question 2

SigLevel <- function (data) {
  if (MStat(data) > 0.851 & MStat < 0.967) {
    return ("*")
  }
  if (DStat(data) > 1.212 & DStat < 1.330) {
    return ("*")
  }
  if (MStat(data) >= 0.967 & MStat < 1.212) {
    return ("**")
  }
  if (DStat(data) >= 1.330 & DStat < 1.569) {
    return ("**")
  }
  if (MStat(data) >= 1.212) {
    return ("***")
  }
  if (DStat(data) >= 1.569) {
    return ("***")
  }
}

print.benfords <- function (data) {
  m.stat <- c("   Leemis' m = ", MStat(data) + SigLevel(data))
  d.stat <- c("Cho-Gains' d = ", DStat(data) + SigLevel(data))
  significance <- c("Signif. codes:", "0.01 ‘***’ 0.05 ‘**’ 0.1 ‘*’")
  table <- rbind(m.stat, d.stat, significance)
  return (table)
}

print.benfords(test.matrix)
