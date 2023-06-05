##
poisson_cumulative_probability <- function(moi, k) {
  cumulative_probability <- 0
  for (i in 0:k) {
    cumulative_probability <- cumulative_probability + (exp(-moi) * moi^i) / factorial(i)
  }
  return(cumulative_probability)
}
###
find_moi_for_success_rate <- function(success_rate) {
  moi <- 1
  cumulative_probability <- 0
  while (cumulative_probability < success_rate) {
    cumulative_probability <- poisson_cumulative_probability(moi, moi)
    cat("Number of successes:", moi, "Cumulative Probability:", cumulative_probability, "\n")
    moi <- moi + 1
  }
  return(moi - 1)
}

success_rate <- 0.7
moi_needed <- find_moi_for_success_rate(success_rate)
cat("MOI needed for an 76% success rate:", moi_needed)


# Function to calculate the Poisson probabilities
poisson_probabilities <- function(moi, k) {
  lambda <- moi  # Average rate of occurrence
  probabilities <- dpois(k, lambda)  # Calculate Poisson probabilities
  return(probabilities)
}


# Calculate and print the probabilities for a range of MOI values
moi_values <- c(5, 10, 15)  # Example MOI values
k <- 20  # Maximum number of successes

for (moi in moi_values) {
  cat("MOI =", moi, "\n")
  probabilities <- poisson_probabilities(moi, k)
  for (i in 0:k) {
    cat("P(X =", i, ") =", probabilities[i+1], "\n")
  }
  cat("\n")
}

?sample_random()
