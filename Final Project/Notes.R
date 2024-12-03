distributions <- c("Uniform", "Exponential", "T")

# Define sample sizes and number of simulations
sample_sizes <- c(5, 10, 20, 50, 100, 200, 500, 1000, 10000)
n_simulations <- 10000
alpha <- 0.05


get_data <- function(distribution, N){
  if (distribution == "Uniform"){
    return(runif(N, min= -1, max = 1))
  }
  if (distribution == "Exponential"){
    return(rexp(N, rate = 1) - 1)
  }
  if (distribution == "T"){
    return(rt(N, df = 2))
  }
}

results <- list()

library(parallelly)
# Simulation loop
for (distribution in distributions) {
  type1_errors <- rep(NA, length(sample_sizes)) 
  
  # Loop over sample sizes
  for (i in seq_along(sample_sizes)) {
    N <- sample_sizes[i] 
    
    p_values <- unlist(mclapply(1:n_simulations, function(sim) {
      sample_data <- get_data(distribution, N) # Pass the correct sample size to get_data
      t_test <- t.test(sample_data, mu = 0)    # Perform t-test
      t_test$p.value}))                     # Extract p-value 
    type1_error_rate <- mean(p_values < alpha)
    type1_errors[i] <- type1_error_rate
  }
  
  # Store results for the current distribution
  results[[distribution]] <- data.frame(
    SampleSize = sample_sizes,
    TypeIErrorRate = type1_errors
  )
}

# Print results
for (dist_name in names(results)) {
  cat("\n", dist_name, "Distribution\n")
  print(results[[dist_name]])
}










