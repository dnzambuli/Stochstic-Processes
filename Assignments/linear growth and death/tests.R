set.seed(123)  # For reproducibility

# Parameters
lambda <- 1.8  # Birth rate
mu <- 0.7  # Death rate
X0 <- 1  # Initial state
t_max <- 30  # Total observation time
t_target <- 5  # Target time for extinction calculation
num_realizations <- 300  # Number of realizations

# Simulation function for the birth-death process
simulate_birth_death <- function(X0, lambda, mu, t_target) {
  X_t <- X0  # Current state
  t <- 0  # Current time
  
  while (X_t > 0 && t <= t_target) {
    # Total rate of events
    rate_total <- lambda * X_t + mu * X_t
    # Time until next event
    t_step <- rexp(1, rate_total)
    t <- t + t_step  # Update time
    
    if (t <= t_target && X_t > 0) {
      # Decide whether birth or death
      if (runif(1) < lambda * X_t / rate_total) {
        X_t <- X_t + 1  # Birth
      } else {
        X_t <- X_t - 1  # Death
      }
    }
  }
  
  return(X_t == 0)  # Return TRUE if extinction occurred, FALSE otherwise
}

# Running the simulation and counting extinction events by t_target
extinctions_by_t5 <- sum(replicate(num_realizations, simulate_birth_death(X0, lambda, mu, t_target)))

# Calculate the probability of extinction by or at t = 5
probability_extinction_by_t5 <- extinctions_by_t5 / num_realizations

# Output the probability
probability_extinction_by_t5
