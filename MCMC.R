
library("gtools")  # for rdirichlet
library("tidyverse"); theme_set(theme_minimal())
library("mcmcr")

# returns log likelihood
model_likelihood <- function(y, rho) {
  y <- as.list(y)
  n <- 3 #3 throws
  
  # enumerate all valid combinations
  combos <- expand.grid(x1 = c(0:n), x2 = c(0:n), x3 = c(0:n), x5 = c(0:n)) %>%
    mutate(sum_x1235 = x1 + x2 + x3 + x5) %>%
    filter(sum_x1235 <= n) %>%
    mutate(
      x0 = n - sum_x1235,
      y = x1 + 2*x2 + 3*x3 + 5*x5) %>%
    select(x0, x1, x2, x3, x5, y)
  
  # compute multinomial probability for each row
  combos$prob <- pmap_dbl(combos[, c("x0", "x1", "x2", "x3", "x5")],
                         function(x0, x1, x2, x3, x5) dmultinom(c(x0, x1, x2, x3, x5), prob = rho))
  
  # aggregate by y values
  prob_df <- combos %>% group_by(y) %>%
    summarize(prob = sum(prob), .groups = "drop")
  
  # return probabilities for requested y values
  log_likelihood <- set_names(y, NULL) %>%
    map_dbl(~(prob_df$prob[prob_df$y == .x] %||% 0) %>% log()) %>%
    sum()
  
  return(log_likelihood)
}

# example
# model_likelihood(y = c(4, 5), p = c(.4, .1, .2, .2, .1))

# dirichlet random walk proposal
propose_rho <- function(current_rho, epsilon = .1){
  alpha <- current_rho/epsilon
  rgamma_sample <- rgamma(length(current_rho), shape = alpha, rate = 1)
  rgamma_sample/sum(rgamma_sample)
}

mh_sampler <- function(y, alpha = c(60, 20, 9, 9, 2), burn_in = 1000, n_iter = 5000, thin = 2,
                       proposal_epsilon = .005, target_accept_rate = .40, adapt_change = 50){
  
  p <- length(alpha)
  samples <- ceiling((n_iter - burn_in)/thin)
  
  rho_mat <- matrix(NA, nrow = samples, ncol = p)
  acc_rate <- numeric(samples)
  new_y <- numeric(samples)
  
  accept_count <- 0
  
  rho_current <- rdirichlet(1, alpha)
  lik_current <- model_likelihood(y, rho_current)
  
  for (i in 1:n_iter) {
      proposal <- rho_current %>% propose_rho(proposal_epsilon)
      
      lik_proposal <- model_likelihood(y, proposal)
      
      prior_current <- ddirichlet(rho_current, alpha) %>% log()
      prior_proposal <- ddirichlet(proposal, alpha) %>% log()
      
      r <- (lik_proposal + prior_proposal) - (lik_current + prior_current)
      
      if (runif(1) < min(1, exp(r))) {
        rho_current <- proposal
        lik_current <- lik_proposal
        acc <- TRUE
      }else{
        acc <- FALSE
      }
      accept_count <- accept_count + acc
    
    # adjust proposal_change every adapt_change steps
    if(i %% adapt_change == 0 & i <= burn_in){
        if(accept_count/adapt_change > target_accept_rate + .05){
          proposal_epsilon <- proposal_epsilon * runif(1, 1, 1.2)
        }else if(accept_count/adapt_change < target_accept_rate - .05){
          proposal_epsilon <- proposal_epsilon / runif(1, 1, 1.2)
        }
      accept_count <- 0
    }
    
    if(i > burn_in & (i - burn_in - 1) %% thin == 0){ #keep
      id <- (i - burn_in - 1) / thin + 1
      rho_mat[id, ] <- rho_current
      acc_rate[id] <- acc
      
      # compute new Ys
      new_y[id] <- sum(rmultinom(1, 3, rho_current)*c(0, 1, 2, 3, 5))
    }
  }
  colnames(rho_mat) <- paste0("p", c(0:3, 5))
  
  list("rho" = rho_mat, "acc_rate" = acc_rate, "new_y" = new_y)
}

# ex_samples <- mh_sampler(y = c(2, 1, 1, 6, 3, 0, 8, 0), burn_in = 5, n_iter = 10, thin = 4, adapt_change = 2)
# 
# ex_samples[[1]] %>% summary()
# 
# ex_samples[[1]] %>% plot()
# 
# ex_samples[[2]]
