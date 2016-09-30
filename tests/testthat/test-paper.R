rm(list = ls())
library(testthat)
library(DeclareDesign)
library(Matching)

context("Checking Code in Paper Works")

# “Characterizing Research Designs in Code" -------------------------------

test_that("section on 'Characterizing Research Designs in Code' works", {
  
  
  my_population <- function(size) {
    data.frame(u = rnorm(size))
  }
  population <- declare_population(custom_population_function = my_population, size = 100)
  
  my_sampling <- function(data) {
    rbinom(n = nrow(data),
           size = 1,
           prob = 0.1)
  }
  sampling <- declare_sampling(sampling_function = my_sampling)
  
  my_assignment <- function(data) {
    rbinom(n = nrow(data),
           size = 1,
           prob = 0.5)
  }
  assignment <-
    declare_assignment(assignment_function = my_assignment,
                       condition_names = c(0, 1))
  
  my_potential_outcomes <-
    function(data) {
      with(data, Z * 0.25 + u)
    }
  potential_outcomes <- declare_potential_outcomes(
    potential_outcomes_function = my_potential_outcomes,
    outcome_variable_name = 'Y',
    condition_names = c(0, 1)
  )
  
  my_estimand <- function(data) {
    with(data, mean(Y_Z_1 - Y_Z_0))
  }
  estimand <- declare_estimand(estimand_function = my_estimand,
                               potential_outcomes = potential_outcomes)
  
  my_estimates <- function(data) {
    reg <- lm(Y ~ Z, data = data)
    phi <- as.list(summary(reg)$coefficients["Z",])
    c(
      est = phi$Estimate,
      se = phi$"Std. Error",
      p = phi$"Pr(>|t|)"
    )
  }
  estimator <-
    declare_estimator(estimates = my_estimates, estimand = estimand)
  
  bias_diagnosand <- "est - estimand"
  
  diagnosand <-
    declare_diagnosand(
      diagnostic_statistic_text = bias_diagnosand,
      summary_function = mean
    )
  
  design <-
    declare_design(
      population = population,
      sampling = sampling,
      assignment = assignment,
      potential_outcomes = potential_outcomes,
      estimator = estimator,
      diagnosand = diagnosand
    )
  
  
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
  
})

# "Learning About Designs Through Diagnosis" ------------------------------

test_that("section on 'Learning About Designs Through Diagnosis' works", {
  
  get_template(template_name = "spillovers")
  get_template(template_name = "factorial")
  get_template(template_name = "covariates")
  
  expect_message(
    heterogeneous_designs <- quick_design(template = make_heterogeneous_fx_design,
                                          n = 20,
                                          g = vary(0, .3, .6, .9))
  )
  
  compare_designs(design = heterogeneous_designs)
  
  expect_message(
    spillover_designs <- quick_design(
      template = make_spillover_design,
      buffer = vary(0, 3, 4),
      dist_effect = vary(0, 1, 3),
      intersect = TRUE
    )
  )
  
  expect_warning(
    compare_designs(design = spillover_designs)
  )
  
  expect_message(
    factorial_designs <- quick_design(
      template = make_factorial_design,
      assignment_strategy = vary("two_by_two", "three_arm"),
      interaction_coefficient = vary(0.00, 0.05, 0.10, 0.15, 0.2,
                                     0.25, 0.30, 0.35, 0.4)
    )
  )
  
  compare_designs(design = factorial_designs)
  
})

# "Illustration of Sampling Decisions: Handling Spatial Spillovers" -------

test_that(
  "section 'Illustration of Sampling Decisions: Handling Spatial Spillovers' of paper works",
  {
    dist_effect <- 2
    buffer <- 0
    spillover <- 0
    
    spatial_data <-
      expand.grid(x = 1:6, y = 1:6)
    spatial_data$id <- 1:nrow(spatial_data)
    
    # Calculate distance of each point to each other point
    distmat <- with(spatial_data, {
      sapply(
        X = 1:length(x),
        FUN = function(i) {
          sapply(
            X = 1:length(x),
            FUN = function(j) {
              sqrt((x[i] - x[j]) ^ 2 + (y[i] - y[j]) ^ 2)
            }
          )
        }
      )
    })
    
    diag(distmat) <- 0
    
    assign(x = "distmat",
           value = distmat,
           envir = globalenv())
    
    # Generate all possible pairs
    pairs <- t(with(spatial_data, combn(id, 2)))
    
    # Generate distances of pairs
    dists <-
      distmat[pairs]
    
    # Generate indicator for all samples that meet the big and small buffers
    pairs <-
      data.frame(unit_1 = pairs[, 1], unit_2 = pairs[, 2])
    
    pairs$outside <- dists > buffer
    
    
    spatial_data$buffer_prob <- with(subset(pairs, subset = outside),
                                     sapply(
                                       X = spatial_data$id,
                                       FUN = function(unit_id)
                                         mean(c(unit_1, unit_2) %in% unit_id)
                                     ))
    
    spatial_data$total_dist <- spatial_data$y ^ 2 / 10
    
    
    population <- declare_population(
      noise = "rnorm(n_)",
      buffer_prob = "spatial_data$buffer_prob",
      distance = "spatial_data$total_dist",
      size = 36,
      options = list(spatial_data = spatial_data,
                     distmat = distmat),
      super_population = TRUE
    )
    
    sample_and_assign <- function(data) {
      sample_probs <- data$buffer_prob
      ids <- 1:length(sample_probs)
      to_sample <-
        sample(
          x = ids,
          size = 2,
          replace = FALSE,
          prob = sample_probs
        )
      to_treat <- sample(x = to_sample, size = 1)
      sampled <- ids %in% to_sample
      treated <- ids %in% to_treat
      return(sampled + treated)
    }
    
    
    assignment <- declare_assignment(condition_names = c(0, 1, 2),
                                     custom_assignment_function = sample_and_assign)
    
    interference <- declare_potential_outcomes(
      formula = E ~ (distmat %*% I(Z == 2) < 4) * I(Z != 2),
      condition_names = c(0, 1, 2)
    )
    
    PO_formula <-
      as.formula(paste0(
        "Y ~ 1*I(Z==2) + E_Z_2 *",
        spillover,
        " + noise + distance *",
        dist_effect
      ))
    
    potential_outcomes <- declare_potential_outcomes(
      formula = PO_formula,
      condition_names = c(0, 1, 2),
      assignment_variable_name = "Z"
    )
    
    sampling <- declare_sampling(sampling = FALSE)
    
    estimand <- declare_estimand(
      estimand_text = "1",
      potential_outcomes = potential_outcomes,
      fixed = TRUE,
      label = "fixed_estimand"
    )
    
    IPW_est <- function(data) {
      probs <- data$buffer_prob
      treated <- data$Z == 2
      sampled <- data$Z > 0
      weights <- 1 / probs
      
      lm_fit <- lm(
        formula = Y ~ (Z == 2),
        data = data,
        subset = Z > 0,
        weights = weights
      )
      
      lm_fit$df.residual <- 1
      
      return(lm_fit)
      
    }
    
    
    estimator <- declare_estimator(
      model = IPW_est,
      estimates = get_regression_coefficient,
      coefficient_name = "Z == 2TRUE",
      estimand = estimand
    )
    
    
    
    design <- declare_design(
      population = population,
      sampling = sampling,
      assignment = assignment,
      estimator = estimator,
      potential_outcomes = list(interference, potential_outcomes),
      label = "Spillover Buffer Design"
    )
    
    expect_warning(diagnose_design(
      design = design,
      population_draws = 10,
      sample_draws = 1,
      assignment_draws = 1,
      bootstrap_diagnosands = F
    ))
    
  }
)



# Illustration of Analysis Decisions: Gains from Covariate Control --------

test_that("section 'Illustration of Analysis Decisions: Gains from Covariate Control' works",{
  N <- 24
  n <- 24
  m <- floor(n / 2)
  sdev <- 1
  b <- 1
  f <- 0
  g <- 0
  
  if (f < 0)
    stop("f non negative please for this illustration")
  if (f + g > 1)
    stop("f + g < 1 please")
  
  
  my_population    <- declare_population(
    e    = declare_variable(type = "normal", location_scale = c(0, sdev)),
    XT   = declare_variable(type = "normal", location_scale = c(0, sdev)),
    XM   = "sign(XT)*XT^2  - mean(sign(XT)*XT^2)",
    # Misspecification
    size = N,
    super_population = TRUE
  )
  
  my_potential_outcomes <-
    declare_potential_outcomes(formula = as.formula(
      paste0(
        "Y ~ Z*",
        b,
        "+Z*XT*",
        f,
        "+XT*",
        g,
        "+Z*e*",
        (1 - (f + g) ^ 2) ^ .5,
        "+(1-Z)*e*",
        (1 - g ^ 2) ^ .5
      )
    ),
    condition_names = c(0, 1))
  
  if (n == N)
    my_sampling <- declare_sampling(sampling = FALSE)
  if (n != N)
    my_sampling <- declare_sampling(n = n)
  
  my_estimand <-
    declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)",
                     potential_outcomes = my_potential_outcomes)
  
  my_estimand0 <-
    declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_1)",
                     potential_outcomes = my_potential_outcomes)
  
  my_assignment   <- declare_assignment(m = m,
                                        potential_outcomes = my_potential_outcomes)
  
  M1 <- declare_estimator(
    formula           = Y ~ Z,
    model             = lm,
    estimates         = get_regression_coefficient,
    coefficient_name = "Z",
    estimand          = my_estimand,
    labels            = "No Controls"
  )
  
  M2 <- declare_estimator(
    formula           = Y ~ Z + XM,
    model             = lm,
    estimates         = get_regression_coefficient,
    coefficient_name = "Z",
    estimand          = my_estimand,
    labels            = "Controls"
  )
  
  M3 <- declare_estimator(
    formula           = Y ~ Z + XM + Z:XM,
    model             = lm,
    estimates         = get_regression_coefficient,
    coefficient_name = "Z",
    estimand          = my_estimand,
    labels            = "Controls and Interaction"
  )
  
  B <- declare_estimator(
    formula           = XM ~ Z,
    model             = lm,
    estimates         = get_regression_coefficient,
    coefficient_name = "Z",
    estimand          = my_estimand0,
    labels            = "Balance check"
  )
  
  my_design <- declare_design(
    population         = my_population,
    potential_outcomes = my_potential_outcomes,
    sampling           = my_sampling,
    assignment         = my_assignment,
    estimator          = list(M1, M2, M3, B)
  )
  
  diagnose_design(
    design = my_design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
  
  
})


# Illustration of Assignment Decisions: Assigning Multiple Treatme --------

test_that("appendix for 'Assigning Multiple Treatments' works", {
  
  # Factorial design with no interaction estimand
  
  interaction_coefficient <- 0
  n <- 500
  
  population <-
    declare_population(noise = "rnorm(n_)", size = 10000)
  sampling <- declare_sampling(n = n)
  
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ .5 * Z1 + .5 * Z2 + interaction_coefficient * Z1 * Z2 + noise,
      condition_names = list(Z1 = c(0, 1),
                             Z2 = c(0, 1)),
      assignment_variable_name = c("Z1", "Z2"),
      interaction_coefficient = interaction_coefficient
    )
  
  assignment_1 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z1")
  
  assignment_2 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z2")
  
  estimator_main <- declare_estimator(
    Y ~ Z1 + Z2,
    model = lm,
    estimates = get_regression_coefficient_robust,
    coefficient_name = "Z1"
  )
  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = list(assignment_1, assignment_2),
    estimator = estimator_main,
    potential_outcomes = potential_outcomes,
    diagnosand = power
  )
  
  expect_error(diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  ))
  
  # Factorial design with Interaction
  
  interaction_coefficient <- 0
  n <- 500
  
  population <-
    declare_population(noise = "rnorm(n_)", size = 10000)
  sampling <- declare_sampling(n = n)
  
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ .5 * Z1 + .5 * Z2 + interaction_coefficient * Z1 * Z2 + noise,
      condition_names = list(Z1 = c(0, 1),
                             Z2 = c(0, 1)),
      assignment_variable_name = c("Z1", "Z2"),
      interaction_coefficient = interaction_coefficient
    )
  
  assignment_1 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z1")
  
  assignment_2 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z2")
  
  
  estimand_int <- declare_estimand(
    estimand_text =
      "mean(Y_Z1_1_Z2_1 - Y_Z1_0_Z2_1) -
    mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0)",
    potential_outcomes = potential_outcomes,
    label = "interaction_effect"
  )
  
  estimator_int <- declare_estimator(
    Y ~ Z1 * Z2,
    model = lm,
    estimates = get_regression_coefficient_robust,
    coefficient_name = "Z1:Z2",
    estimand = estimand_int
  )
  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = list(assignment_1, assignment_2),
    estimator = estimator_int,
    potential_outcomes = potential_outcomes
  )
  
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
  
  # Three arm design
  
  interaction_coefficient <- 0
  n <- 500
  
  population <-
    declare_population(noise = "rnorm(n_)", size = 10000)
  sampling <- declare_sampling(n = n)
  
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ 0.5 * Z1 + 0.5 * Z2 + interaction_coefficient * Z1 * Z2 + noise,
      condition_names = list(Z1 = c(0, 1),
                             Z2 = c(0, 1)),
      assignment_variable_name = c("Z1", "Z2"),
      interaction_coefficient = interaction_coefficient
    )
  
  assignment_3 <-
    declare_assignment(condition_names = c(0, 1, 2),
                       transform_options = list(Z1 = c(1),
                                                Z2 = c(2)))
  
  estimand <- declare_estimand(
    estimand_text =
      "mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0)",
    potential_outcomes = potential_outcomes,
    label = "main_effect"
  )
  
  estimator <- declare_estimator(
    Y ~ Z1 + Z2,
    model = lm,
    estimates = get_regression_coefficient_robust,
    coefficient_name = "Z1",
    estimand = estimand
  )
  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment_3,
    estimator = estimator,
    potential_outcomes = potential_outcomes
  )
  
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
  
})

test_that("section on 'Declaration and Diagnosis of a Bayesian Estimation Strategy' works", {
  # Model of posterior distribution
  compute_posterior <- function(
    # takes data containing y (vector of successes)
    data,
    # beta priors
    alpha_prior = 1,
    beta_prior = 1,
    # and deterministic sample from posterior distribution
    grid_size = 1000) {
    # Get data
    y <- data$y
    # Get domain of unknown proportion parameter
    domain <- seq(from = .0000001,
                  to = .9999999,
                  length.out = grid_size)
    # Get prior probability over domain
    prior <- dbeta(x = domain,
                   shape1 = alpha_prior,
                   shape2 = beta_prior)
    # Get likelihood over domain
    likelihood <- dbinom(x = sum(y),
                         size = length(y),
                         prob = domain)
    # Get unstandardized posterior probs
    unstd_posterior <- prior * likelihood
    # Get standardized posterior probs
    posterior <- unstd_posterior / sum(unstd_posterior)
    # Return prior probs, posterior probs, and correspoinding values of unknown
    return(list(
      prior_prob = prior,
      posterior_prob = posterior,
      domain = domain
    ))
  }
  # Estimates function for summarizing posterior inferences
  get_posterior_estimates <- function(model) {
    posterior_prob <- model$posterior_prob
    prior_prob <- model$prior_prob
    domain <- model$domain
    # Get Maximum A Posteriori
    max_apost <- domain[which.max(posterior_prob)]
    # Get posterior mean
    mean_post <- sum(domain * posterior_prob)
    # Re-normalized prior mean
    prior_prob_n <- prior_prob / sum(prior_prob)
    mean_prior <- sum(domain * prior_prob_n)
    # Get prior/posterior variance ratios
    var_post <- sum(posterior_prob * (domain - mean_post) ^ 2)
    var_prior <- sum(prior_prob_n * (domain - mean_prior) ^ 2)
    # Get percent reduction in variance
    var_red <- (var_post - var_prior) / var_prior
    # Get variance ratio
    var_rat <- var_post / var_prior
    # Get shift in means
    mean_shift <- mean_post - sum(domain * (prior_prob / sum(prior_prob)))
    # Get 90% credibility interval
    cred_low <-
      domain[which(round(cumsum(posterior_prob), 2) == .1)[1]]
    upper_pos <- which(round(cumsum(posterior_prob), 2) == .9)
    cred_upp <- domain[upper_pos[length(upper_pos)]]
    
    return(
      c(
        max_apost = max_apost,
        mean_post = mean_post,
        var_post = var_post,
        var_prior = var_prior,
        var_red = var_red,
        var_rat = var_rat,
        mean_shift = mean_shift,
        cred_upp = cred_upp,
        cred_low = cred_low
      )
    )
  }
  # Simple DGP
  population <- declare_population(
    noise = "runif(n_) - .2",
    prob_success = "ifelse(noise>1,1,ifelse(noise<0,0,noise))",
    success = "rbinom(n_,1,prob_success)",
    size = c(10 ^ 5)
  )
  
  # Sample 100
  sampling <- declare_sampling(n = 100)
  
  # POs with no treatment effect 
  POs <- declare_potential_outcomes(
    formula = y ~ success + Z * 0,
    condition_names = 0:1,
    assignment_variable_name = "Z"
  )
  
  # Estimand is true average underlying success probability
  estimand <- declare_estimand(
    estimand_text = "mean(prob_success)", 
    potential_outcomes = POs,
    estimand_level = 'population')
  
  # One strategy uses flat priors
  flat_prior <- declare_estimator(
    model = compute_posterior,
    model_options = list(
      alpha_prior = 1,
      beta_prior = 1,
      grid_size = 1000
    ),
    estimates = get_posterior_estimates,
    estimand = estimand
  )
  
  # The other uses weakly informative priors
  info_prior <- declare_estimator(
    model = compute_posterior,
    model_options = list(
      alpha_prior = 2,
      beta_prior = 2,
      grid_size = 1000
    ),
    estimates = get_posterior_estimates,
    estimand = estimand
  )
  
  # A range of diagnosands that are specific to Bayes 
  bayesian_diagnosands <- list(
    mean_var_red = declare_diagnosand(
      diagnostic_statistic_text = "var_red",
      summary_function = mean,
      label = "Avg. % Reduction in Variance (Prior vs. Posterior)"
    ),
    mean_max_apost = declare_diagnosand(
      diagnostic_statistic_text = "max_apost",
      summary_function = mean,
      label = "Avg. Maximum A Posteriori"
    ),
    mean_mean_post = declare_diagnosand(
      diagnostic_statistic_text = "mean_post",
      summary_function = mean,
      label = "Avg. Posterior Mean"
    ),
    bias_max_apost = declare_diagnosand(
      diagnostic_statistic_text = "max_apost - estimand",
      summary_function = mean,
      label = "Bias in Maximum A Posteriori"
    ),
    bias_mean_post = declare_diagnosand(
      diagnostic_statistic_text = "mean_post - estimand",
      summary_function = mean,
      label = "Bias in Posterior Mean"
    ),
    coverage_prob = declare_diagnosand(
      diagnostic_statistic_text = "estimand >= cred_low & estimand <= cred_upp",
      summary_function = mean,
      label = "Coverage Probability of Posterior Mean"
    ),
    avg_shift = declare_diagnosand(
      diagnostic_statistic_text = "mean_shift",
      summary_function = mean,
      label = "Average Shift in Mean (Prior vs. Posterior)"
    ),
    avg_shift = declare_diagnosand(
      diagnostic_statistic_text = "estimand",
      summary_function = mean,
      label = "True Population Proportion"
    )
  )
  
  # Design declaration
  design <- declare_design(
    population = population,
    sampling = sampling,
    estimator = list(flat_prior, info_prior),
    potential_outcomes = POs,
    diagnosand = bayesian_diagnosands
  )
  
  # Diagnosis
  diagnosis <- diagnose_design(
    design = design,
    population_draws = 1,
    sample_draws = 100,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

# "Matching Design" ------------------------------
  
test_that("section on 'Matching' works",{
  # The population has three normally distributed covariate
  population <- declare_population(X1 = "rnorm(n_)",
                                   X2 = "rnorm(n_)",
                                   X3 = "rnorm(n_)",
                                   size = 5000)

  # The design samples 1000 units at random
  sampling <- declare_sampling(n = 1000)
  # The potential outcomes of each unit are a linea
  # combination of their covariates and the treatment effect (Z) of 1
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ 5 + X1 + X2 + X3 + Z,
      condition_names = c(0, 1),
      assignment_variable_name = "Z"
    )
  # A custom function assigns treatment as a function of the three covariates
  my_custom_assignment <- function(data) {
    prob <- pnorm(data$X1 + data$X2 + data$X3)
    return(rbinom(
      n = nrow(data),
      size = 1,
      prob = prob
    ))
  }
  # And the assignment procedure is declared
  assignment <-
    declare_assignment(potential_outcomes = potential_outcomes,
                       custom_assignment_function = my_custom_assignment)
  # The estimand is the average treatment effect among the treated (ATT)
  estimand <-
    declare_estimand(
      estimand_text = "mean(Y_Z_1[Z==1] - Y_Z_0[Z==1])",
      potential_outcomes = potential_outcomes,
      estimand_level = "assignment"
    )
  # One (non-matching) estimator to use as a benchmark is a simple
  # difference in means, for which we use the in-build difference_in_means
  estimator_d_i_m <-
    declare_estimator(
      estimates = difference_in_means,
      formula = Y ~ Z,
      estimand = estimand,
      label = "dim"
    )
  # Compare to a custom estimator that uses matching
  matching_estimator <- function(data) {
    # The data is first matched and the effects
    # estimated
    match_out <-
      with(data, Match(
        Y = Y,
        Tr = Z,
        X = cbind(X1, X2, X3),
        estimand = "ATT"
      ))
    # Then the different estimates are returned
    # in a dataframe
    est <- match_out$est
    se <- match_out$se
    p <- 2 * (1 - pnorm(
      q = as.numeric(abs(est)),
      sd = se,
      lower.tail = TRUE
    ))
    return_df <- data.frame(
      est = est,
      se = se,
      p = p,
      ci_lower = est - 1.96 * se,
      ci_upper = est + 1.96 * se
    )
    return(return_df)
  }
  # Declare the matching estimator
  estimator_m <-
    declare_estimator(estimates = matching_estimator,
                      estimand = estimand,
                      label = "matching")
  # Declare the design
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment,
    estimator = list(estimator_d_i_m, estimator_m),
    potential_outcomes = potential_outcomes
  )
  # Diagnose design
  diagnose_design(
    design = design,
    population_draws = 1,
    sample_draws = 1,
    assignment_draws = 1000,
    bootstrap_diagnosands = FALSE
  )
})
  
  
# "Descriptive Design" ------------------------------

test_that("section on 'Descriptive Design' works", {
  # Declare population with a latent probability of voting at all, and 
  # some latent probability of supporting Hilary Clinton in 2016
  population <- declare_population(
    latent_voting = "rnorm(n_)",
    latent_HRC_support = ".1*latent_voting + rnorm(n_) - .1",
    voter = "rbinom(n_, 1, prob = pnorm(latent_voting))",
    HRC_supporter = "rbinom(n_, 1, prob = pnorm(latent_HRC_support))",
    likely_voter = "rbinom(n_, 1, prob = pnorm(latent_voting - 2))",
    size = 10000
  )
  # Sample 1000 people at random
  sampling <- declare_sampling(n = 1000)
  
  # The estimand is simply the amount of votes HRC will receive
  estimand <-
    declare_estimand(estimand_text = "mean(HRC_supporter[voter==1])",
                     potential_outcomes = potential_outcomes)
  # The estimator takes the sample mean and standard error thereof,
  # based on likely voters
  HRC_estimator <- function(data) {
    se_mean <- function(x) {
      n <- length(x)
      return(sd(x) / (sqrt(n)))
    }
    est = with(data, mean(HRC_supporter[likely_voter == 1]))
    se = with(data, se_mean(HRC_supporter[likely_voter == 1]))
    return_df <- data.frame(
      est = est,
      se = se,
      # Null hypothesis is that HRC support is 50% or lower
      p = pnorm(
        est,
        mean = .5,
        sd = se,
        lower.tail = FALSE
      ),
      ci_lower = est - 1.96 * se,
      ci_upper = est + 1.96 * se
    )
    return(return_df)
  }
  # Declare the estimator
  estimator <-
    declare_estimator(estimates = HRC_estimator, estimand = estimand)
  # Declare a custom diagnosand
  mean_estimand <- declare_diagnosand(
    diagnostic_statistic_text = "mean(estimand)",
    summary_function = mean,
    label = "mean(estimand)"
  )
  # Declare the descriptive design
  design <- declare_design(
    population = population,
    sampling = sampling,
    estimator = estimator,
    diagnosand = list(
      mean_estimand,
      mean_estimate,
      sd_estimate,
      power,
      bias,
      rmse,
      coverage
    )
  )
  # Diagnose the design
  diagnose_design(
    design,
    population_draws = 1000,
    population_replicates = 1,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = FALSE
  )
})

test_that("model-based inference example works",{
  # U
  U <- declare_population(
                  u      =  declare_variable(),
                  size   =  10
                  )

  # Y is a concave function of treatment
  Y    <- declare_potential_outcomes(
                condition_names = list(Z = 1:3),
                potential_outcomes_function = function(data) {
                  with(data, 0*(Z==1) + 3*(Z==2) + 4*(Z==3) + u)},
                outcome_variable_name = "Y",
                assignment_variable_name = "Z")


  # Model based estimand: generated as coeficient from model on superdata
  f_tau    <- function(data)  { YY = with(data, c(Y_Z_1,Y_Z_2, Y_Z_3))
                                XX = rep(1:3, each = nrow(data))
                                coef(lm(YY~XX))[2]}

  tau       <- declare_estimand(estimand_function = f_tau, potential_outcomes = Y)

  # Assignment
  p_Z  <- declare_assignment(condition_names = 1:3, probability_each = c(1,1,1)/3)
  p_Z  <- declare_assignment(condition_names = 1:3, probability_each = c(.4, .4, .2))

  # Estimates
  b <- declare_estimator(
    formula           = Y ~ Z, 
    model             = lm, 
    estimates         = get_regression_coefficient, 
    coefficient_name  = "Z",
    estimand          = tau,
    labels            = "OLS")


  # Declare design
  model_design <- declare_design(population = U, 
                                   sampling = declare_sampling(sampling = FALSE), 
                                   assignment = p_Z, 
                                   estimator = b,
                                   potential_outcomes = Y, 
                                   label = "simple_panel")

  # Diagnose
  diagnose_design(design = model_design, 
                  population_draws = 500, 
                  sample_draws = 1)
})
