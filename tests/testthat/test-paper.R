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
  population <-
    declare_population(custom_population_function = my_population, size = 100)
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
    phi <- as.list(summary(reg)$coefficients["Z", ])
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
    declare_diagnosand(diagnostic_statistic_text = bias_diagnosand,
                       summary_function = mean)
  
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
  spillover_template <- get_template(name = "spillovers")
  factorial_template <- get_template(name = "factorial")
  covariates_template <- get_template(name = "covariates")
  
  expect_message(
    heterogeneous_designs <-
      quick_design(
        template = covariates_template,
        n = 20,
        g = vary(0, .3, .6, .9)
      )
  )
  
  compare_designs(design = heterogeneous_designs, population_draws = 10)
  
  expect_message(
    spillover_designs <- quick_design(
      template = spillover_template,
      buffer = vary(0, 3, 4),
      dist_effect = vary(0, 1, 3),
      intersect = TRUE
    )
  )
  
  expect_warning(compare_designs(design = spillover_designs))
  
  expect_message(
    factorial_designs <- quick_design(
      template = factorial_template,
      assignment_strategy = vary("two_by_two", "three_arm"),
      interaction_coefficient = vary(0.00, 0.05, 0.10, 0.15, 0.2,
                                     0.25, 0.30, 0.35, 0.4)
    )
  )
  
  compare_designs(design = factorial_designs, population_draws = 10)
  
})

# "Illustration of Sampling Decisions: Handling Spatial Spillovers" -------

test_that(
  "section 'Illustration of Sampling Decisions: Handling Spatial Spillovers' of paper works",
  {
    # The population is just 36 points in a grid
    # Each point is one unit distant from its neighbor
    # We sample two points and randomly assign one to treatment
    spatial_data <-
      expand.grid(x = 1:6, y = 1:6)
    spatial_data$id <- 1:nrow(spatial_data)
    # Create a distance matrix by calculate distance of 
    # each point to each other point
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
    # Set the diagonal of the distance matrix to 0
    diag(distmat) <- 0
    # and the range of the size of the spillover effect.
    dist_effect <- 2
    buffer <- 4
    spillover <- 0
    # Generate all possible pairs of points
    pairs <- t(with(spatial_data, combn(id, 2)))
    # Generate distances of pairs
    dists <- distmat[pairs]
    # Generate indicator for all samples whose points are 
    # at least the size of the buffer distant from each other 
    pairs <- data.frame(unit_1 = pairs[, 1], unit_2 = pairs[, 2])
    pairs$outside <- dists > buffer
    # Generate the probabilities of being assigned to treatment 
    # for each point given the buffer and the proximity to other points
    spatial_data$buffer_prob <-
      with(subset(pairs, subset = outside),
           sapply(
             X = spatial_data$id,
             FUN = function(unit_id)
               mean(c(unit_1, unit_2) %in% unit_id)
           ))
    # Generate the distance effect
    spatial_data$total_dist <- spatial_data$y ^ 2 / 10
    # The population has a simple structure and the distance
    # matrix and other spatial data in it
    population <- declare_population(
      noise = "rnorm(n_)",
      buffer_prob = "spatial_data$buffer_prob",
      distance = "spatial_data$total_dist",
      size = 36,
      options = list(spatial_data = spatial_data,
                     distmat = distmat),
      super_population = TRUE
    )
    # We sample two units and assign one of them in the same step
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
    # The spillover affects any units that fall within the buffer
    interference <- declare_potential_outcomes(
      formula = E ~ (distmat %*% I(Z == 2) < 4) * I(Z != 2),
      condition_names = c(0, 1, 2)
    )
    # The potential outcomes are a combination of the treatment effect,
    # the spillover, spatial autocorrelation and background noise
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
    # We turn off sampling as it is conducted during assignment
    sampling <- declare_sampling(sampling = FALSE)
    # The estimand is the direct effect of the treatment 
    estimand <- declare_estimand(
      estimand_text = "1",
      potential_outcomes = potential_outcomes,
      fixed = TRUE,
      label = "fixed_estimand"
    )
    # We estimate using an IPW regression 
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
    # Declare the design
    design <- declare_design(
      population = population,
      sampling = sampling,
      assignment = assignment,
      estimator = estimator,
      potential_outcomes = list(interference, potential_outcomes),
      label = "Spillover Buffer Design"
    )
    # Diagnose the design
    diagnose_design(
      design = design,
      population_draws = 10,
      sample_draws = 1,
      assignment_draws = 1,
      bootstrap_diagnosands = F
    )
  }
)



# Illustration of Analysis Decisions: Gains from Covariate Control --------

test_that("section 'Illustration of Analysis Decisions: Gains from Covariate Control' works",{
  # Set up the basic parameters for the analysis
  N <- 24
  n <- 24
  m <- floor(n / 2)
  sdev <- 1
  b <- 1
  f <- 0
  g <- 0
  # Declare the population
  my_population <- declare_population(
    e = declare_variable(type = "normal", location_scale = c(0, sdev)),
    XT = declare_variable(type = "normal", location_scale = c(0, sdev)),
    XM = "sign(XT)*XT^2  - mean(sign(XT)*XT^2)",
    # Misspecification
    size = N,
    super_population = TRUE
  )
  # Potential outcomes are a function of direct effect
  # and heterogeneity by covariate interaction
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
  # Based on parameters, choose an appropriate sampling strategy
  if (n == N)
    my_sampling <- declare_sampling(sampling = FALSE)
  if (n != N)
    my_sampling <- declare_sampling(n = n)
  # The estimand is the ATE
  my_estimand <-
    declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)",
                     potential_outcomes = my_potential_outcomes)
  # A null estimand
  my_estimand0 <-
    declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_1)",
                     potential_outcomes = my_potential_outcomes)
  # Assign m units to treatment
  my_assignment   <- declare_assignment(m = m,
                                        potential_outcomes = my_potential_outcomes)
  # Declare the different kinds of estimators
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
  # Declare the design
  my_design <- declare_design(
    population         = my_population,
    potential_outcomes = my_potential_outcomes,
    sampling           = my_sampling,
    assignment         = my_assignment,
    estimator          = list(M1, M2, M3, B)
  )
  # Diagnose the design
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
  # Factorial design with interaction
  # True interaction set to 0
  interaction_coefficient <- 0
  n <- 500
  # A simple population of 10K units with background noise
  population <- declare_population(noise = "rnorm(n_)", size = 10000)
  # Sample n at random
  sampling <- declare_sampling(n = n)
  # Potential outcomes feature direct and interaction effects
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ .5 * Z1 + .5 * Z2 + interaction_coefficient * Z1 * Z2 + noise,
      condition_names = list(Z1 = c(0, 1),
                             Z2 = c(0, 1)),
      assignment_variable_name = c("Z1", "Z2"),
      interaction_coefficient = interaction_coefficient
    )
  # Assign treatment one and two using simple random assignment
  assignment_1 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z1")
  assignment_2 <-
    declare_assignment(condition_names = c(0, 1),
                       assignment_variable_name  = "Z2")
  # Declare the interaction estimand
  estimand_int <- declare_estimand(
    estimand_text =
      "mean(Y_Z1_1_Z2_1 - Y_Z1_0_Z2_1) -
    mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0)",
    potential_outcomes = potential_outcomes,
    label = "interaction_effect"
  )
  # An estimator of the interaction 
  estimator_int <- declare_estimator(
    Y ~ Z1 * Z2,
    model = lm,
    estimates = get_regression_coefficient_robust,
    coefficient_name = "Z1:Z2",
    estimand = estimand_int
  )
  # Declare the design
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = list(assignment_1, assignment_2),
    estimator = estimator_int,
    potential_outcomes = potential_outcomes
  )
  # Diagnose the design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
  # Three arm design
  # True interaction set to 0
  interaction_coefficient <- 0
  n <- 500
  # A simple population of 10K units with background noise
  population <- declare_population(noise = "rnorm(n_)", size = 10000)
  # Sample n at random
  sampling <- declare_sampling(n = n)
  # Potential outcomes feature direct and interaction effects
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ 0.5 * Z1 + 0.5 * Z2 + interaction_coefficient * Z1 * Z2 + noise,
      condition_names = list(Z1 = c(0, 1),
                             Z2 = c(0, 1)),
      assignment_variable_name = c("Z1", "Z2"),
      interaction_coefficient = interaction_coefficient
    )
  # Assign treatment one and two using complete random assignment
  assignment_3 <-
    declare_assignment(condition_names = c(0, 1, 2),
                       transform_options = list(Z1 = c(1),
                                                Z2 = c(2)))
  # Declare the direct effect estimand
  estimand <- declare_estimand(
    estimand_text =
      "mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0)",
    potential_outcomes = potential_outcomes,
    label = "main_effect"
  )
  # An estimator of the direct effect of Z1
  estimator <- declare_estimator(
    Y ~ Z1 + Z2,
    model = lm,
    estimates = get_regression_coefficient_robust,
    coefficient_name = "Z1",
    estimand = estimand
  )
  # Declare the design
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment_3,
    estimator = estimator,
    potential_outcomes = potential_outcomes
  )
  # Diagnose the design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

test_that("section on 'Declaration and Diagnosis of a Bayesian Estimation Strategy' works",{
  # Simple probit DGP that is mis-specified as beta binomial in Bayesian model
  population <- declare_population(
    # Latent space
    noise = "rnorm(n_,-.9,.5)",
    # Probit transformation
    prob_success = "pnorm(noise)",
    # Actual outcome
    success = "rbinom(n_,1,prob_success)",
    size = c(10 ^ 5)
  )
  # Sample 100 units
  sampling <- declare_sampling(n = 100)
  # Estimand is true average underlying success probability in the population
  estimand <- declare_estimand(estimand_text = "mean(prob_success)",
                               estimand_level = 'population')
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
    y <- data$success
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
  # Estimates function for summarizing posterior distribution
  get_posterior_estimates <- function(model) {
    posterior_prob <- model$posterior_prob
    prior_prob <- model$prior_prob
    domain <- model$domain
    # Get Maximum A Posteriori
    max_apost <- domain[which.max(posterior_prob)]
    # Get posterior mean
    mean_post <- sum((domain * posterior_prob)/sum(posterior_prob))
    # Get prior mean
    mean_prior <- sum((domain * prior_prob) / sum(prior_prob))
    # Get prior/posterior variance ratios
    var_post <- sum(posterior_prob/sum(posterior_prob) * (domain - mean_post) ^ 2)
    var_prior <- sum(prior_prob/sum(prior_prob) * (domain - mean_prior) ^ 2)
    # Get percent reduction in variance
    var_red <- (var_post - var_prior) / var_prior
    # Get variance ratio
    var_rat <- var_post / var_prior
    # Get shift in means
    mean_shift <- mean_post - mean_prior
    # Get 90% credibility interval
    cred_low <- domain[which(round(cumsum(posterior_prob), 2) == .1)[1]]
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
  # Declare to estimation strategies
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
  # The other uses weakly informative priors centered at .5
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
  # A range of diagnosands that are specific to Bayesian estimators
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
  # Declare the design
  design <- declare_design(
    population = population,
    sampling = sampling,
    estimator = list(flat_prior, info_prior),
    diagnosand = bayesian_diagnosands
  )
  # Diagnose the design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

# "Matching Design" ------------------------------

test_that("section on 'Matching' works", {
  # The population has three normally distributed covariate
  population <- declare_population(
    X1 = "rnorm(n_)",
    X2 = "rnorm(n_)",
    X3 = "rnorm(n_)",
    size = 5000
  )
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
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

# "Descriptive Design" ------------------------------

test_that("section on 'Descriptive Design' works", {
  # Declare population with a latent probability of voting at all, and
  # some latent probability of supporting Hillary Clinton in 2016
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
    declare_estimand(estimand_text = "mean(HRC_supporter[voter==1])")
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
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

test_that("model-based inference example works", {
  # Declare a DGP with 10 units and some background noise
  U <- declare_population(u = declare_variable(),
                          size = 10)
  # Y is a concave function of treatment
  Y <- declare_potential_outcomes(
    condition_names = list(Z = 1:3),
    potential_outcomes_function = function(data) {
      with(data, 0 * (Z == 1) + 3 * (Z == 2) + 4 * (Z == 3) + u)
    },
    outcome_variable_name = "Y",
    assignment_variable_name = "Z"
  )
  # Model based estimand: generated as coeficient from model on superdata
  f_tau <- function(data)  {
    YY = with(data, c(Y_Z_1, Y_Z_2, Y_Z_3))
    XX = rep(1:3, each = nrow(data))
    coef(lm(YY ~ XX))[2]
  }
  tau <- declare_estimand(estimand_function = f_tau,
                     potential_outcomes = Y)
  # Declare assignment with equal probabilities
  p_Z  <-
    declare_assignment(condition_names = 1:3,
                       probability_each = c(1, 1, 1) / 3)
  # And one assignment with unequal probabilities
  p_Z  <-
    declare_assignment(condition_names = 1:3,
                       probability_each = c(.4, .4, .2))
  # The estimator is a simple linear regression model
  b <- declare_estimator(
    formula           = Y ~ Z,
    model             = lm,
    estimates         = get_regression_coefficient,
    coefficient_name  = "Z",
    estimand          = tau,
    labels            = "OLS"
  )
  # Declare design
  design <- declare_design(
    population = U,
    sampling = declare_sampling(sampling = FALSE),
    assignment = p_Z,
    estimator = b,
    potential_outcomes = Y,
    label = "simple_panel"
  )
  # Diagnose
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})


# Process tracing ---------------------------------------------------------

test_that("section on 'process tracing' works", {
  # We have a single-case analysis strategy in which we try to
  # make an inference about how likely it is that X caused Y.
  # There are four types of causal scenarios.
  # For A types, X has an adverse causal relationship to Y,
  # so that Y = FALSE when X = TRUE and Y = TRUE when X = FALSE.
  # For B types, X causes Y, such that when
  # X = TRUE, Y = TRUE and when X = FALSE, Y = FALSE.
  # C and D types are chronic and destined, respectively: Y is always FALSE
  # for C types and Y is always TRUE for D types, irrespective of the value of X.
  # We choose only one case in which X = TRUE and Y = TRUE and try to determine the
  # probability that X caused Y given that the case might be a B type or a D type.
  
  # We conduct "naive process tracing" in which inferences are made based on
  # a smoking gun clue, K, seen with 10% probability for B types and not otherwise.
  # Inference updates if clue is seen. However, if the clue is not seen inferences
  # are not updated: the strategy is not fully consistent with Bayes rule.
  # The prior is 0.5 on a B type rather than a D type.
  # In those cases in which K is not seen the correct inference is
  # Pr(type = B | !K) = .9*.5/(.9*.5 + 1*.5) = .474
  # An inference of .5 means a bias of 0.026.
  population <- declare_population(
    # Equal probabilities of different types in the population
    type = "sample(x = c('A','B','C','D'),size = n_,replace = TRUE)",
    # Random process determines value of X
    X = declare_variable(type = "boolean", probabilities = .7),
    # Clue is present with prob = .1 iff case is a B type and X = TRUE
    K = "ifelse(X & type == 'B', sample(c(TRUE,FALSE),size = 1,prob = c(.1,.9)), FALSE)",
    # Y is a function of type and X
    Y = "(type == 'A' & !X) | (type == 'B' & X) | (type == 'D')",
    size   =  200
  )
  # Sample a single X = 1, Y = 1 case; done here by putting 0 weights on other cases
  my_sampling <- function(data) {
    X_and_Y_TRUE <- with(data, X & Y)
    which_case <- sample(x = which(X_and_Y_TRUE), size = 1)
    to_sample <- 1:nrow(data) %in% which_case
    return(to_sample)
  }
  X1Y1_sampling <-
    declare_sampling(custom_sampling_function = my_sampling)
  # The estimand is defined at the level of the sample (i.e. the single case):
  # we want to know: what is the probability that this is a B and not a D case?
  estimand <-
    declare_estimand(estimand_text = "as.numeric(type == 'B')",
                     estimand_level = "sample")
  # Infer that the case is a type B if K is observed and remain with prior
  # beliefs if K is not observed (inconsistent with Bayes' rule)
  my_estimates <- function(data) {
    with(data, c(est = ifelse(K, 1, .5), K_seen = K))
  }
  smoking <- declare_estimator(estimates = my_estimates,
                               estimand = estimand)
  # Process-tracing diagnosands:
  pt_diagnosands <- list(
  # - The average probability that I get a B case
    truth = declare_diagnosand(
      diagnostic_statistic_text = "estimand",
      summary_function = mean,
      label = "Estimand"
    ),
  # - The average value of my inference   
    guess = declare_diagnosand(
      diagnostic_statistic_text = "est",
      summary_function = mean,
      label = "Est based on SG"
    ),
  # - The probability of getting the answer wrong
    error = declare_diagnosand(
      diagnostic_statistic_text = "est - estimand",
      summary_function = mean,
      label = "Bias"
    ),
  # - Given that I see the clue, the probability of being wrong
    cond_error1 = declare_diagnosand(
      diagnostic_statistic_text = "ifelse(K_seen, est - estimand, NA)",
      summary_function = function(x)
        mean(x, na.rm = TRUE),
      label = "Conditional bias when K seen"
    ),
  # - Given that I don't observe the clue, the probability of being wrong
    cond_error0 = declare_diagnosand(
      diagnostic_statistic_text = "ifelse(!K_seen, est - estimand, NA)",
      summary_function = function(x)
        mean(x, na.rm = TRUE),
      label = "Conditional bias when K not seen"
    )
  )
  # Declare the design
  design <- declare_design(
    population = population,
    sampling   = X1Y1_sampling,
    estimator  = smoking,
    diagnosand = pt_diagnosands
  )
  # Look at the data
  mock_data <- draw_data(design = design)
  head(mock_data)
  # Diagnose the design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})


# Discovery section -------------------------------------------------------

test_that("section on 'discovery' works", {
  # Declare a simple population with background covariates 
  population <- declare_population(
    income = "runif(n_)",
    education = "income + 0.25*runif(n_)",
    noise = "runif(n_)",
    Y = ".5 * income + .5 * education + noise",
    size = 500
  )
  # The estimand is the true effect of income 
  estimand <- declare_estimand(estimand_text = "0.5",
                               estimand_level = "population")
  # The right estimator accounts for education
  estimator_right <- declare_estimator(
    model = lm,
    formula = Y ~ income + education,
    estimates = get_regression_coefficient,
    coefficient_name = "income",
    estimand = estimand,
    estimator_label = "correct_model"
  )
  # The wrong estimator does not account for education
  estimator_wrong <- declare_estimator(
    model = lm,
    formula = Y ~ income,
    estimates = get_regression_coefficient,
    coefficient_name = "income",
    estimand = estimand,
    estimator_label = "wrong_model"
  )
  # Declare the split-sample estimator 
  estimator_split_sample <- declare_estimator(
    model = function(data) {
      # The data is split into training and testing sets
      split_sample <- sample(0:1, nrow(data), replace = T)
      train <- data[split_sample == TRUE,]
      test <- data[split_sample == FALSE,]
      # Fit three nested models on the training data
      exploration1 <- lm(Y ~ income, data = train)
      exploration2 <- lm(Y ~ income + education, data = train)
      exploration3 <-
        lm(Y ~ income + education + income * education, data = train)
      explorations <- list(exploration1, exploration2, exploration3)
      # Then compare the Akaike Information Criterion from each
      explorations_compare <-
        sapply(explorations, function(reg)
          AIC(reg))
      # Choose the model with the best fit
      exploration_best <-
        explorations[[which.min(explorations_compare)[1]]]
      # And fit it to the training data
      exploration_test <- lm(formula(exploration_best), data = test)
      return(exploration_test)
    },
    estimates = get_regression_coefficient,
    coefficient_name = "income",
    estimand = estimand,
    estimator_label = "split sample"
  )
  # Compare using the full sample with the right and wrong estimators to 
  # the split sample approach
  design <- declare_design(
    population = population,
    estimator = list(estimator_right, estimator_wrong, estimator_split_sample)
  )
  # Diagnose the design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})

# Regression discontinuity ------------------------------------------------

test_that('section on regression discontinuity works', {
  # Assume all units have the same response functions but their baseline differs
  # Declare control response function
  control <- function(running) {
    as.vector(poly(
      x = running,
      degree = 4,
      raw = T
    ) %*% c(1, 0, 0, 0))
  }
  # Declare treatment response function
  treatment <- function(running) {
    as.vector(poly(
      x = running,
      degree = 4,
      raw = T
    ) %*% c(.02, .02, .02, .2))
  }
  assign(x = "control",value = control,envir = globalenv())
  assign(x = "treatment",value = treatment,envir = globalenv())
  # Declare population
  population <- declare_population(
    # Declare how the running variable is distributed
    running = declare_variable(type = "normal"),
    # Declare how each unit's baseline outcomes are distributed
    baseline = declare_variable(type = "normal", location_scale = c(0, .1)),
    # Set number of units
    size = 100
  )
  # Declare assignment as deterministic function of cutoff and running variable
  assignment <- declare_assignment(
    condition_names = c(0, 1),
    assignment_variable_name = "Z",
    custom_assignment_function = function(data)
      1 * (data[, "running"] > 0)
  )
  # Declare sampling strategy using bandwidth
  bw_sampling <- function(data) {
    # Declare size of window around cutoff to sample units
    as.numeric(data[, "running"] >= (0 - .2) &
                 data[, "running"] <= 0 + .2)
  }
  # Declare sampling strategy
  sampling <-
    declare_sampling(custom_sampling_function = bw_sampling)
  # Declare the potential outcomes function
  
  POs <- declare_potential_outcomes(
    # Units have heterogeneous baselines but identical response functions
    # Define both responses for a given part of the running variable
    formula = y ~ Z * treatment(running) + (1 - Z) * control(running) + baseline,
    assignment_variable_name = "Z",
    condition_names = c(0, 1)
  )
  # Declare 'local' estimand (the average effect of treatment at the cutoff)
  LATE <-
    declare_estimand(
      estimand_text = "treatment(0) - control(0)",
      potential_outcomes = POs,
      estimand_level = 'sample'
    )
  # Define linear estimator shooting at both 'local' and 'sample' ATEs
  estimator <-
    declare_estimator(
      formula = y ~ running + Z + Z * running,
      model = lm,
      estimates = get_regression_coefficient,
      coefficient_name = "Z",
      estimand = LATE
    )
  # Declare design
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment,
    potential_outcomes = POs,
    estimator = estimator,
    label = "RDD Design"
  )
  # Diagnose design
  diagnose_design(
    design = design,
    population_draws = 10,
    sample_draws = 1,
    assignment_draws = 1,
    bootstrap_diagnosands = F
  )
})
