rm(list = ls())
library(testthat)
library(DeclareDesign)

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

# Add in new examples here 



