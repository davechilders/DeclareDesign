rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Summary diagnosis")

test_that("summary works on diagnosis object produced by diagnose_design()", {
  
  population <- declare_population(individuals = list(noise = declare_variable()),
                                   villages = list(elevation = declare_variable(),
                                                   high_elevation = "1*(elevation > 0)"), 
                                   size = c(1000, 100))
  
  sampling <- declare_sampling(n = 24, cluster_variable_name = "villages_ID")
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(condition_names = c(0,1),
                                   block_variable_name = "elevation_high", 
                                   custom_blocking_function = function(data) return(1*(data$elevation > 0)),
                                   cluster_variable_name = "villages_ID")
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means,
                                       formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_clustered <- declare_estimator(estimates = difference_in_means,
                                                 cluster_variable_name = "villages_ID",
                                                 formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_blocked <- declare_estimator(estimates = difference_in_means_blocked,
                                               block_variable_name = "elevation_high",
                                               formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_blocked_clustered <- declare_estimator(estimates = difference_in_means_blocked,
                                                         block_variable_name = "elevation_high",
                                                         cluster_variable_name = "villages_ID",
                                                         formula = Y ~ Z, estimand = estimand)
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    coefficient_name = "Z",
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_d_i_m, estimator_d_i_m_clustered, estimator_d_i_m_blocked, estimator_d_i_m_blocked_clustered, estimator_lm), 
                           potential_outcomes = potential_outcomes,
                           label = "Blocked and Clustered Design")
  
  diagnosis <- diagnose_design(design = design, 
                               population_draws = 3, 
                               sample_draws = 3,
                               assignment_draws = 3,
                               population_replicates = 100)
  
  expect_equal(nrow(summary(diagnosis)), 35)
  expect_equal(nrow(summary(diagnosis, diagnosand = "rmse")), 5)
  expect_equal(nrow(summary(diagnosis, diaGnosand = c("rMse","bias"))), 10)
  expect_equal(nrow(summary(diagnosis, diagnosand = c("Rmse","bias"), Estimator = "lm")), 2)
  expect_equal(nrow(summary(diagnosis, 
                            diagnosand = c("rMsE","bias"),
                            Estimator = "lm",
                            estimate = "Z",
                            estimand = "mean(Y_Z_1 - Y_Z_0)")), 2)
})
 
test_that("summary works on diagnosis object produced by compare_designs()", { 
  
  population <- declare_population(noise = "rnorm(n_)", size = 1000)
  sampling <- declare_sampling(n = 100)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(condition_names = c(0,1), probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  sampling_large <- declare_sampling(n = 500)
  
  
    design_alt <- modify_design(design = design, sampling = sampling_large)
    
    comparison_two_designs <- compare_designs(list(design, design_alt))
    
    # class of compare_designs() output is diagnosis_list right now
    class(comparison_two_designs) <- "diagnosis"
    
    expect_equal(nrow(summary(comparison_two_designs)), 14)
    expect_equal(nrow(summary(comparison_two_designs, 
                              diagnosand = "rmse")), 2)
    expect_equal(nrow(summary(comparison_two_designs, 
                              diaGnosand = c("rMse","bias"))), 4)
    expect_equal(nrow(summary(comparison_two_designs, 
                              diagnosand = c("Rmse","bias"), 
                              Estimator = "d_i_m")), 4)
    expect_equal(nrow(summary(comparison_two_designs, 
                              diagnosand = c("rMsE","bias"),
                              Estimator = "d_i_m",
                              estimate = "1-0",
                              estimand = "mean(Y_Z_1 - Y_Z_0)")), 4)
    # will work if diagnosis label column will be named diagnosis_label rather
    # than diagnosis
    expect_false(nrow(summary(comparison_two_designs, 
                              diagnosand = c("rMsE","bias"),
                              Estimator = "d_i_m",
                              estimate = "1-0",
                              estimand = "mean(Y_Z_1 - Y_Z_0)",
                              diagnosis = "1")) == 4)
  
})
