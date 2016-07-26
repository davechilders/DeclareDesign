rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Coverage plots")

test_that("coverage plots are drawn", {
  
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
  
  diagnosis <- diagnose_design(design = design, population_draws = 4, sample_draws = 4)
  
  multi_estimator_default <- 
    plot(diagnosis)
  
  multi_estimator_cols <- 
    plot(diagnosis,
         facet_formula = ~ estimand_label + estimator_label + estimate_label,
         cols = 5, 
         facet_type = "wrap")
  
  multi_estimator_grid <- 
    plot(diagnosis,
         facet_formula = ~ estimand_label + estimator_label + estimate_label,
         cols = 3, 
         facet_type = "grid")
  
  expect_equal(class(multi_estimator_default),
               class(multi_estimator_cols),
               class(multi_estimator_grid),
               c("gg", "ggplot"))
  expect_equal(as.character(multi_estimator_default$facet)[1],
               as.character(multi_estimator_cols$facet)[1],
               as.character(multi_estimator_grid$facet)[2],
               "list(estimand_label, estimator_label, estimate_label)")
  
  expect_identical(multi_estimator_cols$facet$ncol,
                   5)
  expect_identical(multi_estimator_default$facet$ncol,
                   3)
  expect_identical(class(multi_estimator_grid$facet$cols), "quoted")
  expect_null(multi_estimator_grid$facet$ncol)
  
  # needs to be changed so that we can pick the estimand for which estimator to report
  multi_estimator_formula <- 
    plot(diagnosis,
         facet_formula = ~ estimand_label,
         cols = 3, 
         facet_type = "grid")
  
  expect_null(multi_estimator_formula$facet$facets)
  
  design <- declare_design(population = population,
                           sampling = sampling,
                           assignment = assignment,
                           estimator = estimator_d_i_m,
                           potential_outcomes = potential_outcomes,
                           label = "Blocked and Clustered Design")
  
  diagnosis <- diagnose_design(design = design, population_draws = 2, sample_draws = 4)
  
  single_estimator <- 
    plot(diagnosis,
         facet_formula = ~ estimand_label + estimator_label + estimate_label,
         cols = 3, 
         facet_type = "wrap")
  
  single_estimator_change <- 
    plot(diagnosis,
         facet_formula = ~ estimand_label + estimator_label,
         cols = 3, 
         facet_type = "grid")
  
  expect_equal(length(single_estimator$facet$facets), 3)
  expect_equal(length(single_estimator_change$facet$cols), 2)
  
})
