rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Compare experiments")

##  comparison of >2 designs CHECK
##comparison of varying along two dimensions at once DONE
#comparison across:
##  population size (N, N_per_level)
##population structure (homogenous vs. heterogeneous groups)
##two custom sampling strategies DONE
##assignments DONE
##estimators DONE
##POs DONE
##different custom arguments (inputs) DONE

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

test_that("compare two sampling strategies using modify_design", {
  
  design_alt <- modify_design(design = design, sampling = sampling_large)
  
  comparison_two_designs <- compare_designs(list(design, design_alt), population_draws = 10)
  
})


test_that("compare to two designs varying sampling directly", {
  
  comparison_change_inputs <- compare_designs(design = design, sampling = list(sampling, sampling_large), population_draws = 10)
  
})

assignment_equal_prob <- declare_assignment(condition_names = c(0,1))

test_that("compare two assignment strategies using modify_design", {
  
  design_alt <- modify_design(design = design, assignment = assignment_equal_prob)
  
  comparison_two_designs <- compare_designs(list(design, design_alt), population_draws = 10)
  
})

test_that("compare to two designs varying assignment directly", {
  
  comparison_change_inputs <- compare_designs(design = design, assignment = list(assignment, assignment_equal_prob), population_draws = 10)
  
})

estimator_lm <- declare_estimator(model = lm, formula = Y ~ Z,
                                  estimates = get_regression_coefficient,
                                  coefficient_name = "Z",
                                  estimand = estimand)

test_that("compare two estimators using modify_design", {
  
  design_alt <- modify_design(design = design, estimator = estimator_lm)
  
  comparison_two_designs <- compare_designs(list(design, design_alt), population_draws = 10)
  
})

test_that("compare to two designs varying estimator directly", {
  
  comparison_change_inputs <- compare_designs(design = design, estimator = list(estimator_d_i_m, estimator_lm), population_draws = 10)
  
})

potential_outcomes_fixed <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                       condition_names = c(0, 1),
                                                       assignment_variable_name = "Z")

test_that("compare two PO declarations using modify_design", {
  
  design_alt <- modify_design(design = design, potential_outcomes = potential_outcomes_fixed)
  
  comparison_two_designs <- compare_designs(list(design, design_alt), population_draws = 10)
  
})

test_that("compare to two designs varying PO declarations directly", {
  
  comparison_change_inputs <- compare_designs(design = design, potential_outcomes = list(potential_outcomes, potential_outcomes_fixed), population_draws = 10)
  
})


test_that("compare 3 designs", {
  
  design_large_sampling <- declare_design(population = population,
                                          sampling = sampling_large, 
                                          assignment = assignment, 
                                          estimator = estimator_d_i_m, 
                                          potential_outcomes = potential_outcomes,
                                          label = "Simple Design")
  
  design_lm_estimator <- declare_design(population = population,
                                        sampling = sampling_large, 
                                        assignment = assignment, 
                                        estimator = estimator_lm, 
                                        potential_outcomes = potential_outcomes,
                                        label = "Simple Design")
  
  
  comparison_change_inputs <- compare_designs(design = list(design, design_large_sampling, design_lm_estimator), population_draws = 10)
  
})

test_that("compare varying two parameters", {
  
  comparison_change_inputs <- compare_designs(design = design, 
                                              sampling = list(sampling, sampling_large),
                                              estimator = list(estimator_d_i_m, estimator_lm), population_draws = 10)
  
})

test_that("compare two different custom sampling functions", {
  custom_sampling_1  <- function(data) { N <- nrow(data); sample(c(0, 1), N, prob = c(.1, .9), replace = T) }
  custom_sampling_2  <- function(data) { N <- nrow(data); sample(c(0, 1), N, prob = c(.25, .75), replace = T) }
  
  sampling_custom_1 <- declare_sampling(custom_sampling_function = custom_sampling_1)
  
  sampling_custom_2 <- declare_sampling(custom_sampling_function = custom_sampling_2)
  
  design_1 <- modify_design(design = design, sampling = sampling_custom_1)
  design_2 <- modify_design(design = design, sampling = sampling_custom_2)
  
  comparison_two_designs <- compare_designs(list(design_1, design_2))
  
  comparison_change_inputs <- compare_designs(design = design, 
                                              sampling = list(sampling_custom_1, 
                                                              sampling_custom_2), population_draws = 10)
  
})

test_that("compare two different custom assignment functions", {
  custom_assignment_1  <- function(data) { N <- nrow(data); sample(c(0, 1), N, prob = c(.1, .9), replace = T) }
  custom_assignment_2  <- function(data) { N <- nrow(data); sample(c(0, 1), N, prob = c(.25, .75), replace = T) }
  
  assignment_custom_1 <- declare_assignment(condition_names = c(0,1), 
                                            custom_assignment_function = custom_assignment_1)
  
  assignment_custom_2 <- declare_assignment(condition_names = c(0,1), 
                                            custom_assignment_function = custom_assignment_2)
  
  design_1 <- modify_design(design = design, assignment = assignment_custom_1)
  design_2 <- modify_design(design = design, assignment = assignment_custom_2)
  
  comparison_two_designs <- compare_designs(list(design_1, design_2))
  
  comparison_change_inputs <- compare_designs(design = design, 
                                              assignment = list(assignment_custom_1, 
                                                                assignment_custom_2), population_draws = 10)
  
})
