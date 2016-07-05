rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Check Design")

test_that("check_design works", {
  
  simple_template <- function(N = 5000, 
                              n = 100, 
                              potential_outcomes_formula = Y ~ 5 + .5*Z*noise1 + noise2,
                              noise1 = "rnorm(n_)", noise2 = "rnorm(n_)",
                              potential_outcomes_condition_names = c(0, 1),
                              assignment_variable_name = "Z",
                              assignment_probability_each = c(.7, .3),
                              estimand_text = "mean(Y_Z_1 - Y_Z_0)",
                              estimator_function = difference_in_means,
                              estimator_formula = Y ~ Z
  ){
    
    population <- declare_population(noise1 = noise1, noise2 = noise2, size = N)
    
    sampling <- declare_sampling(n = n)
    
    potential_outcomes <- 
      declare_potential_outcomes(formula = potential_outcomes_formula,
                                 condition_names = potential_outcomes_condition_names,
                                 assignment_variable_name = assignment_variable_name)
    
    assignment <- declare_assignment(potential_outcomes = potential_outcomes, 
                                     probability_each = assignment_probability_each,
                                     assignment_variable_name = assignment_variable_name)
    
    estimand <- declare_estimand(estimand_text = estimand_text, 
                                 potential_outcomes = potential_outcomes)
    
    estimator <- declare_estimator(estimates = estimator_function, 
                                   formula = estimator_formula, 
                                   estimand = estimand)
    
    design <- declare_design(population = population,
                             sampling = sampling, 
                             assignment = assignment, 
                             estimator = estimator, 
                             potential_outcomes = potential_outcomes,
                             label = "Simple Design")
    return(design)
  }
  
  # Check design with incorrect sampling
  design1 <- simple_template(N = 100, n = 110)
  expect_warning(check_design(design1), "sampling", ignore.case = TRUE)
  
  # Check design with incorrect estimator labels
  design2 <- simple_template(N = 200, n = 100)
  design2$estimator[[2]] <- design2$estimator[[1]]
  expect_warning(check_design(design2), "labels", ignore.case = TRUE)
  
  # Check correct design
  design3 <- simple_template(N = 200, n = 100)
  expect_warning(check_design(design3), NA)
  
  
})
