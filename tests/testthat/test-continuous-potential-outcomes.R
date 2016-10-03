rm(list=ls())
library(testthat)
library(DeclareDesign)
suppressMessages({
  library(dplyr)
  library(reshape2)
  library(stringr)
})


context("Continuous potential outcomes")

test_that("test whether custom continuous assignment function and POs with fixed condition names workflow works", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_, 0, .1) + noise,
                                                   condition_names = round(seq(0,1, by = .1), 15), 
                                                   assignment_variable_name = "Z")
  
  my_ra <- function(data){
    Z <- runif(nrow(data), 0, 1)
    return(Z)
  }
  
  # we want to not need to do this in cus
  assignment <- declare_assignment(custom_assignment_function = my_ra)
    pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  
  
  my_estimand_function <- function(data){
    data %>% 
      select(level_A_ID, starts_with("Y_")) %>%
      melt(id.vars= "level_A_ID") %>%
      mutate(Z_star = as.numeric(sapply(str_split(variable, pattern = "_"), function(x) x[3]))) %>%
      group_by(level_A_ID) %>%
      do(mod = lm(value ~ Z_star, data = .)) %>%
      do(data.frame(ind_slopes = coef(.$mod)[2])) %>%
      ungroup() %>%
      summarize(ATE = mean(ind_slopes)) %>%
      as.numeric %>%
      return
  }
  
  estimand <- declare_estimand(estimand_function = my_estimand_function, 
                               potential_outcomes = potential_outcomes)
  
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    coefficient_name = "Z",
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_lm, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  data <- draw_data(design = design)
  head(data)  
  
  diagnosis <- diagnose_design(design = design, population_draws = 10)
})
