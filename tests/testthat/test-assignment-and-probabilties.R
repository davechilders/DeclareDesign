
context("Assignment and probability functions")

test_that("test assignment and probability functions", {
  
  population <- declare_population(individuals = list(noise = "rnorm(n_)",
                                                      ideo_3 = "sample(c('Liberal', 'Moderate', 'Conservative'), size = n_, prob = c(.2, .3, .5), replace = T)"),
                                   villages = list(elevation = "rnorm(n_)",
                                                   high_elevation = "1*(elevation > 0)"), 
                                   size = c(1000, 100))
  sampling <- declare_sampling(N = N_, m = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1, 2),
                                                   assignment_variable_name = "Z")
  
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  
  # Base R assignments
  
  
  assignment_00 <- declare_assignment(assignment_function = rbinom, n = n_, size = 1, prob = .2)
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_00))
  
  # Complete Random Assignment assignments
  assignment_0 <- declare_assignment(potential_outcomes = potential_outcomes)
  assignment_1 <- declare_assignment(potential_outcomes = potential_outcomes, condition_names = c(0, 1))
  assignment_2 <- declare_assignment(potential_outcomes = potential_outcomes, m = 60, condition_names = c(0, 1))
  assignment_3 <- declare_assignment(potential_outcomes = potential_outcomes, m_each =c(20, 30, 50))
  assignment_4 <- declare_assignment(potential_outcomes = potential_outcomes, m_each =c(20, 80), condition_names = c(0, 1))
  assignment_5 <- declare_assignment(potential_outcomes = potential_outcomes, prob_each = c(.2, .3, .5))
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_0))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_1))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_2))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_3))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_4))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_5))
  
  
  # Blocked assignments
  assignment_6 <- declare_assignment(potential_outcomes = potential_outcomes, block_var = ideo_3)
  assignment_7 <- declare_assignment(potential_outcomes = potential_outcomes, block_var = ideo_3, prob_each = c(.3, .6, .1))
  assignment_8 <- declare_assignment(potential_outcomes = potential_outcomes, block_var = ideo_3, condition_names = c(0, 1))
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_6))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_7))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_8))
  
  block_probabilities <- rbind(c(.1, .2, .7),
                               c(.1, .7, .2),
                               c(.7, .2, .1))
  
  assignment_8.5 <- declare_assignment(potential_outcomes = potential_outcomes, 
                                       block_var = ideo_3,
                                       block_prob_each = block_probabilities)
  
  assignment_8.6 <- declare_assignment(potential_outcomes = potential_outcomes, 
                                       block_var = ideo_3,
                                       condition_names = c(0, 1),
                                       block_m = c(10, 10, 10))
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_8.5))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_8.6))
  
  
  # Clustered assignments 
  assignment_9 <- declare_assignment(potential_outcomes = potential_outcomes, clust_var = villages_ID)
  assignment_10 <- declare_assignment(potential_outcomes = potential_outcomes, clust_var = villages_ID, condition_names = c(0, 1))
  assignment_11 <- declare_assignment(potential_outcomes = potential_outcomes, clust_var = villages_ID, prob_each = c(.1, .3, .6))
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_9))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_10))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_11))
  
  # Blocked and Clustered assignments
  assignment_12 <- declare_assignment(potential_outcomes = potential_outcomes, 
                                      clust_var = villages_ID, 
                                      block_var = high_elevation)
  assignment_13 <- declare_assignment(potential_outcomes = potential_outcomes, 
                                      clust_var = villages_ID, 
                                      block_var = high_elevation, condition_names = c(0,1))
  assignment_14 <- declare_assignment(potential_outcomes = potential_outcomes, 
                                      clust_var = villages_ID, 
                                      block_var = high_elevation, prob_each = c(.1, .3, .6))
  
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_12))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_13))
  table(assign_treatment_indicator(data = smp_draw, assignment = assignment_14))
  
  # Draw Data
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  
  
  # Attempt to Assign
  smp_draw$Z0 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_0) 
  smp_draw$Z1 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_1) 
  smp_draw$Z2 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_2) 
  smp_draw$Z3 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_3) 
  smp_draw$Z4 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_4) 
  smp_draw$Z5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_5) 
  
  smp_draw$Z6 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_6) 
  smp_draw$Z7 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_7) 
  smp_draw$Z8 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8) 
  smp_draw$Z8_5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8.5)
  smp_draw$Z8_6 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8.6) 
  
  with(smp_draw, table(ideo_3, Z6))
  with(smp_draw, table(ideo_3, Z7))
  with(smp_draw, table(ideo_3, Z8))
  with(smp_draw, table(ideo_3, Z8_5))
  with(smp_draw, table(ideo_3, Z8_6))
  
  smp_draw$Z9 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_9) 
  smp_draw$Z10 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_10) 
  smp_draw$Z11 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_11) 
  
  with(smp_draw, table(Z9 ,villages_ID))
  with(smp_draw, table(Z10,villages_ID))
  with(smp_draw, table(Z11,villages_ID))
  
  smp_draw$Z12 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_12) 
  smp_draw$Z13 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_13) 
  smp_draw$Z14 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_14) 
  
  with(smp_draw, table(Z12, villages_ID))
  with(smp_draw, table(Z12, high_elevation))
  
  with(smp_draw, table(Z13, villages_ID))
  with(smp_draw, table(Z13, high_elevation))
  
  with(smp_draw, table(Z14, villages_ID))
  with(smp_draw, table(Z14, high_elevation))
  
  # reveal observed probs
  
  prob_obs_1 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z1", assignment = assignment_1) 
  prob_obs_2 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z2", assignment = assignment_2) 
  prob_obs_3 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z3", assignment = assignment_3) 
  prob_obs_4 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z4", assignment = assignment_4) 
  prob_obs_5 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z5", assignment = assignment_5) 
  prob_obs_6 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z6", assignment = assignment_6) 
  prob_obs_7 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z7", assignment = assignment_7) 
  prob_obs_8 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z8", assignment = assignment_8)
  
  
  prob_obs_9 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z9", assignment = assignment_9) 
  prob_obs_10 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z10", assignment = assignment_10) 
  prob_obs_11 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z11", assignment = assignment_11) 
  
  prob_obs_12 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z12", assignment = assignment_12) 
  prob_obs_13 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z13", assignment = assignment_13) 
  prob_obs_14 <- get_observed_assignment_probabilities(data = smp_draw, assignment_variable_name = "Z14", assignment = assignment_14) 
  
  assignment_transform <- declare_assignment(potential_outcomes = potential_outcomes,
                                             transform_options = list(Z1 = c(0, 1),
                                                                      Z2 = 2))
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment_transform)
  
  with(smp_draw, table(Z, Z1))
  with(smp_draw, table(Z, Z2))
  
})
