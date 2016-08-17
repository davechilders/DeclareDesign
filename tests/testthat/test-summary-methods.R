rm(list=ls())
library(testthat)
library(DeclareDesign)

context("summary methods")

pop1 <- declare_population(
  indiv = list(
    income = "rnorm(n_)",
    age = "rpois(n_,30)"
  ),
  city = list(
    city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
    city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)"
  ),
  region = list(),
  make_unique_ID = T,
  size = c(1000,50,20)
)

test_that("summary of standard way of declaring population works", {
  expect_equal(names(summary(pop1, extended = FALSE)), "summary_text")
  expect_equal(names(summary(pop1)), c("summary_text", "stat", "code", "internal"))
  expect_null(summary(pop1)$internal$data, summary(pop1)$internal$options)
  expect_true(any(!(class(summary(pop1)) %in% class(summary(pop1, extended = FALSE)))))
}) 




my_DGP <- function(size){
  data <- data.frame(matrix(data = rnorm(size^2), nrow = size))
  names(data) <- paste0("X",1:ncol(data))
  return(data)
}

pop2 <- declare_population(size = 10,
                           custom_population_function = my_DGP,
                           make_unique_ID = T)

test_that("summary of population with simple custom DGP function works", {
  expect_equal(names(summary(pop2, extended = FALSE)), "summary_text")
  expect_equal(names(summary(pop2)), c("summary_text", "stat", "code", "internal"))
  expect_null(summary(pop2)$internal$data, summary(pop2)$internal$options)
  expect_true(any(!(class(summary(pop2)) %in% class(summary(pop2, extended = FALSE)))))
})
  

# user-provided data

user_data <- declare_population(
  individuals = list(
    income = "rnorm(n_)",
    age = "rpois(n_,30)"
  ),
  city = list(
    city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
    city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)"
  ),
  region = list(),
  make_unique_ID = T,
  size = c(1000,50,20)
)$population()

pop3 <- declare_population(
  individuals = list(
    income = get_variable(level_ID = "individuals_ID", 
                          variable_name = "income",
                          data = user_data)
  ),
  cities = list(
    # Here we just grab a variable that does not vary at city level
    educ_level = get_variable(level_ID = "city_ID",
                              variable_name = "city_educ_mean",
                              data = user_data)
  ),
  size = c(500, 50),
  options = list(user_data = user_data)
)

test_that("summary of population with user-provided data works", {
  expect_equal(names(summary(pop3, extended = FALSE)), "summary_text")
  expect_equal(names(summary(pop3)), c("summary_text", "stat", "code", "internal"))
  expect_equal(names(summary(pop3)$internal$options), "user_data")
  expect_true(any(!(class(summary(pop3)) %in% class(summary(pop3, extended = FALSE)))))
})
  
  

# user-provided data, custom resampling function

my_pop_and_data_function <- function(size,data){
  N <- nrow(data)
  data[sample(1:N,size,TRUE),]
}

pop4 <- declare_population(
  custom_population_function = my_pop_and_data_function,
  data = user_data,
  size = 1000,
  resample_data = T)

test_that("summary of population with user-provided data and custom resampling function works", {
  expect_equal(names(summary(pop4, extended = FALSE)), "summary_text")
  expect_equal(names(summary(pop4)), c("summary_text", "stat", "code", "internal"))
  expect_equal(class(summary(pop4)$internal$data), "data.frame")
  expect_true(any(!(class(summary(pop4)) %in% class(summary(pop4, extended = FALSE)))))
})

# standard way

population <- declare_population(individuals = list(noise = "rnorm(n_)",
                                                    ideo_3 = "sample(c('Liberal', 'Moderate', 'Conservative'), size = n_, prob = c(.2, .3, .5), replace = T)"),
                                 villages = list(elevation = "rnorm(n_)",
                                                 high_elevation = "1*(elevation > 0)"), 
                                 size = c(1000, 100))

potential_outcomes <- 
  declare_potential_outcomes(formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
                             condition_names = c(0, 1, 2),
                             assignment_variable_name = "Z")


test_that("summary of potantial outcomes works", {
  expect_silent(summary(potential_outcomes))
})

# custom potential outcomes

population      <- declare_population(noise = declare_variable(), size = 100)

my_potential_outcomes <- function(data) { (data$Z == "Z1") * 0.25 + runif(nrow(data)) }
potential_outcomes <- declare_potential_outcomes(condition_names = c("Z0", "Z1"), 
                                                 potential_outcomes_function = my_potential_outcomes,
                                                 outcome_variable_name = "Y")

test_that("summary of potential outcomes using custom function works ",{
  expect_silent(summary(potential_outcomes))
})




# NEW STUFF FOR POs ---------------------------------------------------------------------------


## Declare potential outcomes using potential_outcomes_function

population <- declare_population(noise = "rnorm(n_)", size = 250)

my_potential_outcomes <- function(data) { with(data, Z * 0.25 + noise) }

potential_outcomes <- declare_potential_outcomes(potential_outcomes_function = my_potential_outcomes,
                                                 outcome_variable_name = 'Y',
                                                 condition_names = c(0, 1))

expect_silent(summary(potential_outcomes))

## Declare potential outcomes using formula

population <- declare_population(noise = "rnorm(n_)", size = 250)

potential_outcomes <- declare_potential_outcomes(formula = Y ~ 0.25 * Z + noise,
                                                 condition_names = c(0, 1),
                                                 assignment_variable_name = "Z")

expect_silent(summary(potential_outcomes))

## Multiple treatments !!!

population <- declare_population(noise = "rnorm(n_)", size = 250)

potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + 1*Z1 + 2*Z2 - 3*Z1*Z2 + noise,
                                                 condition_names = list(Z1 = c(0, 1), 
                                                                        Z2 = c(0, 1)),
                                                 assignment_variable_name = c("Z1", "Z2"))

expect_silent(summary(potential_outcomes))

## Multiple potential outcomes !!!

population <- declare_population(noise = "rnorm(n_)", size = 250)

potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                    condition_names = c(0, 1),
                                                    assignment_variable_name = "Z")

potential_outcomes_2 <- declare_potential_outcomes(formula = Y2 ~ 5 + .25*Z + noise,
                                                    inherit_condition_names = TRUE,
                                                    assignment_variable_name = "Z")


expect_output(sapply(list(potential_outcomes_1, potential_outcomes_2), summary) %>% cat(sep = "\n\n"))

