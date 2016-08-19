rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Summary methods")


# SUMMARY.POPULATION --------------------------------------------------------------------------

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




# SUMMARY.POTENTIAL_OUTCOMES ------------------------------------------------------------------

## Formula + variables

pop1 <- declare_population(noise = "rnorm(n_)", size = 250)

po1 <- declare_potential_outcomes(formula = Y ~ 0.25 * Z + noise,
                                                 condition_names = c(0, 1),
                                                 assignment_variable_name = "Z")


test_that("summary of potantial outcomes works", {
  expect_equal(names(summary(po1, extended = FALSE)), "summary_text")
  expect_equal(names(summary(po1)), c("summary_text", "po_labels", "code"))
  expect_silent(summary(po1))
})

## Multiple treatments

pop2 <- declare_population(noise = "rnorm(n_)", size = 250)

po2 <- declare_potential_outcomes(formula = Y ~ 5 + 1*Z1 + 2*Z2 - 3*Z1*Z2 + noise,
                                                 condition_names = list(Z1 = c(0, 1), 
                                                                        Z2 = c(0, 1)),
                                                 assignment_variable_name = c("Z1", "Z2"))

test_that("summary of potantial outcomes with multiple treatments works", {
  expect_equal(names(summary(po2, extended = FALSE)), "summary_text")
  expect_equal(names(summary(po2)), c("summary_text", "po_labels", "code"))
  expect_silent(summary(po2))
})

## Declare potential outcomes using potential_outcomes_function

pop3 <- declare_population(noise = "rnorm(n_)", size = 250)

my_po <- function(data) { with(data, Z * 0.25 + noise) }

po3 <- declare_potential_outcomes(potential_outcomes_function = my_po,
                                  outcome_variable_name = 'Y',
                                  condition_names = c(0, 1))

test_that("summary of potential outcomes using custom function works",{
  expect_equal(names(summary(po3, extended = FALSE)), "summary_text")
  expect_equal(names(summary(po3)), c("summary_text", "po_labels", "code"))
  expect_silent(summary(po3))
})


## Multiple potential outcomes

pop4 <- declare_population(noise = "rnorm(n_)", size = 250)

po4_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                    condition_names = c(0, 1),
                                    assignment_variable_name = "Z")

po4_2 <- declare_potential_outcomes(formula = Y2 ~ 5 + .25*Z + noise,
                                    inherit_condition_names = TRUE,
                                    assignment_variable_name = "Z")

test_that("summary of potential outcomes with multiple potential outcomes works",{
  expect_equal(length(lapply(list(po4_1, po4_2), summary)), 2)
  expect_true(all(sapply(X = list(po4_1, po4_2),
                         FUN = function(x) names(summary(x, extended = FALSE))) == 
               "summary_text"))
  expect_true(all(sapply(X = list(po4_1, po4_2),
                         FUN = function(x) names(summary(x))) == 
                    c("summary_text", "po_labels", "code")))
  expect_equal(summary(po4_1)$po_labels[[1]], c("Z_0", "Z_1"))
  expect_equal(summary(po4_2)$po_labels[[1]], "inherit_condition_names = TRUE. The labels of potential outcomes will be inherited from the first potential_outcomes object created by declare_potential_outcomes with specified condition names.")
})

