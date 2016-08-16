#' @title Template to generate regression discontinuity designs
#'
#'
#' @details
#' This templating function generates a sharp regression discontinuity design based on continuity assumptions. 

#' @param N Number of units.
#' @param formula The estimating formula. 
#' @param running_variable A \code{\link{declare_variable}} expression that specifies the distribution of the running variable. Standard normal by default.
#' @param baseline_variable A \code{\link{declare_variable}} expression that specifies the distribution of the baseline variable that is added to each unit's potential outcomes. Standard normal by default. 
#' @param polynomial The number of polynomial terms in the response functions.
#' @param control_coefs The coefficients in the polynomial response function of the control.
#' @param control_coefs The coefficients in the polynomial response function of the treatment.
#' @param cutoff The location of the discontinuity. Median of the running variable by default.
#' @param bandwidth The size of the window around the cutoff to include in the sample. Includes all units by default.
#' @param plot_response Logical indicator for whether to plot a representation of the response functions.
#' @param coefficient_name In case custom formula are specified, the coefficient that should be extracted from the model fit as an estimate. 
#'
#' @return A design
#' @export


rdd_template <- function(N = 100,
                         formula = y ~ running + Z + Z * running,
                         running_variable = declare_variable(type = "normal"),
                         baseline_variable = declare_variable(type = "normal", location_scale = c(0, .1)),
                         polynomial = 4,
                         control_coefs = c(1, 0, 0, 0),
                         treatment_coefs = c(.02, .02, .02, .2),
                         cutoff = NULL,
                         bandwidth = NULL,
                         plot_responses = TRUE,
                         coefficient_name = "Z") {
  n_c_coefs <- length(control_coefs)
  n_t_coefs <- length(treatment_coefs)
  if (n_c_coefs != n_t_coefs) {
    stop("control_coefs and treatment_coefs must be of the same length.")
  }
  if (!all(polynomial == c(n_c_coefs, n_t_coefs))) {
    stop(
      "The number of control_coefs and treatment_coefs must be equal to the degree of the polynomial."
    )
  }
  if (!grepl("n_", running_variable)) {
    warning("In running_variable, you can specify the size using 'n_'.")
  }
  
  if (!grepl("n_", baseline_variable)) {
    warning("In baseline_variable, you can specify the size using 'n_'.")
  }
  
  if (!all(sapply(c(running_variable, baseline_variable), class) == "character")) {
    stop(
      "Please give running_variable and baseline_variable an expression of class character to evaluate."
    )
  }
  
  big_running <-
    eval(expr = parse(text = running_variable),
         envir = list2env(list(n_ = 10 ^ 5)))
  min_running <- min(big_running)
  max_running <- max(big_running)
  
  if (is.null(cutoff)) {
    cutoff <- round(median(big_running), 2)
  }
  if (is.null(bandwidth)) {
    bandwidth <-
      abs(c(min_running, max_running)[which.min(c(abs(min_running), abs(max_running)))])
  }
  
  
  control <- function(running) {
    poly(x = running,
         degree = polynomial,
         raw = T) %*% control_coefs
  }
  treatment <- function(running) {
    poly(x = running,
         degree = polynomial,
         raw = T) %*% treatment_coefs
  }
  
  
  population <- declare_population(
    running = running_variable,
    baseline = baseline_variable,
    control_response = "control(running)",
    treatment_response = "treatment(running)",
    control_response_at_limit = "control(cutoff)",
    treatment_response_at_limit = "treatment(cutoff)",
    size = N,
    options = list(
      cutoff = cutoff,
      treatment = treatment,
      control = control
    )
  )
  
  bw_sampling <- function(data) {
    as.numeric(data$running >= (cutoff - bandwidth) &
                 data$running <= cutoff + bandwidth)
  }
  
  sampling <-
    declare_sampling(custom_sampling_function = bw_sampling)
  
  
  observable_POs <- declare_potential_outcomes(
    formula = y ~ Z * treatment_response + (1 - Z) * control_response + baseline,
    assignment_variable_name = "Z",
    condition_names = c(0, 1)
  )
  unobservable_POs <- declare_potential_outcomes(
    formula = y_lim ~ Z * treatment_response_at_limit + (1 - Z) * control_response_at_limit + baseline,
    assignment_variable_name = "Z",
    condition_names = c(0, 1)
  )
  
  assignment <- declare_assignment(
    condition_names = c(0, 1),
    assignment_variable_name = "Z",
    custom_assignment_function = function(data)
      1 * (data$running > cutoff)
  )
  
  
  LATE <-
    declare_estimand(
      estimand_text = "mean(y_lim_Z_1 - y_lim_Z_0)",
      potential_outcomes = unobservable_POs,
      estimand_level = 'sample'
    )
  
  SATE <-
    declare_estimand(
      estimand_text = "mean(y_Z_1 - y_Z_0)",
      potential_outcomes = observable_POs,
      estimand_level = 'sample'
    )
  
  
  estimator <-
    declare_estimator(
      formula = formula,
      model = lm,
      estimates = get_regression_coefficient,
      coefficient_name = coefficient_name,
      estimand = list(LATE, SATE)
    )
  
  if (plot_responses) {
    pop_data <- draw_population(population = population)
    sample_data <- draw_sample(data = pop_data, sampling = sampling)
    mock_data <-
      assign_treatment(data = sample_data, assignment = assignment)
    POs <- draw_potential_outcomes(
      data = mock_data,
      potential_outcomes = list(observable_POs,
                                unobservable_POs)
    )
    plot(x = mock_data$running,
         y = POs$y_Z_0,
         col = "red")
    points(x = mock_data$running,
           y = POs$y_Z_1,
           col = "blue")
    abline(v = cutoff)
    
  }
  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment,
    potential_outcomes = list(observable_POs, unobservable_POs),
    estimator = estimator,
    label = "RDD Design"
  )
  
  return(design)
  
}
