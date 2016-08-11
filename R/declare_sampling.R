#' Declare the experimental sampling
#'
#'
#' @param sampling_function A function of data that returns an sampling vector of length n.
#' @param sampling_probability_function A function of data that returns an sampling vector of length n.
#' @param condition_names A vector describing the conditions to which subjects can be assigned. Alternatively, condition_names can be obtained from a potential_outcomes object. 
#' @param potential_outcomes potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. The conditions to which subjects can be assigned is obtained from the condition_names stored in a potential outcomes object.  If you prefer, you can use the condition_names argument.
#' @param strata_var The name of the variable according to which strata random sampling should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random sampling should be conducted.
#' @param baseline_condition The value of condition_names that represents the "baseline" condition.  This is the condition against which treatment effects will be assessed. Defaults to the first value of condition_names.
#' @param sampling_variable_name The name of the treatment variable.  Defaults to "Z"
#' @param transform_function A function to transform samplings into one or more variables.
#' @param transform_options Options sent to the \code{custom_transform_function}.
#' @param strataing_function  A function of data that returns a strataing vector of length n.
#' @param clustering_function A function of data that returns a cluster vector of length n.
#' @param existing_sampling_variable_name The name of an already-assigned treatment variable.
#' @param description A description of the sampling procedure in words.
#' @param ... options passed to sampling function and, if provided, the sampling probability function
#'
#' @return sampling object
#' 
#' @examples 
#' 
#' population <- declare_population(individuals = list(noise = "rnorm(n_)",
#'                                    ideo_3 = "sample(c('Liberal', 'Moderate', 'Conservative'), 
#'                                    size = n_, prob = c(.2, .3, .5), replace = T)"),
#'                                  villages = list(elevation = "rnorm(n_)",
#'                                    high_elevation = "1*(elevation > 0)"), 
#'                                  size = c(1000, 100))
#'
#' sampling <- declare_sampling(n = 10, cluster_variable_name = "villages_ID")
#' 
#' potential_outcomes <- declare_potential_outcomes(
#'    formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
#'    condition_names = c(0, 1, 2),
#'    sampling_variable_name = "Z")
#' 
#' # Complete Random sampling samplings
#' sampling_1 <- declare_sampling(potential_outcomes = potential_outcomes)
#' sampling_2 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    m = 60, condition_names = c(0, 1))
#' sampling_3 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 30, 50))
#' sampling_4 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 80), condition_names = c(0, 1))
#' sampling_5 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    probability_each = c(.2, .3, .5))
#' 
#' # strataed samplings
#' sampling_6 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    strata_var = "ideo_3")
#' sampling_7 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    strata_var = "ideo_3", 
#'                                    probability_each = c(.3, .6, .1))
#' sampling_8 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    strata_var = "ideo_3", 
#'                                    condition_names = c(0, 1))
#' 
#' strata_probabilities <- rbind(c(.1, .2, .7),
#'                     c(.1, .7, .2),
#'                     c(.7, .2, .1),
#'                     c(.7, .1, .2),
#'                     c(.2, .1, .7))
#' sampling_8.5 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                      strata_var = "ideo_3",
#'                                      strata_probabilities = strata_probabilities)
#' 
#' # Clustered samplings 
#' sampling_9 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID")
#' sampling_10 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    condition_names = c(0, 1))
#' sampling_11 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    probability_each = c(.1, .3, .6))
#' 
#' # strataed and Clustered samplings
#' sampling_12 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     strata_var = "high_elevation")
#' sampling_13 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     strata_var = "high_elevation", 
#'                                     condition_names = c(0,1))
#' sampling_14 <- declare_sampling(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     strata_var = "high_elevation", 
#'                                     probability_each = c(.1, .3, .6))
#' 
#' # Draw Data
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, sampling = sampling_1)
#' 
#' # Attempt to Assign
#' smp_draw$Z1 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_1) 
#' smp_draw$Z2 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_2) 
#' smp_draw$Z3 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_3) 
#' smp_draw$Z4 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_4) 
#' smp_draw$Z5 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_5) 
#' 
#' smp_draw$Z6 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_6) 
#' smp_draw$Z7 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_7) 
#' smp_draw$Z8 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_8) 
#' smp_draw$Z8_5 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_8.5) 
#' 
#' with(smp_draw, table(ideo_3, Z6))
#' with(smp_draw, table(ideo_3, Z7))
#' with(smp_draw, table(ideo_3, Z8))
#' with(smp_draw, table(ideo_3, Z8_5))
#' 
#' smp_draw$Z9 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_9) 
#' smp_draw$Z10 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_10) 
#' smp_draw$Z11 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_11) 
#' 
#' with(smp_draw, table(Z9 ,villages_ID))
#' with(smp_draw, table(Z10,villages_ID))
#' with(smp_draw, table(Z11,villages_ID))
#' 
#' smp_draw$Z12 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_12) 
#' smp_draw$Z13 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_13) 
#' smp_draw$Z14 <- assign_treatment_indicator(data = smp_draw, sampling = sampling_14) 
#' 
#' with(smp_draw, table(Z12, villages_ID))
#' with(smp_draw, table(Z12, high_elevation))
#' 
#' with(smp_draw, table(Z13, villages_ID))
#' with(smp_draw, table(Z13, high_elevation))
#' 
#' with(smp_draw, table(Z14, villages_ID))
#' with(smp_draw, table(Z14, high_elevation))
#' @export
declare_sampling <- 
  function(sampling = TRUE,
           sampling_function = draw_rs,
           sampling_probability_function = obtain_inclusion_probabilities,
           sampling_variable_name = "S",
           stratafication_function = NULL,
           clustering_function = NULL,
           existing_sampling_variable_name = NULL,
           description = NULL,
           ...) {
    
    if(!(substitute(sampling_function) == "conduct_rs" &
         getNamespaceName(environment(sampling_function)) == "randomizr") & 
       (substitute(sampling_probability_function) == "obtain_inclusion_probabilities" &
        getNamespaceName(environment(sampling_probability_function)) == "randomizr")){
      sampling_probability_function <- NULL
    }
    
    sampling_function_options <- eval(substitute(alist(...)))
    
    sampling_function_internal <- function(data){
      if("N" %in% names(formals(sampling_function)) & !("N" %in% sampling_function_options))
        sampling_function_options$N <- nrow(data)
      
      data_environment <- list2env(data)
      data_environment$N_ <- nrow(data)
      
      do.call(sampling_function, args = sampling_function_options, envir = data_environment)
    }
    
    sampling_probability_function_options <- eval(substitute(alist(...)))
    
    sampling_probability_function_internal <- function(data){
      if("N" %in% names(formals(sampling_probability_function)) & !("N" %in% sampling_probability_function_options))
        sampling_probability_function_options$N <- nrow(data)
      
      data_environment <- list2env(data)
      data_environment$N_ <- nrow(data)
      
      do.call(sampling_probability_function, args = sampling_probability_function_options, envir = data_environment)
    }
    
    if(is.null(existing_sampling_variable_name)) {
      return.object <- list(
        sampling_function = sampling_function_internal,
        sampling_probability_function = sampling_probability_function_internal,
        sampling_variable_name = sampling_variable_name,
        sampling = sampling,
        description = description,
        call = match.call())
    } else {
      return.object <- list(
        existing_sampling_variable_name = existing_sampling_variable_name,
        sampling_variable_name = sampling_variable_name,
        sampling = sampling,
        description = description,
        call = match.call())
    }
    class(return.object) <- "sampling"
    return(return.object)
  }

