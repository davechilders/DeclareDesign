#' Declare the experimental assignment
#'
#'
#' @param assignment_function A function of data that returns an assignment vector of length n.
#' @param assignment_probability_function A function of data that returns an assignment vector of length n.
#' @param condition_names A vector describing the conditions to which subjects can be assigned. Alternatively, condition_names can be obtained from a potential_outcomes object. 
#' @param potential_outcomes potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. The conditions to which subjects can be assigned is obtained from the condition_names stored in a potential outcomes object.  If you prefer, you can use the condition_names argument.
#' @param block_variable_name The name of the variable according to which block random assignment should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random assignment should be conducted.
#' @param baseline_condition The value of condition_names that represents the "baseline" condition.  This is the condition against which treatment effects will be assessed. Defaults to the first value of condition_names.
#' @param assignment_variable_name The name of the treatment variable.  Defaults to "Z"
#' @param transform_function A function to transform assignments into one or more variables.
#' @param transform_options Options sent to the \code{custom_transform_function}.
#' @param blocking_function  A function of data that returns a blocking vector of length n.
#' @param clustering_function A function of data that returns a cluster vector of length n.
#' @param existing_assignment_variable_name The name of an already-assigned treatment variable.
#' @param description A description of the assignment procedure in words.
#' @param ... options passed to assignment function and, if provided, the assignment probability function
#'
#' @return assignment object
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
#'    assignment_variable_name = "Z")
#' 
#' # Complete Random Assignment assignments
#' assignment_1 <- declare_assignment(potential_outcomes = potential_outcomes)
#' assignment_2 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m = 60, condition_names = c(0, 1))
#' assignment_3 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 30, 50))
#' assignment_4 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 80), condition_names = c(0, 1))
#' assignment_5 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    probability_each = c(.2, .3, .5))
#' 
#' # Blocked assignments
#' assignment_6 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3")
#' assignment_7 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3", 
#'                                    probability_each = c(.3, .6, .1))
#' assignment_8 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3", 
#'                                    condition_names = c(0, 1))
#' 
#' block_probabilities <- rbind(c(.1, .2, .7),
#'                     c(.1, .7, .2),
#'                     c(.7, .2, .1),
#'                     c(.7, .1, .2),
#'                     c(.2, .1, .7))
#' assignment_8.5 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                      block_variable_name = "ideo_3",
#'                                      block_probabilities = block_probabilities)
#' 
#' # Clustered assignments 
#' assignment_9 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID")
#' assignment_10 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    condition_names = c(0, 1))
#' assignment_11 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    probability_each = c(.1, .3, .6))
#' 
#' # Blocked and Clustered assignments
#' assignment_12 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation")
#' assignment_13 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation", 
#'                                     condition_names = c(0,1))
#' assignment_14 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation", 
#'                                     probability_each = c(.1, .3, .6))
#' 
#' # Draw Data
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment_1)
#' 
#' # Attempt to Assign
#' smp_draw$Z1 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_1) 
#' smp_draw$Z2 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_2) 
#' smp_draw$Z3 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_3) 
#' smp_draw$Z4 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_4) 
#' smp_draw$Z5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_5) 
#' 
#' smp_draw$Z6 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_6) 
#' smp_draw$Z7 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_7) 
#' smp_draw$Z8 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8) 
#' smp_draw$Z8_5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8.5) 
#' 
#' with(smp_draw, table(ideo_3, Z6))
#' with(smp_draw, table(ideo_3, Z7))
#' with(smp_draw, table(ideo_3, Z8))
#' with(smp_draw, table(ideo_3, Z8_5))
#' 
#' smp_draw$Z9 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_9) 
#' smp_draw$Z10 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_10) 
#' smp_draw$Z11 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_11) 
#' 
#' with(smp_draw, table(Z9 ,villages_ID))
#' with(smp_draw, table(Z10,villages_ID))
#' with(smp_draw, table(Z11,villages_ID))
#' 
#' smp_draw$Z12 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_12) 
#' smp_draw$Z13 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_13) 
#' smp_draw$Z14 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_14) 
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
declare_assignment <- 
  function(assignment_function = conduct_ra,
           assignment_probability_function = obtain_condition_probabilities,
           condition_names = NULL,
           assignment_variable_name = "Z",
           potential_outcomes = NULL,
           transform_function = default_transform_function, 
           transform_options = NULL,
           blocking_function = NULL,
           clustering_function = NULL,
           existing_assignment_variable_name = NULL,
           existing_assignment_probabilities_variable_name = NULL,
           description = NULL,
           ...) {
    
    if(substitute(transform_function) == "default_transform_function" &
       getNamespaceName(environment(transform_function)) == "DeclareDesign" &
       is.null(transform_options)){
      transform_function <- NULL
    }
    
    if(is.null(potential_outcomes$condition_names) & is.null(condition_names)){
      stop("Please provide an input to condition_names or a potential_outcomes object with condition_names.")
    }
    
    # Checks -------------------------------------------------
    potential_outcomes <- clean_inputs(potential_outcomes, "potential_outcomes", accepts_list = FALSE)
    
    if(!is.null(condition_names)){
      condition_names <- clean_condition_names(condition_names)
    }
    
    if(!is.null(potential_outcomes) & !is.null(potential_outcomes$condition_names) & is.null(condition_names)){
      # Obtain Condition Names
      if(class(potential_outcomes) == "list"){
        if(length(unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$outcome_variable_name})))) != length(potential_outcomes)){
          stop("Please use different outcome names in each potential outcomes object.")
        }
        condition_names <- 
          unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$condition_names})))
      }else{
        condition_names <- potential_outcomes$condition_names
      }
    } 
    
    assignment_function_options <- eval(substitute(alist(...)))
    argument_names <- names(formals(assignment_function))
    if(!is.null(condition_names) & "condition_names" %in% argument_names)
      assignment_function_options$condition_names <- condition_names
    
    assignment_function_internal <- function(data){
      if("N" %in% argument_names & !("N" %in% assignment_function_options))
        assignment_function_options$N <- nrow(data)
      
      data_environment <- list2env(data)
      data_environment$n_ <- nrow(data)
      
      do.call(assignment_function, args = assignment_function_options, envir = data_environment)
    }
    
    assignment_probability_function_options <- eval(substitute(alist(...)))
    argument_names <- names(formals(assignment_probability_function))
    if(!is.null(condition_names) & "condition_names" %in% argument_names)
      assignment_probability_function_options$condition_names <- condition_names
    
    assignment_probability_function_internal <- function(data){
      ## the restriction here is that you must have an option in your custom assignment probability
      ## function called 'assignment' that takes a vector of assignments
      ## note this in documentation
      if("assignment" %in% argument_names)
        assignment_probability_function_options$assignment <- data[, assignment_variable_name]
      
      data_environment <- list2env(data)
      data_environment$n_ <- nrow(data)
      
      do.call(assignment_probability_function, args = assignment_probability_function_options, envir = data_environment)
    }
    
    if(is.null(existing_assignment_variable_name)) {
      return.object <- list(
        assignment_function = assignment_function_internal,
        assignment_probability_function = assignment_probability_function_internal,
        condition_names = condition_names,
        assignment_variable_name = assignment_variable_name,
        transform_function = transform_function, 
        transform_options = transform_options,
        description = description,
        call = match.call())
    } else {
      return.object <- list(
        existing_assignment_variable_name = existing_assignment_variable_name,
        existing_assignment_probabilities_variable_name = existing_assignment_probabilities_variable_name,
        condition_names = condition_names,
        assignment_variable_name = assignment_variable_name,
        transform_function = custom_transform_function, 
        transform_options = transform_options,
        description = description,
        call = match.call())
    }
    class(return.object) <- "assignment"
    return(return.object)
  }

