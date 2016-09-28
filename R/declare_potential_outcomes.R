#' Declare potential outcomes
#' 
#' @param potential_outcomes_function A function to draw potential outcomes as a function of a treatment assignment.
#' @param formula A formula indicating the relationship between treatment assignment(s) and covariates.
#' @param outcome_variable_name The variable name of the outcome.
#' @param condition_names A vector of condition names, such as c(0, 1).
#' @param inherit_condition_names An indicator for whether condition_names should be inherited from potential outcomes (TRUE) or not (FALSE).
#' @param sep Separator to construct the variable names of potential outcomes.
#' @param assignment_variable_name The variable name of the treatment assignment(s) that appear in the potential outcomes \code{formula}.
#' @param interference An interference object created by \code{\link{declare_interference}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' @param description Description in text of the potential outcomes. 
#' @param ... Other options for the \code{potential_outcomes_function}.
#' 
#' @return potential_outcomes object.
#'
#' @export
declare_potential_outcomes <- function(
  potential_outcomes_function = 
    default_potential_outcomes_function,
  formula = NULL, outcome_variable_name = NULL, 
  condition_names = NULL, inherit_condition_names = FALSE, sep = "_", 
  assignment_variable_name = "Z",
  interference = NULL, attrition = NULL,
  description = NULL,
  potential_outcomes = TRUE,
  ...){
  
  if(potential_outcomes == FALSE){
    return(structure(
      list(potential_outcomes = FALSE,
           potential_outcomes_function = default_potential_outcomes_function,
           outcome_variable_name = "Y",
           condition_names = c(0),
           assignment_variable_name = "Z",
           interference = NULL, attrition = NULL,
           inherit_condition_names = FALSE,
           sep = "_",
           description = NULL, call = match.call()),
      class = "potential_outcomes"
    ))
  }
  
  if(inherit_condition_names == FALSE & is.null(condition_names)){
    stop("Please either provide condition_names or set inherit_condition_names to TRUE. The first potential_outcomes created in a design must include condition_names.")
  }
  
  condition_names <- clean_condition_names(condition_names)
  
  options <- list(...)
  
  # Checks -------------------------------------------------
  attrition <- clean_inputs(attrition, "attrition", accepts_list = FALSE)
  interference <- clean_inputs(interference, "interference", accepts_list = TRUE)
  
  if(is.null(formula) & is.null(outcome_variable_name)){
    stop("If you do not provide a formula, please provide the name of the outcome variable as a character string to outcome_variable_name.")
  }
  
  if(is.list(condition_names) & (length(assignment_variable_name) != length(condition_names))){
    stop("If you provide a list of vectors of condition names, you must provide a vector to assignment_variable_name of the same length.")
  }
  
  if(is.list(condition_names) & (!all(names(condition_names) %in% assignment_variable_name))){
    stop("If you provide a list of vectors of condition names, that list must be named with the same assignment variable names as you provide to assignment_variable_name.")
  }
  
  if(is.list(condition_names) & (!all(assignment_variable_name %in% names(condition_names)))){
    stop("If you provide a list of vectors of condition names, that list must be named with the same assignment variable names as you provide to assignment_variable_name.")
  }
  
  if(class(potential_outcomes_function) != "function"){
    stop("Please provide a function in the potential_outcomes_function argument.")
  }
  
  potential_outcomes_function_internal <- function(data = NULL){
    argument_names <- names(formals(potential_outcomes_function))
    if(!is.null(formula) & "formula" %in% argument_names)
      options$formula <- formula
    if(!is.null(data) & "data" %in% argument_names)
      options$data <- data
    if(!is.null(condition_names) & "condition_names" %in% argument_names)
      options$condition_names <- condition_names
    if(!is.null(assignment_variable_name) & "assignment_variable_name" %in% argument_names)
      options$assignment_variable_name <- assignment_variable_name
    
    return(do.call(potential_outcomes_function, args = options))
  }
  
  if(is.null(outcome_variable_name) & !is.null(formula)){
    outcome_variable_name <- as.character(formula[[2]])
  }
  
  return_object <- list(potential_outcomes_function = potential_outcomes_function_internal, 
                        outcome_variable_name = outcome_variable_name, sep = "_", 
                        condition_names = condition_names, inherit_condition_names = inherit_condition_names,
                        assignment_variable_name = assignment_variable_name,
                        interference = interference,
                        attrition = attrition, description = description,
                        potential_outcomes = potential_outcomes,
                        call = match.call())
  
  structure(return_object, class = "potential_outcomes")
  
}



