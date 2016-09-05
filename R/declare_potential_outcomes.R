#' Declare potential outcomes. 
#' 
#' Potential outcomes are endogenous outcomes of interest that are not under the direct control of the researcher. Examples include whether a drug is offered by a doctor, whether a drug is ingested, whether a patient survives, and whether outcomes are measured. Use this function to define a mapping between manipulands (typically randomly assigned treatments), ancestor variables (typically pre-treatment covariates) and potential outcomes. For instance, you may declare patient survival to be a function of whether or not the patient receives an experimental drug (a randomly assigned treatment) and the patient's pre-treatment health condition (a pre-treatment covariate). This gives rise to two potential outcomes for each patient - whether the patient survives when he receives the drug (treated potential outcome) and whether the patient dies when he does not receive the drug (control potential outcome). 
#' 
#' @param potential_outcomes_function A function that takes a data frame as the argument \code{data} and returns a vector of length \code{nrow(data)}. Use to define potential outcomes as a function of treatment assignment(s) and pre-treatment covariates See details.
#' @param formula An object of class \link{formula} (or one that can be coerced to that class): a symbolic description of the relationship between potential outcomes, treatment assignment(s) and pre-treatment characateristics. See details.
#' @param outcome_variable_name The name of the outcome variable as a character string. Can be omitted if a formula is provided.
#' @param condition_names A vector of treatment condition names, such as c(0, 1). If there are multiple treatments, a list of vectors of condition names with one vector for each treatment. Required for the first \code{potential_outcomes} object supplied to a design. Can be omitted for subsequent \code{potential_outcomes} objects if \code{inherit_condition_names = TRUE} is specified. 
#' @param inherit_condition_names A logical indicating whether \code{condition_names} should be inherited from a previous \code{potential_outcomes} object (TRUE) or not (FALSE).
#' @param sep A character string indicating the separator that will be used to construct the variable names of potential outcomes. Defaults to \code{_} which yields variable names such as \code{Y_Z_1} and \code{Y_Z_0}.
#' @param assignment_variable_name A character string that contains the variable name of the treatment assignment indicator that appears in the potential outcomes \code{formula} or the \code{potential_outcomes_function}. Defaults to \code{"Z"}. If there are multiple treatments, a vector of treatment indicator names with one element per treatment is required.  
#' @param interference An interference object created by \code{\link{declare_interference}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' @param description A character string containing a description of the potential outcomes. 
#' @param ... Other arguments passed to the \code{potential_outcomes_function}.
#' 
#' @details Use the \code{formula} argument to specify the relationship between potential outcomes, treatment assignments and pre-treatment covariates. A typical formula has the form \code{Y ~ terms}. \code{Y} is the name of the outcome variable. \code{terms} is an expression that typically consists of functions of the treatment assignment indicator(s)  and of the pre-treatment covariates that have been built into the \code{population} object. If a \code{formula} is supplied, there is no need to supply a \code{potential_outcomes_function}.
#' 
#'          An alternative way to specify the relationship between potential outcomes, treatment assignments and pre-treatment covariates is to use the \code{potential_outcomes_function} argument. The function supplied to this argument needs to return an outcome vector of length \code{nrow(data)} when applied to a data frame that contains the following columns: 
#'          \itemize{
#'            \item  one column for each treatment indicator named according to the character string(s) that have been supplied to the \code{assignment_variable_name} argument,
#'            \item  the ID variables and pre-treatment covariates that have been built into the \code{population} object using the \link{declare_population} function.
#'          }
#'          
#'          
#'          There is no need to supply a \code{formula} if a \code{potential_outcomes_function} is supplied.
#'          
#'          The \code{potential_outcomes_function} argument defaults to the \link{default_potential_outcomes_function} which takes the arguments \code{formula} and \code{data} and returns the corresponding outcome vector.
#'          
#'          If more than one potential outcomes object is created, subsequent potential outcomes can be functions of previously specified potential outcomes (see examples).    
#'        
#'          See the \link{proportion_potential_outcomes_function} for a built-in potential outcomes function that can be used to create proportional potential outcomes.
#'  
#' @return potential_outcomes object.
#'
#' @examples 
#' ## Declare potential outcomes using potential_outcomes_function
#' 
#'  population <- declare_population(noise = "rnorm(n_)", size = 250)
#'  
#'  my_potential_outcomes <- function(data) { with(data, Z * 0.25 + noise) }
#'  
#'  potential_outcomes <- declare_potential_outcomes(potential_outcomes_function = my_potential_outcomes,
#'                                                    outcome_variable_name = 'Y',
#'                                                    condition_names = c(0, 1))
#'  
#'  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
#'  
#'  head(pop_draw)
#'  
#'  ## Declare potential outcomes using formula
#'  
#'  population <- declare_population(noise = "rnorm(n_)", size = 250)
#'  
#'  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 0.25 * Z + noise,
#'                                                    condition_names = c(0, 1),
#'                                                    assignment_variable_name = "Z")
#'                                                    
#'  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
#'  
#'  head(pop_draw)
#'  
#'  ## Multiple treatments
#'  
#'  population <- declare_population(noise = "rnorm(n_)", size = 250)
#'  
#'  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + 1*Z1 + 2*Z2 - 3*Z1*Z2 + noise,
#'                                                    condition_names = list(Z1 = c(0, 1), 
#'                                                    Z2 = c(0, 1)),
#'                                                    assignment_variable_name = c("Z1", "Z2"))
#'                                                    
#'   pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
#'   
#'   head(pop_draw)
#'   
#'  ## Multiple potential outcomes 
#'  
#'  population <- declare_population(noise = "rnorm(n_)", size = 250)
#'  
#'  potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
#'                                                      condition_names = c(0, 1),
#'                                                      assignment_variable_name = "Z")
#'  
#'  potential_outcomes_2 <- declare_potential_outcomes(formula = Y2 ~ 5 + .25*Z + noise,
#'                                                      inherit_condition_names = TRUE,
#'                                                      assignment_variable_name = "Z")
#'  
#'  pop_draw <- draw_population(population = population, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2))
#'  
#'  head(pop_draw)
#'  
#'  ## Potential outcomes as a function of previously specified potential outcomes
#'  
#'  population <- declare_population(noise = "rnorm(n_)", size = 250)
#'  
#'  potential_outcomes_1 <- declare_potential_outcomes(formula = D ~ (Z == 1) * (noise > 0),
#'                                                      condition_names = c(0, 1),
#'                                                      assignment_variable_name = "Z")
#' 
#'  potential_outcomes_2 <- declare_potential_outcomes(formula = Y ~ 5 + .5*D*rnorm(n_) + noise,
#'                                                      condition_names = c(0, 1),
#'                                                      assignment_variable_name = "D")
#'                                                      
#'  pop_draw <- draw_population(population = population, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2))
#'  
#'  head(pop_draw)
#'  
#'  
#' @export
declare_potential_outcomes <- function(
  potential_outcomes_function = 
    default_potential_outcomes_function,
  formula = NULL, outcome_variable_name = NULL, 
  condition_names = NULL, inherit_condition_names = FALSE, sep = "_", 
  assignment_variable_name = "Z",
  interference = NULL, attrition = NULL,
  description = NULL, ...){
  
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
                        call = match.call())
  
  structure(return_object, class = "potential_outcomes")
  
}



