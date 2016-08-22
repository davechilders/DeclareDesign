#' Declare Estimand
#' 
#' This function creates an estimand object. quantities to be estimated.
#'
#' @param estimand_function A function that takes a data frame as the argument \code{data} and returns a (possibly single valued) vector of estimands. See details. 
#' @param estimand_text A character string containing an expression that defines the estimand, typically as  a function of potential outcomes. For example, "mean(Y_Z_1 - Y_Z_0)" declares the estimand to be the average difference between the treated (Y_Z_1) and untreated (Y_Z_0) potential outcomes. See details.
#' @param ... Other arguments passed to the \code{estimand_function}.
#' @param estimand_level A character string indicating the level at which the estimand is calculated. Can take the values \code{"population"}, \code{"sample"}, and \code{"assignment"}. If \code{"population"}, an estimand will be drawn for each population draw; if \code{"sample"}, an estimand will be drawn for each sample draw; if \code{"assignment"}, an estimand will be drawn for each assignment draw. Defaults to \code{"population"}.
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}. 
#' @param noncompliance A noncompliance object created by \code{\link{declare_noncompliance}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' @param condition_names A vector of treatment condition names, such as c(0, 1). (what is this used for?)
#' @param subset A character string containing a logical expression that can be passed to the subset argument of the \link{subset} function. For example, "income > 50". Use to declare estimands based on a subset of the units in your population or sample. Typically the subset will be defined based on the pre-treatment covariates that have been built into the \code{population_object} using the \link{declare_population} function.    
#' @param weights_variable_name A character string containing the name of the weighting variable. Weighted estimands are not yet implemented. Please contact the authors if you are interested in using them.
#' @param label A character string containing the estimand's label. Defaults to \code{estimand_text}. 
#' @param description A character string containing a description of the estimand. 
#' 
#' @details The easiest way to define an estimand is to supply a character string containing an expression that defines the estimand to the \code{estimand_text} argument. Alternatively, use the \code{estimand_function} argument. Both, the expression supplied to \code{estimand_text} and the \code{estimand_function}, need to return the estimand when evaluated on the data frame that results from applying the \link{draw_data} function to the design. Typically, the estimand is a function of potential outcomes. It can also be a function of the ID variables and pre-treatment covariates that have been built into the \code{population_object} using the \link{declare_population} function. If \code{level = "assignment"}, the estimand can also be a function of the treatment indicator and observed outcome. If an \code{estimand_text} is provided, there is no need to supply an \code{estimand_function} and vice versa.
#' 
#' @return An estimand object.
#'
#' @examples
#' 
#' ## Declare estimand using estimand_text
#' 
#' population <- declare_population(noise = "rnorm(n_)", size = 250)
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' 
#' estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
#' 
#' get_estimands(estimand, data = data)
#' 
#' ## Declare estimand using estimand_function
#' 
#' population <- declare_population(noise = "rnorm(n_)", size = 250)
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' 
#' my_estimand <- function(data) { with(data, mean(Y_Z_1 - Y_Z_0)) }
#' 
#' estimand <- declare_estimand(estimand_function = my_estimand, potential_outcomes = potential_outcomes)
#' 
#' get_estimands(estimand, data = data)
#' 
#' ## Declare estimand for subset of units
#' 
#' population <- declare_population(noise = "rnorm(n_)", 
#'                                  dem = "rbinom(n_, 1, .5)",
#'                                  size = 250)
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' 
#' estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", 
#'                              potential_outcomes = potential_outcomes,
#'                              subset = "dem == 1")
#' 
#' data <- draw_population(population)
#' 
#' get_estimands(estimand, data = data)
#'
#' ## Declare estimand for different levels
#' 
#' ### Estimand on population level 
#' 
#' population <- declare_population(noise = "rnorm(n_)", size = 250)
#' 
#' sampling <- declare_sampling(n = 100)
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#'                                                  
#' assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
#' 
#' estimand_population <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", 
#'                                          potential_outcomes = potential_outcomes)
#'                                          
#' estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, 
#'                                estimand = estimand_population)
#' 
#' design <- declare_design(population = population,
#'                          sampling = sampling, 
#'                          assignment = assignment, 
#'                          estimator = estimator, 
#'                          potential_outcomes = potential_outcomes)
#'                          
#' diagnosis <- diagnose_design(design,
#'                              population_draws = 2,
#'                              sample_draws = 2,
#'                              assignment_draws = 2)
#'                  
#' head(diagnosis$simulations)
#'
#' ### Estimand on sample level
#' 
#' estimand_sample <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", 
#'                                        potential_outcomes = potential_outcomes,
#'                                        estimand_level = "sample")
#'                                        
#' estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, 
#'                                estimand = estimand_sample)
#' 
#' design <- modify_design(design, estimator = estimator)
#' 
#' diagnosis <- diagnose_design(design,
#'                              population_draws = 2,
#'                              sample_draws = 2,
#'                              assignment_draws = 2)
#'                                                
#' head(diagnosis$simulations)
#' 
#' ### Estimand on assignment level
#' 
#' estimand_assignment <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", 
#'                                        potential_outcomes = potential_outcomes,
#'                                        estimand_level = "assignment")
#'                                        
#' estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, 
#'                                estimand = estimand_assignment)
#' 
#' design <- modify_design(design, estimator = estimator)
#' 
#' diagnosis <- diagnose_design(design,
#'                              population_draws = 2,
#'                              sample_draws = 2,
#'                              assignment_draws = 2)
#'                  
#' head(diagnosis$simulations)
#'
#' @export
declare_estimand <- function(estimand_function = NULL, estimand_text = NULL,
                             ..., estimand_level = "population", 
                             potential_outcomes, noncompliance = NULL, attrition = NULL,
                             condition_names = NULL,
                             subset = NULL, weights_variable_name = NULL, 
                             label = NULL, description = NULL) {
  
  estimand_options <- list(...)
  
  # Checks -------------------------------------------------
  potential_outcomes <- clean_inputs(potential_outcomes, "potential_outcomes", accepts_list = TRUE)
  noncompliance <- clean_inputs(noncompliance, "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(attrition, "attrition", accepts_list = FALSE)
  
  if(!is.null(weights_variable_name)){
    stop("Weighted estimands are not yet implemented. Please contact the authors if you are interested in using them.")
  }
  
  if(!is.null(estimand_function) & !is.null(estimand_text)){
    stop("Please provide either estimand_function or estimand_text.")
  }
  
  if(is.null(potential_outcomes)){
    stop("Please provide a potential_outcomes object. This is used to create the potential outcomes before calculating the estimand.")
  }
  
  if(!(estimand_level %in% c("population", "sample", "assignment"))){
    stop("Please set the argument estimand_level must be either 'population', 'sample', or 'assignment'.")
  }
  
  if(!is.null(condition_names)){
    condition_names <- clean_condition_names(condition_names)
  }
  
  if(!is.null(estimand_text)){
    
    if(class(estimand_text) != "character"){
      stop("Please provide a character string to the estimand_text argument.")
    }
    
    ## if no custom estimand is provided
    
    if(is.null(label)){
      label <- as.character(estimand_text)
    }
    
    if(!is.character(eval(estimand_text))){
      estimand_text <- quote(estimand_text)
    } else {
      estimand_text <- parse(text = estimand_text)
    }
    
    estimand_function_internal <- function(data){
      if(!is.null(subset))
        data <- subset(data, subset = eval(parse(text = subset)))
      ##if(!is.null(weights_variable_name))
      ##  estimator_options$weights <- data[, weights_variable_name]
      return(eval(estimand_text, envir = data))
    }
  } else {
    
    if(class(estimand_function) != "function"){
      stop("Please provide a function to the estimand_function argument.")
    }
    
    ## if a custom estimand is provided
    
    estimand_function_internal <- function(data){
      argument_names <- names(formals(estimand_function))
      options_internal <- list()
      if(!is.null(subset) & "subset" %in% argument_names)
        options_internal$subset <- with(data, eval(parse(text = subset)))
      if(!is.null(weights_variable_name) & "weights" %in% argument_names)
        options_internal$weights <- data[, weights_variable_name]
      if(length(estimand_options) > 0){
        for(i in 1:length(estimand_options)){
          if(names(estimand_options)[[i]] %in% argument_names){
            options_internal[[names(estimand_options)[[i]]]] <- estimand_options[[i]]
          }
        }
      }
      options_internal$data <- data
      
      return(do.call(estimand_function, args = options_internal))
    }
    
  }
  
  structure(list(estimand = estimand_function_internal, potential_outcomes = potential_outcomes, noncompliance = noncompliance, attrition = attrition,
                 estimand_level = estimand_level, condition_names = condition_names, label = label, description = description, 
                 call = match.call()), class = "estimand")
  
}

#' Get Estimands
#' 
#' @param estimand An estimand object or list of estimand objects, created with \code{\link{declare_estimand}}.
#' @param estimator An estimantor object or a list of estimator objects, created with \code{\link{declare_estimator}}.
#' @param data A data.frame, created by \code{\link{draw_sample}} or \code{\link{draw_population}} depending on the target of the estimate (sample or population).
#'
#' @export
get_estimands <- function(estimand = NULL, estimator = NULL, data){
  
  if(sum(is.null(estimand), is.null(estimator)) != 1){
    stop("Please provide either estimand(s) or estimator(s) and not both.")
  }
  
  if(!is.null(estimand)){
    
    if(class(estimand) == "list"){
      estimand_labels <- c(lapply(1:length(estimand), function(j) ifelse(is.null(estimand[[j]]$label), "", estimand[[j]]$label)), recursive = TRUE)
      estimand_labels[which(estimand_labels == "")] <- paste(substitute(estimand)[-1L])[which(estimand_labels == "")]
      estimand_levels <- sapply(estimand, function(x) x$estimand_level)
    } else {
      estimand_labels <- estimand$label
      estimand_levels <- estimand$estimand_level
      if(is.null(estimand_labels)){
        estimand_labels <- paste(substitute(estimand))
      }
    }
    
    ## if estimand sent
    estimand <- clean_inputs(estimand, object_class = "estimand", accepts_list = TRUE)
    
    estimands_list <- list()
    if(!is.null(estimand)){
      for(i in 1:length(estimand)){
        if(!is.null(estimand[[i]])){
          
          ## if there is an estimand defined
          has_potential_outcomes <- has_potential_outcomes(data = data, 
                                                           potential_outcomes = estimand[[i]]$potential_outcomes, 
                                                           attrition = estimand[[i]]$attrition, 
                                                           noncompliance = estimand[[i]]$noncompliance,
                                                           condition_names = estimand[[i]]$condition_names)
          
          if(has_potential_outcomes == TRUE){
            
            ## if PO's already exist, do not create them
            estimands_list[[i]] <- estimand[[i]]$estimand(data = data)
            
          } else {
            
            ## otherwise, use draw_potential_outcomes to create them
            estimands_list[[i]] <- estimand[[i]]$estimand(data = draw_potential_outcomes(data = data, potential_outcomes = estimand[[i]]$potential_outcomes,  
                                                                                         attrition = estimand[[i]]$attrition, noncompliance = estimand[[i]]$noncompliance,
                                                                                         condition_names = estimand[[i]]$condition_names))
            
          }
        } else {
          ## if there is NOT an estimand defined
          estimands_list[[i]] <- "no estimand"
        }
        if(!(is.numeric(estimands_list[[i]]) & length(estimands_list[[i]]) == 1 & is.vector(estimands_list[[i]]))){
          stop("Please set up your estimand function to return a scalar.") 
        }
      }
      
      estimands_df <- data.frame(estimator_label = "no estimator", 
                                 estimand_label = estimand_labels,
                                 estimand_level = estimand_levels,
                                 estimand = c(estimands_list, recursive = T),
                                 stringsAsFactors = FALSE)
      
    } else {
      ## if estimand is null (i.e. an estimator did not have an estimand)
      
      estimands_df <- data.frame(estimator_label = "no estimator", 
                                 estimand_label = "no estimand",
                                 estimand_level = "no estimand",
                                 estimand = "no estimand",
                                 stringsAsFactors = FALSE)
    }
    
  } else {
    
    ## when estimator is sent
    
    if(class(estimator) == "list"){
      estimator_labels <- lapply(1:length(estimator), function(j) estimator[[j]]$label)
      if(any(unlist(lapply(estimator_labels, is.null)))){
        estimator_object_labels <- paste(substitute(estimator)[-1L])
        estimator_labels <- lapply(1:length(estimator), function(j) {
          label <- estimator[[j]]$label
          if(is.null(label)){
            label <- estimator_object_labels[j]
          }
          return(label)})
      }
      
    } else if(class(estimator) == "estimator"){
      if(!is.null(estimator$label)){
        estimator_labels <- list(estimator$label)
      } else {
        estimator_labels <- list(paste(substitute(estimator)))
      }
    }
    
    estimator <- clean_inputs(estimator, object_class = "estimator", accepts_list = TRUE)
    
    estimands_list <- list()
    for(i in 1:length(estimator)){
      if(!is.null(estimator[[i]]$estimand)){
        estimands_list[[i]] <- get_estimands(estimand = estimator[[i]]$estimand, data = data)
        estimands_list[[i]]$estimator_label <- estimator_labels[[i]]
      } else {
        estimands_list[[i]] <- data.frame(estimator_label = estimator_labels[[i]], 
                                   estimand_label = "no estimand",
                                   estimand_level = "no estimand",
                                   estimand = "no estimand",
                                   stringsAsFactors = FALSE)
      }
    }
    estimands_df <- do.call(rbind, estimands_list)
    
  }
  
  return(estimands_df)
  
}

