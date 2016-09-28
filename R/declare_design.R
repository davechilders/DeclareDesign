#' Declare the design
#' 
#' This function gathers together the declarations for population, sampling, assignment, estimator(s), and potential outcomes. Design objects can be summarized with summary(my_design), diagnosed with diagnose_design(my_design), and modified with modify_design(my_design).
#' 
#' @param population A population object, as created by \code{\link{declare_population}}.
#' @param sampling A sampling object, as created by \code{\link{declare_sampling}}.
#' @param assignment An assignment object, as created by \code{\link{declare_assignment}}.
#' @param estimator An estimator object, as created by \code{\link{declare_estimator}}. Optionally a list of estimator objects.
#' @param potential_outcomes A potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. Optionally a list of potential_outcomes objects.
#' @param inputs An inputs object, as create by \code{\link{declare_inputs}}, with the data and code required to replicate the design.
#' @param label An optional design label.
#' @param description A description of the complete design in words.
#' 
#' @return A design object
#' 
#' @export
declare_design <- function(population, 
                           sampling = declare_sampling(sampling = FALSE), 
                           assignment = declare_assignment(assignment = FALSE), 
                           estimator = NULL,
                           potential_outcomes = declare_potential_outcomes(potential_outcomes = FALSE),
                           diagnosand = list(mean_estimand,
                                             mean_estimate,
                                             sd_estimate,
                                             bias,
                                             rmse,
                                             coverage,
                                             power,
                                             type_s_rate),
                           inputs = NULL, label = NULL, description = NULL) {
  
  ## do checks
  
  ## construct estimator labels
  
  if(class(estimator) == "list"){
    try(estimator_object_names <- paste(substitute(estimator)[-1L]), silent = TRUE)
    for(i in 1:length(estimator)){
      if(is.null(estimator[[i]]$label)){
        try(estimator[[i]]$label <- estimator_object_names[i], silent = TRUE)
      }
    }
  } else if(class(estimator) == "estimator"){
    if(is.null(estimator$label)){
      try(estimator$label <- paste(substitute(estimator)), silent = TRUE)
    }
  }
  
  # Checks -------------------------------------------------
  population <- clean_inputs(population, "population", accepts_list = FALSE)
  sampling <- clean_inputs(sampling, "sampling", accepts_list = FALSE)
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  estimator <- clean_inputs(estimator, "estimator", accepts_list = TRUE)
  potential_outcomes <- clean_inputs(potential_outcomes, c("potential_outcomes", "interference", "attrition"), accepts_list = TRUE)
  diagnosand <- clean_inputs(diagnosand, c("diagnosand"), accepts_list = TRUE)
  inputs <- clean_inputs(inputs, "inputs", accepts_list = FALSE)
  
  ## return object
  
  structure(list(population = population, sampling = sampling, assignment = assignment, estimator = estimator,
                 potential_outcomes = potential_outcomes, diagnosand = diagnosand, 
                 inputs = inputs, label = label, description = description,
                 call = match.call()), class = "design")
  
}

#' Modify the design
#' 
#' This function takes a design object and allows the user to modify one or more of the declarations that make up a design.
#' 
#' @param design A design object, created by \code{\link{declare_design}}.
#' @param population A population object, as created by \code{\link{declare_population}}.
#' @param sampling A sampling object, as created by \code{\link{declare_sampling}}.
#' @param assignment An assignment object, as created by \code{\link{declare_assignment}}.
#' @param estimator An estimator object, as created by \code{\link{declare_estimator}}. Optionally a list of estimator objects.
#' @param potential_outcomes A potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. Optionally a list of potential_outcomes objects.
#' @param diagnosand 
#' @param inputs An inputs object, as created by \code{\link{declare_inputs}}.
#' @param label An optional design label.
#' @param description A description of the modified design in words.
#' 
#' @return A design object
#' 
#' @export
modify_design <- function(design, population = NULL, sampling = NULL, assignment = NULL, estimator = NULL, 
                          potential_outcomes = NULL, diagnosand = NULL, inputs = NULL, label = NULL, description = NULL) {
  
  if(all(is.null(population), is.null(sampling), is.null(assignment), is.null(estimator), is.null(potential_outcomes),
         is.null(diagnosand), is.null(label))){
    warning("No part of the design was modified, so the original design is being returned.")
    return(design)
  }
  
  if(is.null(population) & !is.null(design$population)){
    population <- design$population
  }
  if(is.null(sampling) & !is.null(design$sampling)){
    sampling <- design$sampling
  }
  if(is.null(assignment) & !is.null(design$assignment)){
    assignment <- design$assignment
  }
  if(is.null(estimator) & !is.null(design$estimator)){
    estimator <- design$estimator
  }
  if(is.null(potential_outcomes) & !is.null(design$potential_outcomes)){
    potential_outcomes <- design$potential_outcomes
  }
  if(is.null(diagnosand) & !is.null(design$diagnosand)){
    diagnosand <- design$diagnosand
  }
  if(is.null(inputs) & !is.null(design$inputs)){
    inputs <- design$inputs
  }
  if(is.null(label) & !is.null(design$label)){
    label <- design$label
  }
  if(is.null(description) & !is.null(design$description)){
    description <- design$description
  }
  
  declare_design(population = population, sampling = sampling, assignment = assignment, estimator = estimator, 
                 potential_outcomes = potential_outcomes, diagnosand = diagnosand, 
                 inputs = inputs, label = label, description = description)
  
}




