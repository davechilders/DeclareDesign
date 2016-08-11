#' Assign treatment
#' 
#' This function takes a data.frame and an assignment object and returns a data.frame with a treatment assignment, probabilities of assignment, and inverse probability weights.
#'
#' @param data A data.frame, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}. 
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @return A data.frame with new columns added for a treatment assignment, probabilities of assignment, and inverse probability weights.
#' 
#' @examples 
#' population <- declare_population(noise = declare_variable(), size = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' assignment <- declare_assignment(potential_outcomes = potential_outcomes)
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' 
#' head(smp_draw)
#' 
#' @export
assign_treatment <- function(data, assignment) {
  
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  
  for(i in 1:length(assignment)){
    
    # Make clusters and blocks ------------------------------------------------  
    
    if(!is.null(assignment[[i]]$clustering_function)){
      data[, assignment[[i]]$cluster_variable_name] <- assignment[[i]]$clustering_function(data = data)
    }
    
    if(!is.null(assignment[[i]]$blocking_function)) { 
      data[, assignment[[i]]$block_variable_name] <- assignment[[i]]$blocking_function(data = data)
    }
    
    # Assign treatment and reveal outcomes ------------------------------------------------  
    
    if(!is.null(assignment[[i]]$existing_assignment_variable_name)){
      data[, assignment[[i]]$assignment_variable_name] <- data[, assignment[[i]]$existing_assignment_variable_name]
      data[, paste(assignment[[i]]$assignment_variable_name,"assignment_probabilities", sep="_")] <- 
        data[, assignment[[i]]$existing_assignment_probabilities_variable_name]
      data[, paste(assignment[[i]]$assignment_variable_name,"assignment_weights", sep="_")] <- 
        1 / data[, assignment[[i]]$existing_assignment_probabilities_variable_name]
    } else {
      
      ## if the treatment is created using assign_treatment_indicator, rather than using an existing assignment variable
      
      data[, assignment[[i]]$assignment_variable_name] <- assign_treatment_indicator(assignment = assignment[[i]],
                                                                                     data = data)
      
      if(!is.null(assignment[[i]]$assignment_probability_function)){
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_probabilities", sep="_")] <- 
          get_observed_assignment_probabilities(assignment_variable_name = assignment[[i]]$assignment_variable_name,
                                                assignment = assignment[[i]], data = data)
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_weights", sep="_")] <- 
          1/data[, paste(assignment[[i]]$assignment_variable_name, "assignment_probabilities", sep="_")]
      } else {
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_probabilities", sep="_")] <- "unknown"
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_weights", sep="_")] <- 1
      }
      
      ## only reveal assignment_sampling_weights if there are sampling probabilities
      if("inclusion_probabilities" %in% colnames(data)){
        
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_inclusion_probabilities", sep="_")] <- 
          data[, paste(assignment[[i]]$assignment_variable_name, "assignment_probabilities", sep="_")] * 
          data[, "inclusion_probabilities"]
        
        data[, paste(assignment[[i]]$assignment_variable_name, "assignment_sampling_weights", sep="_")] <- 
          1/data[, paste(assignment[[i]]$assignment_variable_name, "assignment_inclusion_probabilities", sep="_")]
      }
    }
    
    if(!is.null(assignment[[i]]$transform_function)){
      # if !default_transform_function,  transform
      if(is.null(assignment[[i]]$transform_options$assignment_variable_name) & 
         "assignment_variable_name" %in% names(formals(assignment[[i]]$transform_function))){
        assignment[[i]]$transform_options$assignment_variable_name <- assignment[[i]]$assignment_variable_name
      }
      assignment[[i]]$transform_options$data <- data
      
      data <- do.call(assignment[[i]]$transform_function, args = assignment[[i]]$transform_options)
    }
    
  }
  
  return(data)
  
}

#' Assign treatment status
#' 
#' This function takes a data.frame and an assignment object and returns an assignment vector.  Users will often prefer to use \code{\link{assign_treatment}}.
#'
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' 
#' @return A random assignment vector of length N.
#'
#' @examples 
#' population <- declare_population(noise = declare_variable(), size = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' assignment <- declare_assignment(potential_outcomes = potential_outcomes)
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' Z <- assign_treatment_indicator(data = smp_draw, assignment=assignment)
#' table(Z)
#' 
#' @export
assign_treatment_indicator <- function(data, assignment) {
  
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = FALSE)
  
  return(assignment$assignment_function(data = data))
  
}

#' Reveal probabilties of assignment to realized treatment conditions
#'
#' Description
#' 
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param assignment_variable_name The name of the treatment assignment variable in data.
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @return A vector probabilities of assignment to treatment.
#' @examples 
#' population <- declare_population(noise = declare_variable(), size = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z")
#' assignment <- declare_assignment(potential_outcomes = potential_outcomes)
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw$Z <- assign_treatment_indicator(data = smp_draw, assignment=assignment)
#' 
#' probs <- get_observed_assignment_probabilities(data = smp_draw, 
#'                         assignment_variable_name= "Z",
#'                         assignment=assignment)
#'                         
#' table(probs)                         
#' 
#' @export
get_observed_assignment_probabilities <- function(data, assignment_variable_name, assignment){
  
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = FALSE)
  
  return(assignment$assignment_probability_function(data = data, assignment_variable_name = assignment_variable_name))
  
}



