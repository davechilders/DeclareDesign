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
    
    if(!is.null(assignment[[i]]$custom_clustering_function)){
      data[, assignment[[i]]$cluster_variable_name] <- assignment[[i]]$custom_clustering_function(data = data)
    }
    
    if(!is.null(assignment[[i]]$custom_blocking_function)) { 
      data[, assignment[[i]]$block_variable_name] <- assignment[[i]]$custom_blocking_function(data = data)
    }
    
    # Assign treatment and reveal outcomes ------------------------------------------------  
    
    if(assignment[[i]]$assignment_type == "existing assignment"){
      data[, assignment[[i]]$assignment_variable_name] <- data[, assignment[[i]]$existing_assignment_variable_name]
    } else {
      
      ## if the treatment is created using assign_treatment_indicator, rather than using an existing assignment variable
      
      data[, assignment[[i]]$assignment_variable_name] <- assign_treatment_indicator(assignment = assignment[[i]], data = data)
      
      if(assignment[[i]]$assignment_type != "custom" & assignment[[i]]$assignment_type != "existing assignment") {
        
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_probabilities", sep="_")] <- get_observed_assignment_probabilities(assignment_variable_name = assignment[[i]]$assignment_variable_name,
                                                                                                                                             assignment = assignment[[i]], data = data)
        
        data[, paste(assignment[[i]]$assignment_variable_name,"assignment_weights", sep="_")] <- 1/data[, paste(assignment[[i]]$assignment_variable_name, "assignment_probabilities", sep="_")]
        
        ## only reveal assignment_sampling_weights if there are sampling probabilities
        if("inclusion_probabilities" %in% colnames(data)){
          
          data[, paste(assignment[[i]]$assignment_variable_name,"assignment_inclusion_probabilities", sep="_")] <- 
            data[, paste(assignment[[i]]$assignment_variable_name, "assignment_probabilities", sep="_")] * data[, "inclusion_probabilities"]
          
          data[, paste(assignment[[i]]$assignment_variable_name, "assignment_sampling_weights", sep="_")] <- 
            1/data[, paste(assignment[[i]]$assignment_variable_name, "assignment_inclusion_probabilities", sep="_")]
        }
      }
    }
    
    if(!is.null(assignment[[i]]$custom_transform_function) | !is.null(assignment[[i]]$transform_options)){
      if(is.null(assignment[[i]]$custom_transform_function)){
        data <- default_transform_function(data = data, options = assignment[[i]]$transform_options,
                                           assignment_variable_name = assignment[[i]]$assignment_variable_name)
      } else {
        data <- assignment[[i]]$custom_transform_function(data = data, options = assignment[[i]]$transform_options,
                                                          assignment_variable_name = assignment[[i]]$assignment_variable_name)
      }
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
  
  ## should be expanded to take either a assignment object or a function
  
  N <- nrow(data)
  
  block_variable <- data[,assignment$block_variable_name]
  cluster_variable <- data[,assignment$cluster_variable_name]
  
  condition_names <- assignment$condition_names
  baseline_condition <- assignment$baseline_condition
  m <- assignment$m
  m_each <- assignment$m_each
  probability_each <- assignment$probability_each
  block_m <- assignment$block_m
  block_m_each <- assignment$block_m_each
  block_probabilities <- assignment$block_probabilities
  assignment_type <- assignment$assignment_type
  
  # For custom random assignment functions
  if(is.null(assignment_type)){
    assignment_type <- "custom"
  }
  
  if(!is.null(assignment$custom_assignment_function)){
    if("data" %in% names(formals(assignment$custom_assignment_function))){
      assignment$custom_assignment_function_options$data <- data
    }
    if("n_" %in% names(formals(assignment$custom_assignment_function))){
      assignment$custom_assignment_function_options$n_ <- nrow(data)
    }
    
      Z <- do.call(assignment$custom_assignment_function, 
                   args = assignment$custom_assignment_function_options)
  } 
  
  # For complete random assignment designs
  if(assignment_type=="complete"){
    Z <- complete_assignment(N = N,
                             m = m,
                             m_each = m_each,
                             probability_each = probability_each, 
                             condition_names = condition_names,
                             baseline_condition=baseline_condition)
  }
  
  # For block random assignment designs
  if(assignment_type=="blocked"){
    Z <- blocked_assignment(block_variable=block_variable, 
                            block_m = block_m,
                            block_m_each = block_m_each,
                            probability_each = probability_each,
                            block_probabilities=block_probabilities,
                            condition_names = condition_names,
                            baseline_condition=baseline_condition)
  }
  
  # For cluster random assignment designs
  if(assignment_type=="clustered"){
    Z <- clustered_assignment(cluster_variable=cluster_variable, 
                              m = m, 
                              m_each = m_each,
                              probability_each = probability_each,
                              condition_names = condition_names,
                              baseline_condition=baseline_condition)
  }
  
  # For blocked and clustered random assignment designs
  if(assignment_type=="blocked and clustered"){
    Z <- blocked_and_clustered_assignment(cluster_variable=cluster_variable,
                                          block_variable=block_variable, 
                                          block_m_each=block_m_each,
                                          block_probabilities=block_probabilities,
                                          probability_each = probability_each,
                                          condition_names = condition_names,
                                          baseline_condition=baseline_condition)
  }
  
  if(assignment_type=="none"){
    Z <- rep(1, N)
  }
  
  return(Z)
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
  
  prob_mat <- get_assignment_probabilities(assignment = assignment, data = data)
  prob_obs <- rep(NA, nrow(data))
  condition_names <- unique(data[,assignment_variable_name])
  for(i in 1:length(condition_names)){
    prob_obs[data[,assignment_variable_name]==condition_names[i]] <- 
      prob_mat[data[,assignment_variable_name]==condition_names[i], paste0("prob_", condition_names[i])]
  }
  return(prob_obs)  
}



