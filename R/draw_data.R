#' Draw mock data based on a design
#'
#' @param design A design object created by \code{\link{declare_design}}. 
#'
#' @export
draw_data <- function(design) {
  
  # Checks -------------------------------------------------
  design <- clean_inputs(design, "design", accepts_list = FALSE)
  
  population <- design$population
  potential_outcomes <- design$potential_outcomes
  sampling <- design$sampling
  assignment <- design$assignment
  
  data <- draw_population(population = population, potential_outcomes = potential_outcomes)
  
  if(!is.null(sampling)){
    data <- draw_sample(data = data, sampling = sampling)
  }
  
  if(!is.null(assignment)){ 
    data <- assign_treatment(data = data, assignment = assignment)
    
    data <- draw_outcome(data = data, potential_outcomes = potential_outcomes)
  }
  
  return(data)
  
}
