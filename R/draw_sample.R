#' Draw a sample from the population
#' 
#' This function takes a data frame representing the population and returns a data frame of the data from a sample drawn from the population.
#' 
#' @param data A data.frame object representing the population to sample from, typically created by \code{draw_population}.
#' @param sampling A sampling object describing the sampling strategy created by \code{declare_sampling}.
#' 
#' @return a data.frame including only sampled units.
#' 
#' @examples
#' population <- declare_population(size = 850)
#' sampling <- declare_sampling(n=500)
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#'
#' head(smp_draw)
#'
#' @export
draw_sample <- function(data, sampling = NULL) {
  
  sampling <- clean_inputs(sampling, object_class = "sampling", accepts_list = FALSE)
  
  if(sampling$sampling == TRUE){
    
    # Construct strata and clusters if custom functions --------------------------------------------------
    
    if(!is.null(sampling$clustering_function)){
      data <- sampling$clustering_function(data = data)
    }
    
    if(!is.null(sampling$stratification_function)) { 
      data <- sampling$stratification_function(data = data)
    }
    
    # Draw the sample ------------------------------------------------------
    
    S <- draw_sample_indicator(data = data, sampling = sampling)
    
    if(!is.null(sampling$sampling_probability_function)){
      inclusion_probabilities <- get_sampling_probabilities(data = data, sampling = sampling)
      sampling_data <- data.frame(sampled = S, 
                                  inclusion_probabilities = inclusion_probabilities, 
                                  sampling_weights = 1/inclusion_probabilities)
    } else {
      sampling_data <- data.frame(sampled = S, 
                                  inclusion_probabilities = "unknown", 
                                  sampling_weights = 1)
    }
    
    data <- data.frame(data, sampling_data)
    
    sample_data <- data[data$sampled == 1, ]
    sample_data$sampled <- NULL
    
    return(sample_data)
    
  } else {
    
    data$inclusion_probabilities <- 1
    data$sampling_weights <- 1
    
    return(data)
    
  }
  
}

#' Draw a sample indicator from population
#' 
#' This function takes a data.frame object representing the population and returns a vector of sampling indicators, 1 for sampled and 0 for not sampled.
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}.
#' 
#' @return A vector of 0's and 1's indicating which population units are sampled.
#' 
#' @examples
#' population <- declare_population(size = 850)
#' sampling <- declare_sampling(n=500)
#' pop_draw <- draw_population(population = population)
#' pop_draw$S <- draw_sample_indicator(data = pop_draw, sampling = sampling)
#' 
#' table(pop_draw$S)
#' 
#' @export
draw_sample_indicator <- function(data, sampling) {
  
  sampling <- clean_inputs(sampling, object_class = "sampling", accepts_list = FALSE)
  
  if(sampling$sampling == TRUE){
    S <- sampling$sampling_function(data = data)
  } else {
    S <- rep(1, nrow(data))
  }
  
  return(S)
  
}

