
#' Calculate inclusion probabilties
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}.
#' @return A matrix of probabilities of selection.
#' @examples
#' population <- declare_population(size = 850)
#' sampling <- declare_sampling(n=500)
#' pop_draw <- draw_population(population = population)
#' sampling_probabilities <- get_sampling_probabilities(data = pop_draw, 
#'                                                      sampling = sampling)
#' head(sampling_probabilities)
#' @export
get_sampling_probabilities <- function(data, sampling){
  
  sampling <- clean_inputs(sampling, object_class = "sampling", accepts_list = FALSE)
  
  if(sampling$sampling == TRUE){
    S_prob <- sampling$sampling_probability_function(data = data)
  } else {
    S_prob <- rep("unknown", nrow(data))
  }
  
  return(S_prob)
}
