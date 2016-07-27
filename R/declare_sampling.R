#' Declare the sampling strategy
#' 
#' @param probability The probability of inclusion in the sample. Must be a scalar that takes values between 0 and 1.  
#' @param strata_variable_name The name of the variable according to which stratified random sampling should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random sampling should be conducted.
#' @param n The number of units (or clusters) to be sampled.
#' @param strata_n A vector of the number of units in each stratum to be sampled.
#' @param strata_probabilities Probabilities of selection for each strata.
#' @param custom_sampling_function A custom function to draw a sample. At minimum it should take at least data as an argument and return a vector of ones and zeros, where ones indicate inclusion in the sample, zeros otherwise.
#' @param custom_stratification_function A custom function to sample within strata.
#' @param custom_clustering_function A custom function to sample clusters.
#' @param description A description of the sampling procedure in words.
#' @param sampling Indicates whether units are sampled from the population (TRUE) or the full population is used in the design (FALSE).
#' @param ... options passed to custom sampling function
#' 
#' @return a \code{sampling} object
#'
#' @export
declare_sampling <- function(probability = NULL,
                             strata_variable_name = NULL, 
                             cluster_variable_name = NULL,
                             n = NULL, 
                             strata_n = NULL, 
                             strata_probabilities = NULL,
                             custom_sampling_function = NULL,
                             custom_stratification_function = NULL,
                             custom_clustering_function = NULL,
                             description = NULL,
                             sampling = TRUE,
                             ...) {
  
  # Determine assignment type
  sampling_type <- "simple"  ## what should this be called. Simple? 
  if(!is.null(strata_variable_name)) {sampling_type <- "stratified"}
  if(!is.null(cluster_variable_name)) {sampling_type <- "clustered"}
  if(!is.null(cluster_variable_name) & !is.null(strata_variable_name)) {
    sampling_type <- "stratified and clustered"
  }
  if(!sampling){
    sampling_type <- "none"
  }
  
  # Checks ------------------------------------------------------------------
  if(sampling_type == "stratified" & !is.null(n)){
    stop("Please do not specify n in a stratified sampling design.  Use strata_n or strata_probabilities instead.")
  }
  
  if(!is.null(custom_stratification_function) & !is.character(strata_variable_name)){
    stop("If you supply a custom strata function, you must supply the name of the strata variable.")
  }
  
  if(!is.null(custom_clustering_function) & !is.character(cluster_variable_name)){
    stop("If you supply a custom cluster function, you must supply the name of the cluster variable.")
  }

  
  if(!is.null(custom_sampling_function)){
    custom_sampling_function_options <- list(...)
    argument_names <- names(formals(custom_sampling_function))
    if(!is.null(strata_variable_name) & "strata_variable_name" %in% argument_names)
      custom_sampling_function_options$strata_variable_name <- strata_variable_name
    if(!is.null(cluster_variable_name) & "cluster_variable_name" %in% argument_names)
      custom_sampling_function_options$cluster_variable_name <- cluster_variable_name
    if(!is.null(n) & "n" %in% argument_names)
      custom_sampling_function_options$n <- n
    if(!is.null(strata_n) & "strata_n" %in% argument_names)
      custom_sampling_function_options$strata_n <- strata_n
    if(!is.null(strata_probabilities) & "strata_probabilities" %in% argument_names)
      custom_sampling_function_options$strata_probabilities <- strata_probabilities
  }
  
  
  
  if(is.null(custom_sampling_function)){
    return.object <- list(probability = probability,
                          strata_variable_name = strata_variable_name,
                          cluster_variable_name = cluster_variable_name,
                          n = n,
                          strata_n = strata_n,
                          strata_probabilities = strata_probabilities,
                          sampling_type = sampling_type,
                          custom_stratification_function = custom_stratification_function,
                          custom_clustering_function = custom_clustering_function,
                          description = description,
                          call = match.call())
  } else {
    return.object <- list(
      custom_sampling_function = custom_sampling_function,
      custom_sampling_function_options = custom_sampling_function_options,
      sampling_type = "custom", description = description,
      call = match.call())
  }
  class(return.object) <- "sampling"
  return(return.object)
}

