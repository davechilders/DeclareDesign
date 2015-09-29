
#' Calculate probabilties of assignment
#'
#' Description
#' @param design A design object created by \code{\link{declare_design}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A matrix of probabilities of assignment to treatment.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # design <- declare_design(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # mock$Z        <- assign_treatment(design, data = mock)
#' # design_probs <- get_design_probs(design, mock)
#' 
#' head(design_probs)
#' @export
get_inclusion_probs <- function(sampling, population_data){
  
  N <- nrow(population_data)  
  strata_variable_name <- sampling$strata_variable_name
  cluster_variable_name <- sampling$cluster_variable_name
  
  if(!is.null(strata_variable_name)){
    strata_var <- population_data[,strata_variable_name]  
  }else{
    strata_var <- NULL
  }
  
  if(!is.null(cluster_variable_name)){
    clust_var <- population_data[,cluster_variable_name]
  }else{
    clust_var <- NULL
  }
  
  
  m <- sampling$m
  prob <- sampling$prob
  strata_m <- sampling$strata_m
  strata_prob <- sampling$strata_prob
  sampling_type <- sampling$sampling_type
  
  if(sampling_type=="complete"){
    probs <- complete_sample_probs(N = N, m = m, prob = prob)
  }
  
  if(sampling_type=="stratified"){
    probs <- stratified_sample_probs(strata_var = strata_var, prob = prob, strata_m = strata_m, strata_prob = strata_prob)
  }
  
  if(sampling_type=="clustered"){
    probs <- cluster_sample_probs(clust_var = clust_var, m = m, prob = prob)
  }
  
  if(sampling_type=="stratified and clustered"){
    probs <- stratified_and_clustered_sample_probs(clust_var = clust_var, strata_var = strata_var, strata_m = strata_m, prob = prob, strata_prob = strata_prob)
  }
  
  return(probs)
}

#' @export
complete_sample_probs <- function(N, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  prob_mat <- complete_ra_probs(N = N, m = m, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

#' @export
stratified_sample_probs <- function(strata_var, prob = NULL, strata_m = NULL, strata_prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  prob_mat <- block_ra_probs(block_var = strata_var, block_m = strata_m, block_prob = block_prob, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}


#' @export
cluster_sample_probs <- function(clust_var, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  prob_mat <- cluster_ra_probs(clust_var = clust_var, m = m, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

#' @export
stratified_and_clustered_sample_probs <- function(clust_var, strata_var, strata_m = NULL, prob = NULL, strata_prob = NULL){
  
  prob_each <- NULL
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  # Must do someday
  # block_m, strata_m
  prob_mat <- blocked_and_clustered_ra_probs(clust_var = clust_var, block_var = strata_var, 
                           block_m = strata_m, prob_each = prob_each, 
                           block_prob = block_prob, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}