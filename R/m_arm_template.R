#' M-arm Template
#' 
#' @param N A scalar (must be an integer) indicating the total population size.
#' @param n A scalar (must be an integer) indicating the total sample size. By default, n = N.
#' @param m A scalar (must be an integer) indicating the number of arms. Note that m > 1 for any experiment.
#' @param probability_each A vector of length m, indicating the probability of assignment to each arm. The first element indicates the control group and subsequent elements indicate each of the treatment groups in ascending order (i.e. treatment 1 to treatment m-1). The elements in the vector must sum to 1. By default, units have a 1/m probability of assignment to each experimental group.
#' @param mu_Y0 A scalar indicating the expectation of the untreated potential outcome (control group).
#' @param ATEs A vector of length m-1 indicating the desired average treatment effect ATE for each experimental group relative to control (mu_Y0).
#' @param noise_scale A scalar or a vector of length m indicating the standard deviation of individual-level noise for each experimental group. If it is a scalar, the standard deviation of individual-level noise is constant across all treatment groups. If it is a vector, the standard deviation of noise is group-specific. The elements in the vector correspond to control and then the treatment groups in ascending order.
#' @param coef_X A scalar coefficient on a covariate, X, in the potential outcomes function.  By default, coef_X = 0.
#' @param location_scale_X A vector of length two indicating the mean and standard deviation of covariate X. By default, location_scale_X = 0. 
#' @param cov_adjustment A logical argument indicating whether covariate adjustment should be used in estimation. By default, cov_adjustment = F. 
#' @param block_var_vector For non-clustered designs, a vector indicating the block to which a unit belongs. For cluster-randomized experiments, the vector should be at the cluster level (of length n_clust_pop).
#' @param block_var_probs For non-clustered designs, a vector specifying the distribution of a multinomial blocking variable (probability per level). The elements in the vector must be between 0 and 1 and sum to 1.
#' @param blocked_RA A logical argument indicating whether blocked random assignment should be used. By default, blocked_RA = F.
#' @param block_prob_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param n_clust_pop A scalar (must be an integer) indicating the number of equal-sized clusters in the population.
#' @param n_clust_samp A scalar (must be an integer) indicating the number of equal-sized clusters in the sample. 
#' @param n_blocks For clustered designs, users can specify the number of equal-sized blocks in the population.
#' @param SE Indicates the standard error estimator. Options include "OLS" for classical OLS standard errors, "robust" for Huber-White heteroskedasticity-robust standard errors, or "clustered" for robust clustered standard errors. By default, the "robust" standard errors are used for non-clustered designs and "clustered" standard errors are used for clustered designs.
#' @param custom_pop_list Allows users to input cluster-assigned designs with clusters and blocks of varying sizes. Users should input a list of three vectors: (1) individuals using rep(1, N), where N is the population size; (2), clusters, a vector containing the number per cluster such that the sum of the vector is N; and (3) blocks, a vector containing the number of clusters in each block such that the sum of the vector is equal to the length of the cluster vector. All elements must be positive integers.
#' 
#' @return A design
#' 
#' @examples 
#' \dontrun{
#' design_1 <- m_arm_template(N = 1000, n = 500, m = 4, probability_each = c(.2, .3, .15, .35), 
#'                            mu_Y0 = .3, ATEs = c(-.2, .04, .32), noise_scale = 1)
#' design_2 <- m_arm_template(N = 200, mu_Y0 = .2, ATEs = .06, binary_POs = T)
#' design_3 <- m_arm_template(N = 400, n = 300, m = 3, mu_Y0 = 2, ATEs = c(-.38, -.12), 
#'                            block_var_probs = c(.2, .25, .25, .3), block_noise_coef = 2, 
#'                            blocked_RA = T)
#' design_4 <- m_arm_template(N = 600, n_clust_pop = 100, n_clust_samp = 80, ATEs = .25, coef_X = .8, 
#'                            cov_adjustment = T)
#' design_5 <- m_arm_template(N = 800, n_clust_pop = 200, m = 3, mu_Y0 = .65, ATEs = c(-.1, -.05), 
#'                            probability_each = c(.25, .25, .5), binary_POs = T)
#' design_6 <- m_arm_template(N = 500, n_clust_pop = 100, mu_Y0 = .4, ATEs = -.1, block_noise_coef = 2, 
#'                            clust_noise_coef = 2, n_blocks = 5, binary_POs = T, blocked_RA = T)
#' design_7 <- m_arm_template(N = 600, n_clust_pop = 120, m = 3, custom_pop_list = list(inds = rep(1, 600), 
#'                            clusters = rep(c(6, 6, 3, 5, 5), 24), blocks = c(rep(3, each = 40))), 
#'                            ATEs = c(-.15, .2),  mu_Y0 = .4, clust_noise_coef = 2, binary_POs = T, 
#'                            noise_scale = 1)
#'}                            
#' @export

make_block_noise <- function(block_var){
  if(!is.numeric(block_var)){bv <- as.numeric(factor(block_var))}
  else{bv <- block_var}
  rn <- rnorm(length(unique(bv)))
  return(rn[bv])
}

m_arm_template <- function(N, 
                           n = N, 
                           m = 2,
                           probability_each = rep(1/m, m), 
                           mu_Y0 = 0, 
                           ATEs = 0, 
                           noise_scale = 1, 
                           coef_X = 0,
                           binary_POs = F,
                           location_scale_X = c(0, 1),
                           cov_adjustment = F,
                           block_var_probs = c(.5, .5),
                           block_var_vector = NULL,
                           blocked_RA = F,
                           block_noise_coef = 0,
                           block_prob_each = NULL,
                           n_clust_pop = NULL,
                           n_blocks = 1,
                           clust_noise_coef = 1,
                           n_clust_samp = n_clust_pop,
                           custom_pop_list = NULL,
                           SE = ifelse(is.null(n_clust_pop), "robust", "clustered")){


# Checks ------------------------------------------------------------------
  
  if(n > N){
    stop("Sample (n) cannot be larger than population (N)!")
  }
  if(m < 2){
    stop("Experiment requires that m > 1.")
  }
  if(length(ATEs) != (m - 1)) {
    stop("Vector of ATEs must be of length m-1 (one ATE per treatment group).")
  }
  if(length(noise_scale) != m  &  length(noise_scale)!= 1) {
    stop("Vector of noise scale must be of length m or 1!")
  }
  if(sum(noise_scale < 0) > 0| clust_noise_coef < 0 | block_noise_coef < 0 ){
    stop("All noise-scaling inputs must be zero or positive.")
  }
  if(sum(block_var_probs) != 1){
    stop("Block_var_probs must sum to 1.")
  }
  if(binary_POs == TRUE & sum(mu_Y0 + ATEs > 1) > 0 | sum(mu_Y0 + ATEs < 0) > 0){
    stop("All combinations of mu_Y0 + ATEs must be between 0 and 1.")
  }
  if(!is.null(n_clust_pop) & !is.null(n_clust_samp)){
    if(n_clust_samp > n_clust_pop){
      stop("Number of clusters in the sample cannot exceed the number of clusters in the population.")
    }
  }


# Declare Design Objeccts -------------------------------------------------


  if(is.null(n_clust_pop)){  
    if(is.null(block_var_vector)){
      population    <- declare_population(
        noise = declare_variable(location_scale = c(0, 1)),
        X = declare_variable(location_scale = location_scale_X),
        mu_Y0 = mu_Y0,
        block_var = declare_variable(type = "multinomial",
                                     probabilities = block_var_probs, outcome_categories = 1:length(block_var_probs)),
        block_noise = "make_block_noise(block_var)",
        size = N)
    }
    
    if(!is.null(block_var_vector)){
      population    <- declare_population(
        noise = declare_variable(location_scale = c(0, 1)),
        X = declare_variable(location_scale = location_scale_X),
        mu_Y0 = mu_Y0,
        block_var = block_var_vector,
        block_noise = "make_block_noise(block_var)",
        size = N, super_population = T)
    }
    
    if(binary_POs == FALSE){
      write_PO_formula <- function(ATEs, noise_scale, block_noise_coef){
        scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
        tab <- cbind(ATEs, scale)
        
        POs <- paste0("Y ~ mu_Y0 + ", noise_scale[1], " * noise * (Z == 'control') +", 
                      paste(sapply(1:nrow(tab), function(j) {
                        paste0(tab[j, 1], " * (Z == 'treatment",  j, "') + ", 
                               tab[j, 2], " * noise * (Z == 'treatment", j, "')")}), 
                        collapse = " + "), " + X * ", coef_X, "+ block_noise *", block_noise_coef)
        
        return(POs)
      }
      
      
      potential_outcomes <- declare_potential_outcomes(
        condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
        formula = as.formula(write_PO_formula(ATEs = ATEs, noise_scale = noise_scale,
                                              block_noise_coef = block_noise_coef)))
    }
    
    if(binary_POs == T){
      
      write_binary_PO_formula <- function(ATEs, noise_scale, block_noise_coef){
        scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
        tab <- cbind(ATEs, scale)
        cb_noise <- paste0("block_noise * ", block_noise_coef)
      
        POs <- paste0("Y ~ 1 * ((Z == 'control') * (", noise_scale[1], " * noise +", cb_noise,
                      "+ qnorm(",mu_Y0, ", mean(", noise_scale[1], " * noise +", cb_noise,")",
                      ", sd(", noise_scale[1], " * noise +", cb_noise,"))) +",
                      paste(sapply(1:nrow(tab), function(j) {
                        paste0("(Z == 'treatment",  j, "') * (", 
                               tab[j, 2], " * noise +", cb_noise, " + qnorm((", mu_Y0, "+", tab[j,1], 
                               "), mean(", tab[j, 2], " * noise +", cb_noise,
                               "), sd(", tab[j, 2], " * noise +", cb_noise,")))")}), 
                        collapse = " + "), " > 0)")
        return(POs)
      }
      
      potential_outcomes <- declare_potential_outcomes(
        condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
        formula = as.formula(write_binary_PO_formula(ATEs = ATEs, noise_scale = noise_scale,
                                                     block_noise_coef = block_noise_coef)))
    }
    
    if(N == n){
      sampling <- declare_sampling(sampling = F)
    }
    
    if(N != n){
      sampling <- declare_sampling(n = n)
    }
    
    if(!blocked_RA){
      assignment   <- declare_assignment(probability_each = probability_each, 
                                         potential_outcomes = potential_outcomes)
    }
    
    if(blocked_RA){
      assignment   <- declare_assignment(block_variable_name = "block_var",
                                         block_probabilities = block_prob_each,
                                         potential_outcomes = potential_outcomes)
    }
    
  }
  if(!is.null(n_clust_pop)){
    if(is.null(custom_pop_list)){
      population    <- declare_population(
        individual = list(noise_ind = declare_variable(),
                          X = declare_variable(location_scale = location_scale_X)),
        cluster = list(noise_clust = declare_variable()),
        block = list(noise_block = declare_variable()),
        size = c(N, n_clust_pop, n_blocks))}
    
    if(!is.null(custom_pop_list)){
      population    <- declare_population(
        individual = list(noise_ind = declare_variable(),
                          X = declare_variable(location_scale = location_scale_X)),
        cluster = list(noise_clust = declare_variable()),
        block = list(noise_block = declare_variable()),
        size = custom_pop_list)}
    
    if(binary_POs == F){
      write_PO_formula <- function(ATEs, noise_scale, clust_noise_coef, block_noise_coef){
        scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
        tab <- cbind(ATEs, scale)
        
        POs <- paste0("Y ~ ", mu_Y0, " + ", noise_scale[1], " * noise_ind * (Z == 'control') +", 
                      paste(sapply(1:nrow(tab), function(j) {
                        paste0(tab[j, 1], " * (Z == 'treatment",  j, "') + ", 
                               tab[j, 2], " * noise_ind * (Z == 'treatment", j, "')")}), 
                        collapse = " + "), " + X * ", coef_X, " + noise_clust * ", clust_noise_coef,
                      "+ noise_block * ", block_noise_coef)
        
        return(POs)
      }
      
      potential_outcomes <- declare_potential_outcomes(
        condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
        formula = as.formula(write_PO_formula(ATEs = ATEs, noise_scale = noise_scale, 
                                              clust_noise_coef = clust_noise_coef,
                                              block_noise_coef = block_noise_coef)))
    }
    
    if(binary_POs == TRUE){
      write_binary_PO_formula <- function(ATEs, noise_scale, clust_noise_coef, block_noise_coef){
        scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
        tab <- cbind(ATEs, scale)
        cb_noise <- paste0("noise_clust * ", clust_noise_coef, " + noise_block * ", block_noise_coef)
        
        POs <- paste0("Y ~ 1 * ((Z == 'control') * (", noise_scale[1], " * noise_ind +", cb_noise,
                      "+ qnorm(",mu_Y0, ", mean(", noise_scale[1], " * noise_ind +", cb_noise,")",
                      ", sd(", noise_scale[1], " * noise_ind +", cb_noise,"))) +",
                      paste(sapply(1:nrow(tab), function(j) {
                        paste0("(Z == 'treatment",  j, "') * (", 
                               tab[j, 2], " * noise_ind +", cb_noise, " + qnorm((", mu_Y0, "+", tab[j,1], 
                                "), mean(", tab[j, 2], " * noise_ind +", cb_noise,
                                "), sd(", tab[j, 2], " * noise_ind +", cb_noise,")))")}), 
                        collapse = " + "), " > 0)")
      
        return(POs)
      }
      
      potential_outcomes <- declare_potential_outcomes(
        condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
        formula = as.formula(write_binary_PO_formula(ATEs = ATEs, noise_scale = noise_scale,
                                                     clust_noise_coef = clust_noise_coef, 
                                                     block_noise_coef = block_noise_coef)))
    }
    
    if(n_clust_pop == n_clust_samp){
      sampling <- declare_sampling(sampling = F)
    }
    
    if(n_clust_pop != n_clust_samp){
      sampling <- declare_sampling(n = n_clust_samp,
                                   cluster_variable_name = "cluster_ID")
    }
    
    
    if(!blocked_RA){
      assignment   <- declare_assignment(cluster_variable_name = "cluster_ID", 
                                         potential_outcomes = potential_outcomes,
                                         probability_each = probability_each)
    }
    
    if(blocked_RA){
      assignment   <- declare_assignment(block_variable_name = "block_ID",
                                         cluster_variable_name = "cluster_ID", 
                                         potential_outcomes = potential_outcomes)
    }
  }  
    
  
    estimand_function <- function(data, treatment_num){
      return(mean(data[,paste0("Y_Z_treatment", treatment_num)] - data$Y_Z_control))
    }
    
    if(!cov_adjustment){estimator_formula = "Y ~ Z"}
    if(cov_adjustment){estimator_formula = "Y ~ Z + X"}
  
    estimator_function <- function(data, treatment_num, SE = "robust", cluster_ID = NULL){
      mod <- lm(as.formula(estimator_formula), weights = Z_assignment_weights, data = data)
      if(SE == "OLS"){
        estimates <- get_regression_coefficient(model = mod, 
                                                coefficient_name = paste0("Ztreatment", treatment_num))
      }
      if(SE == "robust"){
        estimates <- get_regression_coefficient_robust(model = mod,
                                                       coefficient_name = paste0("Ztreatment", treatment_num))
      }
      if(SE == "clustered"){
        estimates <- get_regression_coefficient_clustered(model = mod,
                                                          data = data,
                                                          coefficient_name = paste0("Ztreatment", treatment_num),
                                                          cluster_name = cluster_ID)
      }
      return(estimates)
    }
  
  
  make_estimator <- function(treatment_num, cluster_ID, SE){
    estimand  <- declare_estimand(estimand_function  = estimand_function, 
                                  treatment_num = treatment_num,
                                  potential_outcomes = potential_outcomes)
    
    estimator <- declare_estimator(estimates = estimator_function,
                                   treatment_num = treatment_num,
                                   cluster_ID = cluster_ID,
                                   SE = SE,
                                   estimand = estimand,
                                   label = paste0("ATE_hat_treatment", treatment_num))
    return(estimator)
  }
  
  if(is.null(n_clust_pop)){
    estimator_list <- lapply(X = 1 : length(ATEs), FUN = make_estimator, SE = SE, cluster_ID = NULL)
  }
  if(!is.null(n_clust_pop)){
    estimator_list <- lapply(X = 1 : length(ATEs), FUN = make_estimator, SE = SE, cluster_ID = "cluster_ID")
  }


# Declare Design ----------------------------------------------------------


  my_design <- declare_design(
    population         = population, 
    potential_outcomes = potential_outcomes, 
    sampling           = sampling, 
    assignment         = assignment, 
    estimator          = estimator_list)
  
  return(my_design)
}

