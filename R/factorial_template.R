#' 2x2 Factorial Template
#' 
#' @param N A scalar (must be an integer) indicating the total population size.
#' @param n A scalar (must be an integer) indicating the total sample size. By default, n = N.
#' @param m A scalar (must be an integer) indicating the number of arms. Note that m > 1 for any experiment.
#' @param probability_each A vector of four elements indicating the probability of assignment to eaach treatment condition in the order Z00, Z01, Z10, Z11. The elements in the vector must sum to 1. By default, units have a 1/4 probability of assignment to each experimental group.
#' @param cond_means A vector with four elements indicating the mean of the realized potential outcome in each treatment condition in the order Z00, Z01, Z10, Z11.
#' @param noise_scale A scalar or a vector of four elements indicating the standard deviation of individual-level noise for each experimental group. If it is a scalar, the standard deviation of individual-level noise is constant across all treatment groups. If it is a vector, the standard deviation of noise is group-specific. The elements in the vector correspond to treatment condition in the order Z00, Z01, Z10, Z11.
#' @param binary_POs A logical argument indicating whether potential outcomes are binary. If binary_POs = TRUE, all elements in the vector cond_means must be valid probabilities (between 0 and 1, inclusive). By default, binary_POs = FALSE.
#' @param coef_X A scalar coefficient on a covariate, X, in the potential outcomes function.  By default, coef_X = 0.
#' @param location_scale_X A vector of length two indicating the mean and standard deviation of covariate X. By default, location_scale_X = c(0, 1). 
#' @param cov_adjustment A logical argument indicating whether covariate adjustment should be used in estimation. By default, cov_adjustment = F. 
#' @param block_var_vector For non-clustered designs, an optional vector of length N indicating the block to which each unit belongs. 
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
#' design <- factorial_template(N = 5000, n = 500, cond_means = c(1, 1.5, 1.3, 1.8), noise_scale = 1)
#' design2 <- factorial_template(N = 5000, n_clust_pop = 100, n_blocks = 10, 
#'                              cond_means = c(1, 1.5, 1.3, 1.8), noise_scale = 1)
#'}                            
#' @export
#' 
#' 
show_condition_names <- function(factors = c(2, 2)){
  k <- prod(factors)  
  perm <- function(v) {
    sapply(1:length(v), function(x) {
      rep(rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
          length.out=prod(v))
    } ) - 1
  }
  
  ts    <-  perm(factors)
  condition_names  <- sapply(1:nrow(ts), function(j) paste(c("Z",ts[j,]), collapse = ""))
  return(list(condition_names, ts))
}

show_condition_names()[[1]] # use to provide guide for inputting condition means

make_block_noise <- function(block_var){
  if(!is.numeric(block_var)){bv <- as.numeric(factor(block_var))}
  else{bv <- block_var}
  rn <- rnorm(length(unique(bv)))
  return(rn[bv])
}

factorial_template <- function(N, 
                               n = N, 
                               cond_means = c(0, 0, 0, 0),
                               probability_each = rep(1/4, 4),
                               noise_scale = 1,
                               coef_X = 0,
                               location_scale_X = c(0, 1),
                               binary_POs = F,
                               cov_adjustment = F,
                               block_var_probs = c(.5, .5),
                               blocked_RA = F,
                               block_var_vector = NULL,
                               block_m_each = NULL,
                               block_noise_coef = 0,
                               block_prob_each = NULL,
                               n_clust_pop = NULL,
                               n_blocks = 1,
                               clust_noise_coef = 1,
                               n_clust_samp = n_clust_pop,
                               custom_pop_list = NULL,
                               SE = ifelse(is.null(n_clust_pop), "robust", "clustered")){
 
 # Primitives --------------------------------------------------------------
   factors <- c(2, 2)
   
   condition_names <- show_condition_names(factors = factors)[[1]]
   
   ts <- show_condition_names(factors = factors)[[2]]
   
   main_conditions <- sapply(1:length(factors), function(f) (
      sapply(1:(factors[f]-1), function(t)
        condition_names[ts[,f]== t])))
   

# Checks ------------------------------------------------------------------

   if(n > N){
     stop("Sample (n) cannot be larger than population (N)!")
   }
   
   if(length(cond_means) != 4) {
     stop("Vector of condition means must contain four elements for the four experimental groups in the order 'Z00', 'Z10', 'Z10', and 'Z11'.")
   }

   if(length(noise_scale) != 4  &  length(noise_scale)!= 1) {
     stop("Vector of noise scale must be of length 4 or 1!")
   }
   
   if(sum(noise_scale < 0) > 0| clust_noise_coef < 0 | block_noise_coef < 0 ){
     stop("All noise-scaling inputs must be zero or positive.")
   }
   
   if(sum(block_var_probs) != 1){
     stop("Block_var_probs must sum to 1.")
   }
   if(binary_POs == TRUE & sum(cond_means > 1) > 0 | binary_POs == TRUE & sum(cond_means < 0) > 0){
     stop("All combinations of mu_Y0 + ATEs must be between 0 and 1.")
   }
   
   if(!is.null(n_clust_pop) & !is.null(n_clust_samp)){
     if(n_clust_samp > n_clust_pop){
       stop("Number of clusters in the sample cannot exceed the number of clusters in the population.")
     }
   }
# Design ------------------------------------------------------------------
if(is.null(n_clust_pop)){
  if(is.null(block_var_vector)){
    population    <- declare_population(
      noise = declare_variable(location_scale = c(0, 1)),
      X = declare_variable(location_scale = location_scale_X),
      block_var = declare_variable(type = "multinomial",
                                   probabilities = block_var_probs, outcome_categories = 1:length(block_var_probs)),
      block_noise = "make_block_noise(block_var)",
      size = N)
  }
  
  if(!is.null(block_var_vector)){
    population    <- declare_population(
      noise = declare_variable(location_scale = c(0, 1)),
      X = declare_variable(location_scale = location_scale_X),
      block_var = block_var_vector,
      block_noise = "make_block_noise(block_var)",
      size = N)
  }  
  
  if(binary_POs == FALSE){
    write_PO_formula <- function(cond_means, noise_scale, block_noise_coef){
      tab <- cbind(cond_means, noise_scale)
      
      POs <- paste0("Y ~ ", paste(sapply(1:nrow(tab), function(j) {
        paste0(tab[j, 1], " * (Z == '",  condition_names[j], "') + ", 
               tab[j, 2], " * noise * (Z == '", condition_names[j], "')")}), 
        collapse = " + "), " +  X  * ", coef_X, "+ block_noise *", block_noise_coef)
      
      return(POs)
    }
    
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = condition_names,
      formula = as.formula(write_PO_formula(cond_means = cond_means, noise_scale = noise_scale,
                                            block_noise_coef = block_noise_coef)))
  }
  
  if(binary_POs == T){
    
    write_binary_PO_formula <- function(cond_means, noise_scale, block_noise_coef){
      tab <- cbind(cond_means, noise_scale)
      cb_noise <- paste0("block_noise * ", block_noise_coef)
      
      POs <- paste0("Y ~ 1 * (", paste(sapply(1:nrow(tab), function(j) {
                      paste0("(Z == '",  condition_names[j], "') * (", 
                             tab[j, 2], " * noise +", cb_noise, " + qnorm(", tab[j,1], 
                             ", mean(", tab[j, 2], " * noise +", cb_noise,
                             "), sd(", tab[j, 2], " * noise +", cb_noise,")))")}), 
                      collapse = " + "), " > 0)")
      return(POs)
    }
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = condition_names,
      formula = as.formula(write_binary_PO_formula(cond_means = cond_means, noise_scale = noise_scale,
                                                   block_noise_coef = block_noise_coef)))
  }
  
   write_PO_formula <- function(cond_means, noise_scale){
     tab <- cbind(cond_means, noise_scale)

     POs <- paste0("Y ~ ", paste(sapply(1:nrow(tab), function(j) {
                              paste0(tab[j, 1], " * (Z == '",  condition_names[j], "') + ", 
                                     tab[j, 2], " * noise * (Z == '", condition_names[j], "')")}), 
                              collapse = " + "), " +  X  * ", coef_X, "+ block_noise *", block_noise_coef)
     return(POs)
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
  
  if(binary_POs == FALSE){
    write_PO_formula <- function(cond_means, noise_scale, block_noise_coef, clust_noise_coef){
      tab <- cbind(cond_means, noise_scale)
      
      POs <- paste0("Y ~ ", paste(sapply(1:nrow(tab), function(j) {
        paste0(tab[j, 1], " * (Z == '",  condition_names[j], "') + ", 
               tab[j, 2], " * noise_ind * (Z == '", condition_names[j], "')")}), 
        collapse = " + "), " +  X  * ", coef_X, "+ noise_block *", block_noise_coef,
          " + noise_clust *", clust_noise_coef)
      
      return(POs)
    }
    
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = condition_names,
      formula = as.formula(write_PO_formula(cond_means = cond_means, noise_scale = noise_scale,
                                            clust_noise_coef = clust_noise_coef,
                                            block_noise_coef = block_noise_coef)))
  }
  
  if(binary_POs == T){
    
    write_binary_PO_formula <- function(cond_means, noise_scale, block_noise_coef){
      tab <- cbind(cond_means, noise_scale)
      cb_noise <- paste0("block_noise * ", block_noise_coef, "+ clust_noise *", clust_noise_coef)
      
      POs <- paste0("Y ~ 1 * (", paste(sapply(1:nrow(tab), function(j) {
        paste0("(Z == '",  condition_names[j], "') * (", 
               tab[j, 2], " * noise_ind +", cb_noise, " + qnorm((", tab[j,1], 
               "), mean(", tab[j, 2], " * noise_ind +", cb_noise,
               "), sd(", tab[j, 2], " * noise_ind +", cb_noise,")))")}), 
        collapse = " + "), " > 0)")
      return(POs)
    }
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = condition_names,
      formula = as.formula(write_binary_PO_formula(cond_means = cond_means, noise_scale = noise_scale,
                                                   block_noise_coef = block_noise_coef)))
  }
  
  write_PO_formula <- function(cond_means, noise_scale){
    tab <- cbind(cond_means, noise_scale)
    
    POs <- paste0("Y ~ ", paste(sapply(1:nrow(tab), function(j) {
      paste0(tab[j, 1], " * (Z == '",  condition_names[j], "') + ", 
             tab[j, 2], " * noise * (Z == '", condition_names[j], "')")}), 
      collapse = " + "), " +  X  * ", coef_X, "+ block_noise *", block_noise_coef)
    return(POs)
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
                                       block_probabilities = block_prob_each,
                                       cluster_variable_name = "cluster_ID", 
                                       potential_outcomes = potential_outcomes)
  }
  
}  
   
        
     estimand_function <- function(data, estimand_num){ 
       estimand_weights <- matrix(c( -1, 0 , 1, 0,
                                     -1, 1, 0, 0,
                                     0, -1, 0, 1,
                                     0, 0, -1, 1,
                                    -.5, -.5, .5, .5,
                                    -0.5, .5, -.5, .5,
                                    1, -1, -1, 1), nrow = 7, ncol = 4, byrow = T)
        exp <- paste0(estimand_weights[estimand_num,], "*", "data[,'Y_Z_", 
                      condition_names, "']", collapse = "+")
        return(mean(eval(parse(text = exp))))
       }

    
     estimator_function <- function(data, estimand_num, cluster_ID = NULL){
      for(j in 1:nrow(main_conditions)){data[paste0("T", j)] <- 1 * (data$Z %in% main_conditions[,j])}
       
       if(!cov_adjustment){estimator_formula = "Y ~ T1 * T2"}
       if(cov_adjustment){estimator_formula = "Y ~ T1 * T2 + X"}
       
       mod <- lm(estimator_formula, data = data, weights = Z_assignment_weights)
       
       if(estimand_num %in% c(1, 2, 7)){
         
         coef_name <- ifelse(estimand_num == 1, "T1", ifelse(estimand_num == 2, "T2", "T1:T2"))
           
           if(SE == "OLS"){
             estimates <- get_regression_coefficient(model = mod, 
                                                     coefficient_name = coef_name)
           }
           if(SE == "robust"){
             estimates <- get_regression_coefficient_robust(model = mod,
                                                            coefficient_name = coef_name)
           }
           if(SE == "clustered"){
             estimates <- get_regression_coefficient_clustered(model = mod,
                                                               data = data,
                                                               coefficient_name = coef_name,
                                                               cluster_name = cluster_ID)
           }
       }
       
       
       if(SE == "OLS"){vcov.mat <- vcov(mod)}
       if(SE == "robust"){
         vcov.mat <- sandwich::vcovHC(mod, type = "HC2")}
       if(SE == "clustered"){
         cluster_robust <- function(model, cluster){
           require(sandwich, quietly = TRUE)
           require(lmtest, quietly = TRUE)
           not.miss<- !is.na(predict(model))
           if(length(not.miss)!=length(cluster)){
             stop("check your data: cluster variable has different N than model")
           }
           M <- length(unique(cluster[not.miss]))
           N <- length(cluster[not.miss])
           K <- model$rank
           if(M<50){
             warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
           }
           dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
           uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum, na.rm=TRUE));
           vcovCL <- dfc * sandwich(model, meat = crossprod(uj)/N)
           return(vcovCL)
         }
         complete_obs <- as.numeric(rownames(model.matrix(mod)))
         vcov.mat <- cluster_robust(mod, cluster = data[complete_obs, cluster_ID])
       }
  
       
       if(estimand_num == 3){
         est      <- summary(mod)$coef["T1",1] + summary(mod)$coef["T1:T2", 1]
         se       <- sqrt(vcov.mat["T1", "T1"] + vcov.mat["T1:T2", "T1:T2"] + 2 * vcov.mat["T1", "T1:T2"])
         df       <- mod$df.residual
         p        <- pt(q = abs(est/se), df = df, lower.tail = F)
         conf_int <- c(est - 1.96*se, est + 1.96*se)
         estimates <- data.frame(estimate_label = "T1 + T1:T2", est = as.numeric(est), 
                                 se = as.numeric(se), p = as.numeric(p), 
                                 ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                                 df = as.numeric(df), stringsAsFactors = FALSE)}
       
       if(estimand_num == 4){
         est       <- summary(mod)$coef["T2",1] + summary(mod)$coef["T1:T2", 1]
         se        <- sqrt(vcov.mat["T2", "T2"] + vcov.mat["T1:T2", "T1:T2"] + 2 * vcov.mat["T2", "T1:T2"])
         df        <- mod$df.residual
         p         <- pt(q = abs(est/se), df = df, lower.tail = F)
         conf_int  <- c(est - 1.96*se, est + 1.96*se)
         estimates <- data.frame(estimate_label = "T2 + T1:T2", est = as.numeric(est), 
                                 se = as.numeric(se), p = as.numeric(p), 
                                 ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                                 df = as.numeric(df), stringsAsFactors = FALSE)}
       
        if(estimand_num == 5){
          est      <- summary(mod)$coef["T1",1] + 0.5 * summary(mod)$coef["T1:T2", 1]
          se       <- sqrt(vcov.mat["T1", "T1"] + 0.25 * vcov.mat["T1:T2", "T1:T2"] + vcov.mat["T1", "T1:T2"])
          df       <- mod$df.residual
          p        <- pt(q = abs(est/se), df = df, lower.tail = F)
          conf_int  <- c(est - 1.96*se, est + 1.96*se)
          estimates <- data.frame(estimate_label = "T1 + 0.5 T1:T2", est = as.numeric(est), 
                                  se = as.numeric(se), p = as.numeric(p), 
                                  ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                                  df = as.numeric(df), stringsAsFactors = FALSE)}
       
       if(estimand_num == 6){
         est      <- summary(mod)$coef["T2",1] + 0.5 * summary(mod)$coef["T1:T2", 1]
         se       <- sqrt(vcov.mat["T2", "T2"] + 0.25 * vcov.mat["T1:T2", "T1:T2"] + vcov.mat["T2", "T1:T2"])
         df       <- mod$df.residual
         p        <- pt(q = abs(est/se), df = df, lower.tail = F)
         conf_int  <- c(est - 1.96*se, est + 1.96*se)
         estimates <- data.frame(estimate_label = "T2 + 0.5 T1:T2", est = as.numeric(est), 
                                 se = as.numeric(se), p = as.numeric(p), 
                                 ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                                 df = as.numeric(df), stringsAsFactors = FALSE)}

      
      return(estimates)
    }

     make_estimator <- function(estimand_num){
       estimand  <- declare_estimand(estimand_function = estimand_function, 
                                     estimand_num = estimand_num,
                                     potential_outcomes = potential_outcomes)
       labs = c("ME_T1|T2 = 0", "ME_T2|T1 = 0", "ME_T1|T2 = 1", "ME_T2|T1 = 1",
                  "Avg_ME_T1", "Avg_ME_T2", "CME_T1_T2")
       estimator <- declare_estimator(estimates = estimator_function, 
                                      estimand_num = estimand_num, 
                                      cluster_ID = ifelse(is.null(n_clust_pop), NA, "cluster_ID"),
                                      estimand = estimand,
                                      label = labs[estimand_num])
       return(estimator)
     }
     

     
     estimator_list <- lapply(X = 1 : 7, FUN = make_estimator)

 # Declare Design ----------------------------------------------------------
     
     my_design <- declare_design(
       population         = population, 
       potential_outcomes = potential_outcomes, 
       sampling           = sampling, 
       assignment         = assignment, 
       estimator          = estimator_list)
     
     return(my_design)
     
}



