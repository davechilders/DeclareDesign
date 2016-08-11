#' Estimate the ICC of a given variable
#'
#' @param variable A vector whose intra-cluster correlation coefficient should be estimated
#' @param cluster_variable A vector of cluster IDs indicating the cluster corresponding to each element of variable 
#' 
#' @export
calculate_ICC <- function(variable, cluster_variable){
  # Code inspired by ICC package, Matthew Wolak <matthewwolak@gmail.com>
  # 2015-06-17 licensed under GPL (>= 2)
  anova_out <- anova(aov(variable ~ as.factor(cluster_variable)))
  J <- length(unique(cluster_variable))
  mean_sq_j <- anova_out$"Mean Sq"[1]
  mean_sq_i <- var_i <- anova_out$"Mean Sq"[2]
  n_j <- tapply(variable,as.factor(cluster_variable), FUN = length)
  k <- (1/(J - 1)) * (sum(n_j) - (sum(n_j^2)/sum(n_j)))
  var_j <- (mean_sq_j - mean_sq_i)/k
  rho <- var_j/(var_i + var_j)
  return(rho)
}

# Default interference function ----------------------------------------------

default_interference_function <- function(data, options, assignment_variable_name){
  if(!is.null(options)){
    
  }
  return(data)
}

# Default transform function ----------------------------------------------

default_transform_function <- function(data, ..., assignment_variable_name){
  options <- list(...)
  if(!is.null(options)){
    for(i in 1:length(options)){
      data[, names(options)[i]] <- as.numeric(data[, assignment_variable_name] %in% options[[i]])
    }
  }
  return(data)
}


# Built-in Estimates Functions --------------------------------------------


#' Extract Regression Coefficients
#' 
#' @param model A model fit
#' @param formula An optional formula
#' @param coefficient_name The name of the coefficient to extract.
#' @param statistics The statistics to extract. Defaults to c("est", "se", "p", "ci_lower", "ci_upper", "df")
#' @param label Character label for the regression coefficient
#'
#' @export
get_regression_coefficient <- function(model, formula = NULL, coefficient_name, label = coefficient_name){
  
  coef_num <- which(names(coef(model)) %in% coefficient_name)
  df <- df.residual(model)
  est <- coef(model)[coef_num]
  se <- sqrt(diag(vcov(model)))[coef_num]
  p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
  conf_int <- suppressMessages(confint(model))[coef_num, ]
  
  
  return(data.frame(estimate_label = label, est = as.numeric(est), 
                    se = as.numeric(se), p = as.numeric(p), 
                    ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                    df = as.numeric(df), stringsAsFactors = FALSE))
}



#' Extract Regression Coefficients
#' 
#' @param model A model fit
#' @param formula An optional formula
#' @param coefficient_name The name of the coefficient to extract.
#' @param statistics The statistics to extract. Defaults to c("est", "se", "p", "ci_lower", "ci_upper", "df")
#' @param label Character label for the regression coefficient
#'
#' @export
get_regression_coefficient_robust <- function(model, formula = NULL, coefficient_name, label = coefficient_name){
  coef_num <- which(names(coef(model)) %in% coefficient_name)
  df <- df.residual(model)
  est <- coef(model)[coef_num]
  se <- sqrt(diag(sandwich::vcovHC(model, type = "HC2")))[coef_num]
  p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
  
  conf_int <- est + se %o% qt(c(0.025,0.975), summary(model)$df[2])

  return(data.frame(estimate_label = label, est = as.numeric(est), 
                    se = as.numeric(se), p = as.numeric(p), 
                    ci_lower = as.numeric(conf_int[1]), ci_upper = as.numeric(conf_int[2]), 
                    df = as.numeric(df), stringsAsFactors = FALSE))
}

# Built-in-Estimators -----------------------------------------------------

collapse_clusters <- function(data, cluster_variable_name, cluster_collapse_function){
  return(aggregate(as.formula(paste("cbind(", paste(colnames(data)[!(colnames(data) %in% cluster_variable_name)], collapse = ","), ") ~", cluster_variable_name)), 
                   data = data, FUN = cluster_collapse_function))
}

#' Built-in Estimators: Difference-in-means
#' 
#' @param formula An object of class "formula", such as Y ~ Z
#' @param data A data.frame, often created by \code{\link{draw_population}}.
#' @param weights An optional vector of weights (not yet implemented).
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param cluster_variable_name The (optional) name of a clustered variable. The function will first collapse the data by cluster before calculating the difference-in-means.
#' @param cluster_collapse_function The (optional) function to use to collapse the data by cluster. Default is \code{mean}.
#' @param alpha The significance level, 0.05 by default.
#'
#' @export
difference_in_means <- function(formula, condition1 = NULL, condition2 = NULL, 
                                data, weights = NULL, subset = NULL, 
                                cluster_variable_name = NULL, cluster_collapse_function = mean, alpha = .05,
                                estimate_label = NULL) {
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  if(!is.null(subset))
    data <- data[subset, ]
  
  if(!is.null(cluster_variable_name)){
    data <- collapse_clusters(data = data, cluster_variable_name = cluster_variable_name, 
                              cluster_collapse_function = cluster_collapse_function)
  }
  
  Y <- data[, all.vars(formula[[2]])]
  t <- data[, all.vars(formula[[3]])]
  
  if(is.factor(t)){
    condition_names <- levels(t)
  }else{
    condition_names <- sort(unique(t))
  }
  
  if(is.null(condition1) & is.null(condition2)){
    condition1 <- condition_names[1]
    condition2 <- condition_names[2]
  }
  
  ##if(!is.null(weights))
  ##  w <- weights[subset]
  
  N <- length(Y)
  diff <- mean(Y[t == condition2]) - mean(Y[t == condition1])
  se <- sqrt(var(Y[t == condition2])/sum(t == condition2) + 
               var(Y[t == condition1])/sum(t == condition1))
  df <- N - 2
  p <- 2 * pt(abs(diff/se), df = df, lower.tail = FALSE)
  ci_lower <- diff - qt(1 - alpha/2, df = df) * se
  ci_upper <- diff + qt(1 - alpha/2, df = df) * se
  
  if(is.null(estimate_label)){
    estimate_label <- paste0("d_i_m_", ifelse(!is.null(cluster_variable_name), "clustered_", ""), condition2, "-", condition1)
  }
  
  return_df <- data.frame(estimate_label = estimate_label,
                          est = diff, se = se, p = p, 
                          ci_lower = ci_lower, ci_upper = ci_upper, df = df,
                          stringsAsFactors = FALSE)
  
  return(return_df)
  
}


#' Built-in Estimators: Blocked Difference-in-means 
#' 
#' @param formula An object of class "formula", such as Y ~ Z
#' @param data A data.frame, often created by \code{\link{draw_population}}.
#' @param weights An optional vector of weights (not yet implemented).
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param block_variable_name The name of the blocking variable.
#' @param cluster_variable_name The (optional) name of a clustered variable. The function will first collapse the data by cluster before calculating the difference-in-means.
#' @param cluster_collapse_function The (optional) function to use to collapse the data by cluster. Default is \code{mean}.
#' @param alpha The significance level, 0.05 by default.
#'
#' @export
difference_in_means_blocked <- function(formula, condition1 = NULL, condition2 = NULL, data, weights = NULL, subset = NULL, 
                                        block_variable_name = NULL, cluster_variable_name = NULL, cluster_collapse_function = mean, 
                                        estimate_label = NULL, alpha = .05) {
  
  if(is.null(block_variable_name))
    stop("This difference-in-means estimator can only be used if you specify block_variable_name, a string indicating which variable in the data frame contains the blocks.")
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  if(!is.null(subset))
    data <- data[subset, ]
  
  if(!is.null(cluster_variable_name)){
    data <- collapse_clusters(data = data, cluster_variable_name = cluster_variable_name, cluster_collapse_function = cluster_collapse_function)
  }
  
  Y <- data[, all.vars(formula[[2]])]
  t <- data[, all.vars(formula[[3]])]
  
  if(is.factor(t)){
    condition_names <- levels(t)
  }else{
    condition_names <- sort(unique(t))
  }
  
  if(is.null(condition1) & is.null(condition2)){
    condition1 <- condition_names[1]
    condition2 <- condition_names[2]
  }
  
  b <- data[, block_variable_name]
  
  N <- length(Y)
  
  block_names <- sort(unique(b))
  
  block_weights <- (sapply(block_names, function(i) sum(b==i)))/N
  
  means_by_block <- tapply(Y,list(t, b), mean, na.rm = TRUE)
  diff <- (block_weights %*% (means_by_block[as.character(condition2),] - 
                                means_by_block[as.character(condition1),]))
  
  vars <- sapply(block_names, function(i)  {
    var(Y[b==i & t == condition1], na.rm = TRUE )/sum(b==i & t == condition2)+
      var(Y[b==i & t == condition2], na.rm = TRUE )/sum(b==i & t == condition1)})
  se  <- (block_weights^2 %*% vars)^.5
  
  df <- length(Y) - length(block_names) - 1
  p <- 2 * pt(abs(diff/se), df = df, lower.tail = FALSE)
  ci_lower <- diff - qt(1 - alpha / 2, df = df) * se
  ci_upper <- diff + qt(1 - alpha / 2, df = df) * se
  
  if(is.null(estimate_label)){
    estimate_label <- paste0("d_i_m_blocked_", ifelse(!is.null(cluster_variable_name), "clustered_", ""), condition2, "-", condition1)
  }
  
  return(data.frame(estimate_label = estimate_label,
                    est = diff, se = se, p = p, 
                    ci_lower = ci_lower, ci_upper = ci_upper, df = df,
                    stringsAsFactors = FALSE))
}

# Potential outcomes ------------------------------------------------------

#' Default potential outcomes function
#' 
#' @param formula A formula describing the outcomes as a function of a 
#' @param data A data frame
#' @param ... optional options passed along.
#'
#' @export
default_potential_outcomes_function <- function(formula, data, ...){
  
  options <- list(...)
  
  potential_outcomes_env <- list2env(data)
  if(length(options) > 0){
    for(i in 1:length(options)){
      assign(x = names(options)[i], value = options[[i]], 
             envir = potential_outcomes_env)
    }
  }
  potential_outcomes_env$n_ <- nrow(data)
  
  return_po <- eval(expr = formula[[3]], envir = potential_outcomes_env)
  
  if(length(return_po) != potential_outcomes_env$n_){
    formula <- update.formula(old = formula,new = . ~ I(.) + I(rep(0,n_)))
    return_po <- eval(expr = formula[[3]], envir = potential_outcomes_env)
  }
  
  return(return_po)
  
}

#' Default potential outcomes function
#' 
#' @param formula A formula describing the outcomes as a function of a 
#' @param data A data frame
#' @param ... optional options passed along.
#'
#' @export
default_exposure_function <- default_potential_outcomes_function

# Rows: outcome names
# Columns: condition names

#' Default function for proportional potential outcomes
#' 
#' @param data A data.frame
#' @param population_proportions Proportion in outcome for each treatment condition.
#' @param condition_names A vector of condition names.
#' @param assignment_variable_name The variable name for the assignment indicator.
#' 
#' @export
proportion_potential_outcomes_function <- function(data, population_proportions = c(.5, .5), 
                                                   condition_names = NULL, assignment_variable_name = "Z"){
  
  
  
  if(!assignment_variable_name %in% names(data)){
    stop("Unable to find the assignment variable. Please check that you have specified assignment_variable_name correctly.")
  }
  
  assignment_variable <- data[,assignment_variable_name]
  
  N <- nrow(data)
  
  is_vector <- is.numeric(population_proportions) & 
    is.vector(population_proportions)
  
  if(is_vector){
    
    if(is.null(condition_names) & !is.null(names(population_proportions)))
      condition_names <- names(population_proportions)
    
    out_names <- c(0,1)
    
    population_proportions <- matrix(
      data = c(1-population_proportions,
               population_proportions),
      byrow = T,
      nrow = 2,
      dimnames = list(out_names,condition_names))
    
  }
  
  if(!is.null(condition_names) & ncol(population_proportions) != length(condition_names)){
    stop("ncol(population_proportions) != length(condition_names). \nPlease provide population proportions for each of the treatment conditions.")
  }
  
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  
  row_names <- rownames(population_proportions)
  
  if(is.null(row_names)){
    if(is.null(out_names)){
      out_names <- 1:nrow(population_proportions)
    }
  } else {
    out_names <- row_names
  }
  
  if(is.null(condition_names)){
    condition_names <- colnames(population_proportions)
    if(is.null(condition_names)){
      condition_names <- sort(unique(assignment_variable))
    }
  } 
  
  outcomes <- apply(counts, 2, function(times){
    sample(
      rep(out_names,times = times)
    )
  })
  
  revealed_outcome <- sapply(assignment_variable,function(x) which(condition_names == x))
  
  rows <- 1:length(revealed_outcome)
  
  reveal_mat <- cbind(rows,revealed_outcome)
  
  return_vector <- outcomes[reveal_mat]
  suppressWarnings({
    if(all(identical(as.character(as.numeric(return_vector)),return_vector)))
      return_vector <- as.numeric(return_vector)
  })
  return(return_vector)
  
}




# Bootstrap ---------------------------------------------------------------


#' Bootstrap data, including multi-level data
#' 
#' @param data A data.frame, usually provided by the user and not created with the DeclareDesign package.
#' @param size A size declaration -- must describe this!
#' @param level_IDs The variables that indicate the data hierarchy.
#' 
#' @return A data.frame
#'
#' @export
bootstrap_data <- function(data, size, level_IDs = NULL){
  
  hierarchy <- get_hierarchy(size = size)
  N <- hierarchy$N
  N_per_level <- hierarchy$N_per_level
  group_sizes_per_level <- hierarchy$group_sizes_per_level
  N_levels <- hierarchy$N_levels
  
  sample_by_level <- list()
  for(j in N_levels:1){
    if(j == N_levels){
      if(is.null(level_IDs) & N_levels==1){
        sample_by_level[[j]] <- sample(1:nrow(data), N_per_level[j], replace = TRUE)
      }
      sample_by_level[[j]] <- sample(data[, level_IDs[j]], N_per_level[j], replace = TRUE)
    } else {
      ## now go through each of the units in the level above it
      sample_current_level <- c()
      for(k in sample_by_level[[j+1]]){
        sample_current_level <- c(sample_current_level, 
                                  sample(data[data[, level_IDs[j+1]] == k, level_IDs[j]], 
                                         round(N_per_level[j]/N_per_level[j+1]), replace = TRUE))
      }
      sample_by_level[[j]] <- sample_current_level
    }
  }
  data <- data[sample_by_level[[1]], , drop = FALSE]
  
  ## reset row names so they are unique
  rownames(data) <- 1:nrow(data)
  
  return(data)
}



