#' Declare the data-generating process of a variable
#'
#' @param type declare the type of variable
#' @param location_scale a vector containing the first and / or second moments of the distribution, if the variable takes mean and variance type arguments
#' @param min_max for uniform distributions, the minimum and maximum values
#' @param probabilities for binary and categorical variables, a vector of probabilities
#' @param outcome_categories for binary and categorical variables, labels to attach to outcomes
#'
#' @export
declare_variable <- function(
  type = NULL,
  location_scale = NULL,
  min_max = NULL,
  probabilities = NULL,
  outcome_categories = NULL
) {
  
  if(is.null(type)){
    type <- "normal"
  }
  
  type <- tolower(type)
  
  is_available <- type %in% c(continuous,binary,categorical,rate,count)
  if(is_available == FALSE){
    stop(paste0(
      "declare_variable can currently handle the following types of variables: \n categorical (",
      paste(categorical,collapse = ", "),
      "); \n binary (",
      paste(binary,collapse = ", "),
      "); \n continuous (",
      paste(continuous,collapse = ", "),
      "); \n count (",
      paste(count,collapse = ", "),
      "); \n and rate (",
      paste(rate,collapse = ", "),
      ")."
    ))
  }
  # Continuous
  if(type %in% continuous){
    
    if(type != "uniform"){
      rng <- "rnorm"
      
      if(is.null(location_scale)){
        location_scale <- c(0,1)
      }
      
      if(length(location_scale)<2){
        location_scale <- c(location_scale,location_scale/2)
      }
      
      
      return_expr <- paste0(
        rng,
        paste(
          "(n_",
          location_scale[1],
          location_scale[2],
          sep = ","
        ),
        ")")
      
    } else {
      rng <- "runif"
      
      if(is.null(min_max)){
        min_max <- c(-1,1)
      }
      
      return_expr <- paste0(
        rng,"(",
        paste("n_",
              min_max[1],
              min_max[2],
              sep = ","),
        ")")
      
    }
    
  }
  
  # Count
  if(type %in% count){
    
    if(is.null(location_scale)){
      location_scale <- c(10,1)
    }
    
    if(length(location_scale)<2){
      location_scale <- c(location_scale,1)
    }
    
    
    if(type != "gamma"){
      rng <- "rpois"
      
      return_expr <- paste0(
        rng,"(",
        paste("n_",
              location_scale[1],
              sep = ","),
        ")")
      
    } else {
      rng <- "rgamma"
      
      return_expr <- paste0(
        rng,"(",
        paste("n_",
              location_scale[1],
              location_scale[2],
              sep = ","),
        ")")
    }
    
  }
  
  # Categorical
  if(type %in% categorical){
    rng <- "rmultinom"
    
    if(type == "race"){
      outcome_categories <- race_categories
    }
    
    if(type == "us_party"){
      outcome_categories <- us_party_categories
    }
    
    
    if(is.null(outcome_categories) & is.null(probabilities)){
      outcome_categories <- 1:4
    } else {
      if(is.null(outcome_categories)){
        outcome_categories <- 1:length(probabilities)
      }
    }
    
    if(is.null(probabilities)){
      probabilities <- rep(1/length(outcome_categories),length(outcome_categories))
    } else {
      if(length(probabilities) != length(outcome_categories)){
        stop(paste0("Your vector of outcome_categories has a length of ",
                    length(outcome_categories),
                    ", therefore the vector of probabilities should also be of length ",
                    length(outcome_categories),"."
        ))
      }
    }
    
    is_integer <- is.integer(outcome_categories)
    
    probabilities <- paste0(
      "c('",
      paste(probabilities,collapse = "','"),
      "')")
    
    if(is_integer){
      outcome_categories <- paste0(
        "c(",
        paste(outcome_categories,collapse = ","),
        ")")
      
      return_expr <- paste0(
        "sample(x = ",
        outcome_categories,
        ",size = n_,replace = TRUE,prob = ",
        probabilities,")"
      )
      
    } else {
      outcome_categories <- paste0(
        "c('",
        paste(outcome_categories,collapse = "','"),
        "')")
      
      return_expr <- paste0(
        "as.factor(sample(x = ",
        outcome_categories,
        ",size = n_,replace = TRUE,prob = ",
        probabilities,"))"
      )
    }
  }
  
  if(type %in% binary){
    rng <- "rbinom"
    
    if(is.null(probabilities)){
      probabilities <- 0.5
    }
    
    return_expr <- paste0(
      rng,"(",
      paste("n_",
            1,
            probabilities,
            sep = ","),
      ")")
    
    
    if(type == "gender"){
      outcome_categories <- c("F","M")
    }
    

    
    if(!is.null(outcome_categories)){
      return_expr <- paste0(
        "temp_var <- as.factor(",
        return_expr,
        ");levels(temp_var) <- c('",
        paste(outcome_categories,collapse = "','"),
        "');temp_var"
      )
    }
    
    if(type == "boolean"){
      return_expr <- paste0(return_expr," == 1")
    }
    
  }
  
  # Rate
  if(type %in% rate){
    
    rng <- "rbeta"
    
    if(is.null(location_scale)){
      if(!is.null(probabilities)){
        location_scale <- probabilities[1]
      } else {
        location_scale <- .5
      }
    }
    if(length(location_scale)<2){
      location_scale <- c(location_scale,.01)
    }
    
    alpha_beta <- beta_reparam(mu = location_scale[1],location_scale[2])
    
    return_expr <- paste0(
      rng,"(",
      paste("n_",
            alpha_beta[1],
            alpha_beta[2],
            sep = ","),
      ")")
    
  }
  
  return(return_expr)
  
}

beta_reparam <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  if(any(c(alpha,beta) < 0)){
    stop("You have chosen location and scale parameters for a rate variable that go beyond the [0,1] bounds.")
  }
  return(params = list(alpha = alpha, beta = beta))
}


environment(declare_variable) <- list2env(
  list(
    continuous = c(
      # Distributions:
      "normal","uniform",
      # Types
      "continuous"
    ),
    categorical = c(
      # Distributions
      "multinomial",
      # Types
      "categorical",
      # Custom variables
      "race","us_party"
    ),
    binary = c(
      # Distributions
      "binomial",
      # Types
      "binary",
      # Custom variables
      "gender",
      # Boolean 
      "boolean"
    ),
    rate = c(
      # Distributions
      "beta",
      # Types 
      "rate",
      # Custom variables
      "percentage","ratio","proportion"
    ),
    count = c(
      # Distributions
      "poisson","gamma",
      # Types
      "count",
      # Custom variables
      "age","events"
    ),
    beta_reparam = beta_reparam,
    race_categories = c("black","white","asian","hispanic","other"),
    us_party_categories = c("dem","rep","ind")
  )
)

#' Print the different kinds of variable types
#'
#' @export
get_variable_types <- function(){
  variable_types <- c("continuous","categorical","binary","count","rate")
  print_list <- lapply(variable_types,
         get,
         env = environment(declare_variable))
  names(print_list) <- variable_types
  return(print_list)
}



#' A function for grabbing variables at a given level in multi-level structures
#' 
#' @param level_ID The name of the level ID to get a variable for.
#' @param variable_name The variable name extracted from the data.
#' @param data A data.frame object used to get a variable.
#' @param aggregate_function The function to use to aggregate data if applicable.
#' @param ... A set of additional options sent to the aggregate function.
#'
#' @export
get_variable <- function(
  level_ID, 
  variable_name, 
  data, 
  aggregate_function = NULL,
  ...
){
  
  options <- list(...)
  
  default_fun <- "function(x)unique(x)[1]"
  
  if(is.null(aggregate_function)){
    aggregate_function_string <- default_fun
  } else {
    aggregate_function_string <- as.character(substitute(aggregate_function))
  }
  
  data_string <- as.character(substitute(data))
  variable_name_string <- as.character(substitute(variable_name))
  level_ID_string <- as.character(substitute(level_ID))
  options_string <- as.character(substitute(options))
  
  expr <- paste0(
    "with(",data_string,",tapply(X = ",variable_name_string,",INDEX = ",level_ID_string,
    ",FUN = ",aggregate_function_string,options_string,"))"
  )
  return(expr)
}






















