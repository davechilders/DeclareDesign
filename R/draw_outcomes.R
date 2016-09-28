#' Draw potential outcomes
#' 
#' @param data A data.frame object
#' @param condition_names A vector of condition names.
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param noncompliance A noncompliance object created by \code{\link{declare_noncompliance}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}.}
#'
#' @export
draw_potential_outcomes <- function(data, condition_names = NULL, potential_outcomes, 
                                    noncompliance = NULL, attrition = NULL) {
  
  if(is.null(potential_outcomes)){
    stop("You must provide a potential_outcomes object to draw_potential_outcomes.")
  }
  
  if(class(potential_outcomes) == "list" & class(condition_names) == "list" &
     length(potential_outcomes) != length(condition_names)){
    stop("If you provide a list of potential_outcomes and a list of condition_names, you must provide a list of condition_names of the same length.")
  }
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "attrition", "noncompliance", "interference"), accepts_list = TRUE)
  
  inherit_condition_names <- sapply(potential_outcomes, function(x) x$inherit_condition_names)
  
  if(!any(lapply(potential_outcomes, function(x) class(x)) == "potential_outcomes") & any(inherit_condition_names) & is.null(condition_names)){
    stop("At least one object sent to the potential_outcomes argument must be created by declare_potential_outcomes.")
  }
  
  noncompliance <- clean_inputs(noncompliance, object_class = "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(attrition, object_class = "attrition", accepts_list = FALSE)
  
  if(!is.null(noncompliance)){
    potential_outcomes <- c(list(noncompliance), potential_outcomes)
  }
  
  if(is.null(condition_names)){
    condition_names <- lapply(potential_outcomes, function(x) x$condition_names)
    first_potential_outcomes_object <- which(sapply(potential_outcomes, function(x) class(x)) == "potential_outcomes")[1]
    if(any(inherit_condition_names) & is.null(condition_names[[first_potential_outcomes_object]])){
      stop("If you choose the inherit_condition_names option for any potential_outcomes, interference, noncompliance, or attrition declarations, the first potential_outcomes object created by declare_potential_outcomes must have condition_names. These will be inherited by any objects that set inherit_condition_names = TRUE.")
    }
    
    for(i in which(inherit_condition_names == TRUE)){
      condition_names[[i]] <- condition_names[[first_potential_outcomes_object]]
    }
    
  }else{
    condition_names <- replicate(length(potential_outcomes), condition_names, simplify = FALSE)
  }
  # You must provide a condition_names argument that makes sense for all po objects.
  
  has_condition_names <- all(sapply(condition_names, function(x) is.null(x))) == FALSE
  has_assignment_variable_names <- all(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name))) == TRUE
  
  if(has_condition_names & !has_assignment_variable_names){
    stop("Please provide the name of the treatment variable to the assignment_variable_name argument in declare_potential_outcomes if you provide condition_names.")
  }
  
  which_po_class <- sapply(potential_outcomes, function(x) class(x) %in% c("potential_outcomes", "noncompliance", "attrition"))
  if(sum(sapply(potential_outcomes[which_po_class], function(x) !is.null(x$assignment_variable_name))) != length(potential_outcomes[which_po_class])){
    stop("If you provide a assignment_variable_name for any of the potential_outcomes, you must provide it for all of them.")
  }
  
  if(has_condition_names & has_assignment_variable_names) {
    for(i in 1:length(potential_outcomes)){
      if(potential_outcomes[[i]]$potential_outcomes == TRUE){
        
        # make the combinations
        
        sep = potential_outcomes[[i]]$sep
        
        condition_combinations <- expand.grid(condition_names[[i]])
        if(is.null(names(condition_names[[i]]))){
          colnames(condition_combinations) <- potential_outcomes[[i]]$assignment_variable_name
        }
        
        for(j in 1:nrow(condition_combinations)){
          
          if(ncol(condition_combinations) > 1){
            condition_combination <- lapply(1:ncol(condition_combinations[j, ]), function(x){ condition_combinations[j, x] })
          } else {
            condition_combination <- list(condition_combinations[j, ])
          }
          names(condition_combination) <- colnames(condition_combinations)
          
          outcome_name_internal <- 
            paste(potential_outcomes[[i]]$outcome_variable_name, 
                  paste(names(condition_combination), condition_combinations[j,], sep = sep, collapse = sep),
                  sep = sep)
          
          data[,outcome_name_internal] <- 
            draw_potential_outcome_vector(data = data, 
                                          potential_outcomes = potential_outcomes[[i]],
                                          condition_name = condition_combination)
        }
        
        if(!is.null(noncompliance) & class(potential_outcomes[[i]]) != "noncompliance"){
          
          for(j in condition_names[[i]]){
            local_d_column <- paste(noncompliance$outcome_variable_name, noncompliance$assignment_variable_name, j, sep = noncompliance$sep)
            local_y_z_column <- paste(potential_outcomes[[i]]$outcome_variable_name, noncompliance$assignment_variable_name,
                                      j, sep = potential_outcomes[[i]]$sep)
            data[, local_y_z_column] <- NA
            local_d_values <- unique(data[, local_d_column])
            
            for(k in local_d_values){
              local_y_d_column <- paste(potential_outcomes[[i]]$outcome_variable_name, potential_outcomes[[i]]$assignment_variable_name, k,
                                        sep = potential_outcomes[[i]]$sep)
              data[data[,local_d_column] == k, local_y_z_column] <- data[data[,local_d_column] == k, local_y_d_column]
            }
          }
          
        }
        
        if(!is.null(potential_outcomes[[i]]$attrition)){
          data <- draw_potential_outcomes(data = data, 
                                          potential_outcomes = potential_outcomes[[i]]$attrition, 
                                          condition_names= condition_combination)
        }
      }
      if(!is.null(attrition)){
        data <- draw_potential_outcomes(data = data, 
                                        potential_outcomes = attrition, 
                                        condition_names = attrition$condition_names)
      }
    }
  }
  return(data)
}



#' Draw outcome
#' 
#' @param data A data.frame object
#' @param condition_names A vector of condition names.
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param noncompliance A noncompliance object created by \code{\link{declare_noncompliance}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' 
#' @return data.frame including the outcome
#'
#' @export
draw_outcome <- function(data, condition_names = NULL, potential_outcomes, 
                         noncompliance = NULL, attrition = NULL){
  
  if(is.null(potential_outcomes)){
    stop("You must provide a potential_outcomes object to draw_outcome.")
  }
  
  if(class(potential_outcomes) == "list" & class(condition_names) == "list" &
     length(potential_outcomes) != length(condition_names)){
    stop("If you provide a list of potential_outcomes and a list of condition_names, you must provide a list of condition_names of the same length.")
  }
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "interference"), accepts_list = TRUE)
  
  inherit_condition_names <- sapply(potential_outcomes, function(x) x$inherit_condition_names)
  
  if(!any(lapply(potential_outcomes, function(x) class(x)) == "potential_outcomes") & any(inherit_condition_names) & is.null(condition_names)){
    stop("At least one object sent to the potential_outcomes argument must be created by declare_potential_outcomes.")
  }
  
  noncompliance <- clean_inputs(noncompliance, object_class = "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(attrition, object_class = "attrition", accepts_list = FALSE)
  
  which_po_class <- sapply(potential_outcomes, function(x) class(x) %in% c("potential_outcomes", "noncompliance", "attrition"))
  
  has_assignment_variable_names <- all(sapply(potential_outcomes[which_po_class], function(x) !is.null(x$assignment_variable_name))) == TRUE
  
  if(sum(sapply(potential_outcomes[which_po_class], function(x) !is.null(x$assignment_variable_name))) != length(potential_outcomes[which_po_class])){
    stop("If you provide a assignment_variable_name for any of the potential_outcomes, you must provide it for all of them.")
  }
  
  if(!is.null(noncompliance)){
    potential_outcomes <- c(list(noncompliance), potential_outcomes)
    noncompliance_has_assignment_variable_names <- !is.null(noncompliance$assignment_variable_name)
    has_assignment_variable_names <- has_assignment_variable_names & noncompliance_has_assignment_variable_names
    if(!noncompliance_has_assignment_variable_names){
      stop("Please provide an assignment variable name to declare_noncompliance.")
    }
  }
  
  if(is.null(condition_names)){
    condition_names <- lapply(potential_outcomes, function(x) x$condition_names)
    first_potential_outcomes_object <- which(sapply(potential_outcomes, function(x) class(x)) == "potential_outcomes")[1]
    if(any(inherit_condition_names) & is.null(condition_names[[first_potential_outcomes_object]])){
      stop("If you choose the inherit_condition_names option for any potential_outcomes, interference, noncompliance, or attrition declarations, the first potential_outcomes object created by declare_potential_outcomes must have condition_names. These will be inherited by any objects that set inherit_condition_names = TRUE.")
    }
    
    if(any(inherit_condition_names)){
      for(i in which(inherit_condition_names == TRUE)){
        condition_names[[i]] <- condition_names[[first_potential_outcomes_object]]
      }
    }
    
  }else{
    condition_names <- replicate(length(potential_outcomes), condition_names, simplify = FALSE)
  }
  
  has_condition_names <- all(sapply(condition_names, function(x) is.null(x))) == FALSE
  
  if(has_condition_names & !has_assignment_variable_names){
    stop("Please provide the name of the treatment variable to the assignment_variable_name argument in declare_potential_outcomes if you provide condition_names.")
  }
  
  if(has_condition_names & has_assignment_variable_names) {
    
    
    for(i in 1:length(potential_outcomes)){
      
      if(potential_outcomes[[i]]$potential_outcomes == TRUE){
        
        data <- draw_observed_outcome(data = data, 
                                      potential_outcomes = potential_outcomes[[i]], 
                                      condition_names= condition_names[[i]])
        
        if(!is.null(potential_outcomes[[i]]$attrition)){
          data <- draw_observed_outcome(data = data, 
                                        potential_outcomes = potential_outcomes[[i]]$attrition, 
                                        condition_names= condition_names[[i]])
          
          data[data[,potential_outcomes[[i]]$attrition$outcome_variable_name]==0, potential_outcomes[[i]]$outcome_variable_name] <- NA
          
        }
        
      }
      
    }
    
    if(!is.null(attrition)){
      data <- draw_observed_outcome(data = data, 
                                    potential_outcomes = attrition, 
                                    condition_names = attrition$condition_names)
      
      for(i in 1:length(potential_outcomes)){
        data[data[,attrition$outcome_variable_name]==0, potential_outcomes[[i]]$outcome_variable_name] <- NA  
      }
    }
  }
  
  return(data)
  
}

#' Determine whether potential outcomes exist in a data frame
#' 
#' @param data data.frame input
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param condition_names A vector indicating the names of conditions
#' @param noncompliance A noncompliance object created by \code{\link{declare_noncompliance}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' 
#' @return indicator for whether potential outcomes exist
#' 
#' @export
has_potential_outcomes <- function(data, potential_outcomes, condition_names = NULL, 
                                   noncompliance = NULL, attrition = NULL){
  
  if(class(potential_outcomes) == "list" & class(condition_names) == "list" &
     length(potential_outcomes) != length(condition_names)){
    stop("If you provide a list of potential_outcomes and a list of condition_names, you must provide a list of condition_names of the same length.")
  }
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "attrition", "noncompliance", "interference"), accepts_list = TRUE)
  
  inherit_condition_names <- sapply(potential_outcomes, function(x) x$inherit_condition_names)
  
  if(!any(lapply(potential_outcomes, function(x) class(x)) == "potential_outcomes") & any(inherit_condition_names) & is.null(condition_names)){
    stop("At least one object sent to the potential_outcomes argument must be created by declare_potential_outcomes.")
  }
  
  noncompliance <- clean_inputs(noncompliance, object_class = "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(attrition, object_class = "attrition", accepts_list = FALSE)
  
  if(!is.null(noncompliance)){
    potential_outcomes <- c(list(noncompliance), potential_outcomes)
  }
  
  if(is.null(condition_names)){
    condition_names <- lapply(potential_outcomes, function(x) x$condition_names)
    first_potential_outcomes_object <- which(sapply(potential_outcomes, function(x) class(x)) == "potential_outcomes")[1]
    if(any(inherit_condition_names) & is.null(condition_names[[first_potential_outcomes_object]])){
      stop("If you choose the inherit_condition_names option for any potential_outcomes, interference, noncompliance, or attrition declarations, the first potential_outcomes object created by declare_potential_outcomes must have condition_names. These will be inherited by any objects that set inherit_condition_names = TRUE.")
    }
    
    for(i in which(inherit_condition_names == TRUE)){
      condition_names[[i]] <- condition_names[[first_potential_outcomes_object]]
    }
    
  }else{
    condition_names <- replicate(length(potential_outcomes), condition_names, simplify = FALSE)
  }
  # You must provide a condition_names argument that makes sense for all po objects.
  
  has_condition_names <- all(sapply(condition_names, function(x) is.null(x))) == FALSE
  has_assignment_variable_names <- all(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name))) == TRUE
  
  if(has_condition_names & !has_assignment_variable_names){
    stop("Please provide the name of the treatment variable to the assignment_variable_name argument in declare_potential_outcomes if you provide condition_names.")
  }
  
  which_po_class <- sapply(potential_outcomes, function(x) class(x) %in% c("potential_outcomes", "noncompliance", "attrition"))
  if(sum(sapply(potential_outcomes[which_po_class], function(x) !is.null(x$assignment_variable_name))) != length(potential_outcomes[which_po_class])){
    stop("If you provide a assignment_variable_name for any of the potential_outcomes, you must provide it for all of them.")
  }
  
  has_potential_outcomes <- list()
  
  if(has_condition_names & has_assignment_variable_names) {
    for(i in 1:length(potential_outcomes)){
      
      # make the combinations
      
      sep <- potential_outcomes[[i]]$sep
      
      condition_combinations <- expand.grid(condition_names[[i]])
      if(is.null(names(condition_names[[i]]))){
        colnames(condition_combinations) <- potential_outcomes[[i]]$assignment_variable_name
      }
      
      has_potential_outcomes[[i]] <- rep(NA, nrow(condition_combinations))
      for(j in 1:nrow(condition_combinations)){
        
        if(ncol(condition_combinations) > 1){
          condition_combination <- lapply(1:ncol(condition_combinations[j, ]), function(x){ condition_combinations[j, x] })
        } else {
          condition_combination <- list(condition_combinations[j, ])
        }
        names(condition_combination) <- colnames(condition_combinations)
        
        outcome_name_internal <- 
          paste(potential_outcomes[[i]]$outcome_variable_name, 
                paste(names(condition_combination), condition_combinations[j,], sep = sep, collapse = sep),
                sep = sep)
        
        has_potential_outcomes[[i]][j] <- all(outcome_name_internal %in% colnames(data))
      }
    }
  }
  return(all(do.call(rbind, has_potential_outcomes)))
}

#' Draw observed outcome
#' 
#' @param data data.frame
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param condition_names A vector of condition names.
#' 
#' @return data.frame including the observed outcome
#' 
#' @export
draw_observed_outcome <- function(data, potential_outcomes, condition_names = NULL){
  
  # Checks -------------------------------------------------
  potential_outcomes <- clean_inputs(potential_outcomes, c("potential_outcomes", "attrition", "noncompliance", "interference"), accepts_list = FALSE)
  
  sep = potential_outcomes$sep
  
  if(class(potential_outcomes) != "interference"){
    
    condition_combinations <- expand.grid(condition_names)
    
    if(is.null(names(condition_names))){
      colnames(condition_combinations) <- potential_outcomes$assignment_variable_name
    }
    
    outcome_name_internal <- list()
    
    for(j in 1:nrow(condition_combinations)){
      
      if(ncol(condition_combinations) > 1){
        condition_combination <- lapply(1:ncol(condition_combinations[j, ]), function(x){ condition_combinations[j, x] })
      } else {
        condition_combination <- list(condition_combinations[j, ])
      }
      names(condition_combination) <- colnames(condition_combinations)
      
      outcome_name_internal[[j]] <- 
        paste(potential_outcomes$outcome_variable_name, 
              paste(names(condition_combination), condition_combinations[j,], sep = sep, collapse = sep),
              sep = sep)
    }
    
    realized_condition_names <- list()
    for(k in 1:ncol(condition_combinations)){
      realized_condition_names[[names(condition_combinations)[k]]] <- unique(data[, names(condition_combinations)[k]])
    }
    
    realized_condition_combinations <- expand.grid(realized_condition_names)
    
    realized_outcome_name_internal <- list()
    for(j in 1:nrow(realized_condition_combinations)){
      
      if(ncol(realized_condition_combinations) > 1){
        realized_condition_combination <- lapply(1:ncol(realized_condition_combinations[j, ]), function(x){ realized_condition_combinations[j, x] })
      } else {
        realized_condition_combination <- list(realized_condition_combinations[j, ])
      }
      names(realized_condition_combination) <- colnames(realized_condition_combinations)
      
      realized_outcome_name_internal[[j]] <- 
        paste(potential_outcomes$outcome_variable_name, 
              paste(names(realized_condition_combination), realized_condition_combinations[j,], sep = sep, collapse = sep),
              sep = sep)
    }
    
    if(all(realized_outcome_name_internal %in% colnames(data))){
      
      ## switching equation
      data[, potential_outcomes$outcome_variable_name] <- NA
      for(j in 1:nrow(condition_combinations)){
        multi_condition_status_internal <- eval(parse(text = paste(colnames(condition_combinations), paste0("'", condition_combinations[j, ], "'"), sep= "==", collapse = "&")), 
                                                envir = data)
        data[multi_condition_status_internal, potential_outcomes$outcome_variable_name] <- 
          data[multi_condition_status_internal, outcome_name_internal[[j]]]
      }
      
      if(!is.null(potential_outcomes$attrition)){
        draw_potential_outcome_vector(data = data, potential_outcomes = potential_outcomes$attrition)
      }
      
    } else {
      # Is correct for continuous!
      data[, potential_outcomes$outcome_variable_name] <- 
        draw_outcome_vector(data = data, potential_outcomes = potential_outcomes)
    }
  } else {
    # Is correct for inteference!
    data[, potential_outcomes$outcome_variable_name] <- 
      draw_outcome_vector(data = data, potential_outcomes = potential_outcomes)
  }
  
  return(data)
}


#' Draw observed outcome (vector)
#' 
#' @param data A data.frame object
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param attrition An attrition object created by \code{\link{declare_attrition}}.
#' 
#' @return a vector of the observed outcome.
#'
#' @export
draw_outcome_vector <- function(data, potential_outcomes, attrition = NULL){
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "attrition", "noncompliance", "interference"), accepts_list = FALSE)
  
  outcome_draw <- potential_outcomes$potential_outcomes_function(data = data)
  
  if( is.atomic(outcome_draw) || is.list(outcome_draw)) {
    outcome_draw <- as.numeric(outcome_draw)
    if(length(outcome_draw) != nrow(data)){
      stop("The potential_outcomes function returned an outcome with a different number of rows than the population data. They must be the same.")
    }
  } else {
    stop("The potential_outcomes function you provided returned something other than a vector, like a matrix. Please edit your potential_outcomes function.")
  }
  
  return(outcome_draw)
  
}

#' Draw potential outcome vector
#'
#' @param data A data.frame object
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param condition_name A vector of condition names.
#'
#' @return a vector of a potential outcome
#'
#' @export
draw_potential_outcome_vector <- function(data, potential_outcomes, condition_name){
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "attrition", "noncompliance", "interference"), accepts_list = FALSE)
  
  for(i in 1:length(condition_name)){
    data[,names(condition_name)[i]] <- condition_name[[i]]
  }
  
  outcome_draw <- potential_outcomes$potential_outcomes_function(data = data)
  
  if( is.atomic(outcome_draw) || is.list(outcome_draw)) {
    outcome_draw <- as.numeric(outcome_draw)
    if(length(outcome_draw) != nrow(data)){
      stop("The potential_outcomes function returned an outcome with a different number of rows than the population data. They must be the same.")
    }
  } else {
    stop("The potential_outcomes function you provided returned something other than a vector, like a matrix. Please edit your potential_outcomes function.")
  }
  
  return(outcome_draw)
  
}

