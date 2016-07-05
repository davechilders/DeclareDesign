#' Check Design Object
#' 
#' Check consistency in design object
#' 
#' @param design A design object created by \code{\link{declare_design}}.
#' 
#' @export

check_design <- function(design) {
  
  if (class(design) != "design") stop("The design argument must have class 'design'")
  
  message_list <- list()
  message_index <- 1
  # Run through draw_ functions to detect errors at any step
  
  pop_error <- try(
    draw_population(population = design$population, potential_outcomes = design$potential_outcomes),
    silent = TRUE
  )
  
  if (class(pop_error) == "try-error") {
    message_list[[message_index]] <- 
      paste0(message_index, ". Conflict between potential outcomes and population declarations with message: \n", 
             pop_error[[1]], "\n")
    message_index <- message_index + 1
  } else {
    sample_error <- try(
      draw_sample(data = pop_error, sampling = design$sampling),
      silent = TRUE
    )
    
    if (class(sample_error) == "try-error") {
      message_list[[message_index]] <- 
        paste0(message_index, ". Conflict between sampling and population/potential outcomes declarations with message: \n", 
               sample_error[[1]], "\n")
      message_index <- message_index + 1
    } else {
      assign_error <- try(
        assign_treatment(data = sample_error, assignment = design$assignment),
        silent = TRUE
      )
      
      if (class(assign_error) == "try-error") {
        message_list[[message_index]] <- 
          paste0(message_index, ". Conflict between assignment and sampling declarations with message: \n", 
                 assign_error[[1]], "\n")
        message_index <- message_index + 1
      } else {
        outcome_error <- try(
          draw_outcome(data = assign_error, potential_outcomes = design$potential_outcomes),
          silent = TRUE
        )
        
        if (class(outcome_error) == "try-error") {
          message_list[[message_index]] <- 
            paste0(message_index, ". Conflict between assignment and potential outcomes declarations with message: \n", 
                   outcome_error[[1]], "\n")
          message_index <- message_index + 1
        }
      }
    }
  }
  
  # Check sanity of get_ functions
  
  if (class(pop_error) == "data.frame"){
    for (i in 1:length(design$estimator)){
      
      estimand_error <- try(
        get_estimands(estimand = design$estimator[[i]]$estimand, data = pop_error),
        silent = TRUE
      )
      
      if (class(estimand_error) == "try-error" | 
          length(estimand_error) != 1 |
          !is.numeric(estimand_error)) {
        message_list[[message_index]] <-
          paste0(message_index, ". Conflict between assignment and sampling declarations with message: \n",
                 ifelse(class(estimand_error) == "try-error", yes = estimand_error[[1]],
                        no = paste0( "Error in get_estimands(estimand = design$estimator[[1]]$estimand, data = pop_error) :\n",
                                     ifelse(length(estimand_error) != 1, 
                                            yes = "There should be only one estimand defined \n",
                                            no = "Estimand should return numeric value \n")
                                     )
                 )
          )
        message_index <- message_index + 1
      }
    }
    if (class(outcome_error) == "data.frame") {
      estimator_error <- try(
        get_estimates(estimator = design$estimator, data = outcome_error),
        silent = TRUE
      )
      if (dim(estimator_error)[2] != length(design$estimator)) {
        message_list[[message_index]] <-
          paste0(message_index, ". Conflict in number of estimates provided with message: \n",
                 "Error in get_estimates(estimator = design$estimator, data = outcome_error) :\n",
                 "The estimates table does not have length(design$estimator) of columns \n")
        message_index <- message_index + 1
      }
    }
  }
  
  # Check that labels of estimands and estimator are unique
  
  estimator_labels <- sapply(design$estimator, FUN = function(x) x$labels)
  estimand_labels <- sapply(design$estimator, FUN = function(x) x$estimand$labels[7])
  
  for (i in c("estimator","estimand")){
    if (length(unique(get(paste0(i, "_labels")))) != length(get(paste0(i, "_labels"))) ) {
      message_list[[message_index]] <-
        paste0(message_index, ". Conflict in names of estimators with message: \n Labels of ", 
               i, "s should be unique \n")
      message_index <- message_index + 1
    }
  }

  # Check consistency of notation for assignment
  
  
  design$estimator[[1]]$estimand$label 
  
  estimator_labels <- sapply(design$estimator, FUN = function(x) x$labels)
  estimand_labels <- sapply(design$estimator, FUN = function(x) x$estimand$label)
  
  for (i in c("estimator","estimand")){
    if (length(unique(get(paste0(i, "_labels")))) != length(get(paste0(i, "_labels"))) ) {
      message_list[[message_index]] <-
        paste0(message_index, ". Conflict in labels of", i, "s with message: \n Labels of ", 
               i, "s should be unique \n")
      message_index <- message_index + 1
    }
  }
  
  assignment_variable_names_in_po <- 
    sapply(design$potential_outcomes, 
           FUN = function(x) x$assignment_variable_name)
  assignment_variable_names_in_estimator <- 
    sapply(design$estimator, 
           FUN = function(x) sapply(x$estimand$potential_outcomes, 
                                    FUN = function(x) x$assignment_variable_name))
  
  if (length(unique(assignment_variable_names_in_po)) != 1) {
    message_list[[message_index]] <-
      paste0(message_index, ". Conflict in names of assignment variables with message: \n",
             "Names of assignment variables are not unique in potential outcomes declaration \n")
    message_index <- message_index + 1
  } else if (length(unique(assignment_variable_names_in_estimator)) != 1) {
    message_list[[message_index]] <-
      paste0(message_index, ". Conflict in names of assignment variables with message: \n",
             "Names of assignment variables are not unique in potential outcomes declaration within estimator \n")
    message_index <- message_index + 1
  } else if (length(unique(c(assignment_variable_names_in_po,
                      assignment_variable_names_in_estimator) )) != 1) {
    message_list[[message_index]] <-
      paste0(message_index, ". Conflict in names of assignment variables with message: \n",
             "Names of assignment variables are not the same in potential outcomes and estimates declaration \n")
    message_index <- message_index + 1
  } else if ( !all(grepl(unique(assignment_variable_names_in_estimator), estimand_labels)) ) {
    message_list[[message_index]] <-
      paste0(message_index, ". Conflict between names of assignment variables and estimand definition with message: \n",
             "Names of assignment variables do not match names used in estimand definition \n")
    message_index <- message_index + 1
  }
  
  if (length(message_list) > 0) warning(message_list)
  
}


