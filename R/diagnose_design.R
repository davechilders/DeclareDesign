

#' Diagnose a design object
#'
#' @param design Design object created by \link{declare_design}.
#' @param population_draws Number of simulated populations to draw.
#' @param sample_draws Number of samples to draw per population.
#' @param assignment_draws Number of random assignments to draw per sample if there is sampling or per population if there is no sampling.
#'
#' @return Diagnosis object, with simulations of estimates and estimands and estimated diagnosands.
#' @export
diagnose_design <-
  function(design,
           population_draws = 2,
           sample_draws = 2,
           assignment_draws = 2,
           bootstrap_diagnosands = TRUE,
           population_replicates = 50) {
    
    ## core operations
    
    design <- clean_inputs(design, "design", accepts_list = FALSE)
    
    # check to ensure that the design is complete
    
    if(any(is.null(design$population),
           is.null(design$sampling),
           is.null(design$assignment),
           is.null(design$estimator),
           is.null(design$potential_outcomes),
           is.null(design$diagnosand))){
      stop("Your design object does not include one or more of: population, sampling, assignment, estimator, potential_outcomes, diagnosand")
    }
    
    
    population_estimands <-
      sample_estimands <- assignment_estimands <- estimates <- list()
    
    estimators_population_estimands <-
      get_estimator_at_level(design$estimator, estimand_level = "population")
    estimators_sample_estimands <-
      get_estimator_at_level(design$estimator, estimand_level = "sample")
    estimators_assignment_estimands <-
      get_estimator_at_level(design$estimator, estimand_level = "assignment")
    
    for (i in 1:population_draws) {
      ## draw population
      
      population_data <-
        draw_population(
          population = design$population,
          potential_outcomes = design$potential_outcomes
        )
      
      ## if any population estimands, get estimands
      ## write a thing that only does get_estimands on the ones for this level
      
      
      if (length(estimators_population_estimands) > 0) {
        population_estimands[[i]] <-
          data.frame(
            get_estimands(estimator = estimators_population_estimands,
                          data = population_data),
            population_draw = i
          )
      }
      
      sample_estimands[[i]] <- list()
      assignment_estimands[[i]] <- list()
      estimates[[i]] <- list()
      for (j in 1:sample_draws) {
        sample_data <-
          draw_sample(data = population_data, sampling = design$sampling)
        
        ## if any sample estimands, get estimands
        ## write a thing that only does get_estimands on the ones for this level
        if (length(estimators_sample_estimands) > 0) {
          sample_estimands[[i]][[j]] <-
            data.frame(
              get_estimands(estimator = estimators_sample_estimands, data = sample_data),
              population_draw = i,
              sample_draw = j
            )
          
        }
        
        assignment_estimands[[i]][[j]] <- list()
        estimates[[i]][[j]] <- list()
        for (k in 1:assignment_draws) {
          sample_data <-
            assign_treatment(data = sample_data, assignment = design$assignment)
          
          ## if any assignment estimands, get estimands
          ## write a thing that only does get_estimands on the ones for this level
          
          sample_data <-
            draw_outcome(data = sample_data,
                         potential_outcomes = design$potential_outcomes)
          
          if (length(estimators_assignment_estimands) > 0) {
            assignment_estimands[[i]][[j]][[k]] <-
              data.frame(
                get_estimands(estimator = estimators_assignment_estimands, data = sample_data),
                population_draw = i,
                sample_draw = j,
                assignment_draw = k
              )
          }
          estimates[[i]][[j]][[k]] <-
            data.frame(
              get_estimates(estimator = design$estimator, data = sample_data),
              population_draw = i,
              sample_draw = j,
              assignment_draw = k
            )
          
        }
        
      }
      
    }
    
    level_indicators_df <-
      expand.grid(
        population_draw = 1:population_draws,
        sample_draw = 1:sample_draws,
        assignment_draw = 1:assignment_draws
      )
    
    ## put together the pop estimand
    
    if (length(estimators_population_estimands) > 0) {
      population_estimands <- do.call(rbind, population_estimands)
      population_estimands <-
        merge(population_estimands, level_indicators_df, by = "population_draw")
      population_estimands <- population_estimands[, sort(names(population_estimands))]
    }else{
      population_estimands <- NULL
    }
    
    ## put together the sample estimand
    if (length(estimators_sample_estimands) > 0) {
      sample_estimands <-
        do.call(rbind, lapply(
          sample_estimands,
          FUN = function(x)
            do.call(rbind, x)
        ))
      sample_estimands <-
        merge(sample_estimands,
              level_indicators_df,
              by = c("population_draw", "sample_draw"))
      sample_estimands <- sample_estimands[, sort(names(sample_estimands))]
    }else{
      sample_estimands <- NULL
    }
    
    ## put together the assign estimand
    if (length(estimators_assignment_estimands) > 0) {
      assignment_estimands <- super_unlist_rbind(assignment_estimands)
      assignment_estimands <- assignment_estimands[, sort(names(assignment_estimands))]
    }else{
      assignment_estimands <- NULL
    }
    
    ## put together the estimates
    estimates <- super_unlist_rbind(estimates)
    
    ## put together the estimands    
    estimands_df <-
      rbind(population_estimands,
            sample_estimands,
            assignment_estimands)
    
    if(!is.null(estimands_df)){
      simulations_df <-
        merge(
          estimands_df,
          estimates,
          by = c(
            "estimator_label",
            "estimand_label",
            "estimand_level",
            "population_draw",
            "sample_draw",
            "assignment_draw"
          )
        )
    }
    
    simulations_df <-
      simulations_df[order(
        simulations_df$population_draw,
        simulations_df$sample_draw,
        simulations_df$assignment_draw
      ),]
    
    simulations_names <- names(simulations_df)
    fixed_names <-
      c(
        "population_draw",
        "sample_draw",
        "assignment_draw",
        "estimand_label",
        "estimand",
        "estimand_level",
        "estimator_label",
        "estimate_label"
      )
    non_fixed <-
      simulations_names[!simulations_names %in% fixed_names]
    
    simulations_df <- simulations_df[, c(fixed_names, non_fixed)]
    
    rownames(simulations_df) <- NULL
    
    simulations_df <- get_diagnostic_statistics(diagnosand = design$diagnosand, simulations = simulations_df)
    
    diagnosands_df <- get_diagnosands(diagnosand = design$diagnosand, simulations = simulations_df)
    
    if(bootstrap_diagnosands == TRUE){
      diagnosands_df_bootstrap_sd <- bootstrap_diagnosand(simulations_df = simulations_df,
                                                          diagnosand = design$diagnosand,
                                                          population_replicates = population_replicates)
      
      diagnosands_df <- merge(diagnosands_df, diagnosands_df_bootstrap_sd, by = c("estimand_label", "estimator_label", "estimand_level", "diagnosand_label"))
      
    }
    
    # Sorting the diagnosands df
    diagnosand_levels <- sapply(design$diagnosand, function(x)x$label)
    diagnosands_df <- within(diagnosands_df,{
      diagnosand_label <- factor(diagnosand_label, levels = diagnosand_levels)
    })
    
    diagnosands_df <- diagnosands_df[with(diagnosands_df, order(estimand_label, estimator_label, estimand_level, estimate_label, diagnosand_label)),] 
    rownames(diagnosands_df) <- NULL
    diagnosis <- list(diagnosands = diagnosands_df, simulations = simulations_df)
    
    structure(diagnosis, class = "diagnosis")
    
  }

#' @export
print.diagnosis <- function(x, ...){
  print(summary(x))
  invisible(summary(x))
}

#' @export
summary.diagnosis <- function(object, ...) {
  if (length(list(...)) > 0) {
    diagnosis_matrix <- 
      object$diagnosands[ eval(parse(text = parse_conditions(df = object$diagnosands, ...))), ]
  } else if (length(list(...)) == 0) {
    diagnosis_matrix <- object$diagnosands
  }
  structure(diagnosis_matrix, class = c("summary.diagnosis", "data.frame"))
}

#' @export
print.summary.diagnosis <- function(x, ...){
  class(x) <- "data.frame"
  cat("\nResearch design diagnosis\n\n")
  print_diagnosis <- x
  names(x) <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub("_", " ", names(x)), perl = TRUE)
  print(x)
  cat("\n")
  invisible(x)
}

#' @export
parse_conditions <- function(df,...) {
  pars <- list(...)
  pars <- lapply(pars, FUN = function(x) x = paste0(gsub("[[:punct:]]", "", tolower(x)), 
                                                    collapse = "|"))
  
  paste(paste0('grepl(pattern = "', pars, '", x = gsub("[[:punct:]]", "", x =', deparse(substitute(df)), '$', 
               tolower(names(pars)), '_label), ignore.case = TRUE)'), 
        collapse = ' & ')
}

#' @export
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

bootstrap_diagnosand_draw <- function(simulations_df){
  
  populations <- split(simulations_df, simulations_df$population_draw)
  
  populations <- sample(populations, size = length(populations), replace = TRUE)
  
  assignments <- 
    lapply(populations, function(x){
      samples <- split(x, x$sample_draw)
      samples <- sample(samples, size = length(samples), replace = TRUE)
      
      assignments <- 
        lapply(samples, function(x){
          assignments <- split(x, x$assignment_draw)
          sample(assignments, size = length(assignments), replace = TRUE)
          
        })
    })
  
  return(super_unlist_rbind(assignments))
  
}

bootstrap_diagnosand <- function(simulations_df, diagnosand, population_replicates = 50){
  diagnosand <- clean_inputs(diagnosand, object_class = "diagnosand", accepts_list = TRUE)
  boot_list <- lapply(X = 1:population_replicates, FUN = function(x) bootstrap_diagnosand_draw(simulations_df))
  diagnosands_replicates <- do.call(rbind, lapply(boot_list, get_diagnosands, diagnosand = diagnosand))
  
  
  labels <- paste0(sapply(diagnosand, function(x)x$label), collapse = "`+`")
  diagnosands_summary <- aggregate(cbind(diagnosand_sd_boot = diagnosand) ~ estimand_label + estimator_label + estimand_level + diagnosand_label, 
                                   data = diagnosands_replicates, FUN = sd, na.action = na.pass)
  
  return(diagnosands_summary)
}

get_estimator_at_level <- function(estimator, estimand_level) {
  for (i in 1:length(estimator)) {
    if (!is.null(estimator[[i]]$estimand)) {
      estimands_at_level <-
        sapply(estimator[[i]]$estimand, function(x)
          x$estimand_level == estimand_level)
      if (sum(estimands_at_level) != 0) {
        estimator[[i]]$estimand <-
          estimator[[i]]$estimand[estimands_at_level]
      } else {
        estimator[[i]]$estimand <- NULL
      }
      estimator[[i]]$estimand_label <-
        estimator[[i]]$estimand_label[estimands_at_level]
    }
  }
  
  estimand_counts <-
    sapply(estimator, function(i)
      length(i$estimand))
  
  if (sum(estimand_counts) > 0) {
    estimator <- estimator[estimand_counts > 0]
    return(estimator)
  } else{
    return(NULL)
  }
  
  
}

super_unlist_rbind <- function(q) {
  return_value <-
    do.call(rbind,
            lapply(
              q,
              FUN = function(x)
                do.call(rbind,
                        lapply(
                          x,
                          FUN = function(y)
                            do.call(rbind, y)
                        ))
            ))
  return(return_value)
}
