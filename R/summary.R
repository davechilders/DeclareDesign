
#' @export
summary.design <- function(object, ...) {
  
  ## population
  population <- summary(object$population)
  
  ## potential outcomes
  potential_outcomes <- summary.potential_outcomes(object$potential_outcomes)
  
  ## sampling
  if(!is.null(object$sampling)){
    sampling <- summary(object$sampling)
  } else {
    sampling <- "The design is carried out on the population."
  }  
  
  ## assignment
  assignment <- summary.assignment(object$assignment)
  
  ## estimators (with estimands)
  estimators <- summary.estimator(object$estimator)
  
  ## combine
  summary_text <- list(population = population, potential_outcomes = potential_outcomes,
                       sampling = sampling, assignment = assignment, estimators = estimators)
  
  structure(summary_text, class = c("summary.design", "design"))
  
}

#' Summary of design in code
#'
#' @param design A design object created by \code{\link{declare_design}}.
#'
#' @return character string representing the design in R code
#' 
#' @export
summary_code <- function(design){
  
  stitch_call <- function(call, formals){
    
    function_name <- call[[1]]
    
    call <- as.list(call)
    call[[1]] <- NULL
    
    as.call(c(function_name, call, formals[!(names(formals) %in% c("...", names(call))) & sapply(formals, function(i) is.null(i)) == FALSE]))
    
  }
  
  design <- clean_inputs(design, "design", accepts_list = FALSE)
  
  paste_skip_lines <- function(x) {
    paste(x, collapse = "\n\n")
  }
  
  steps <- c("population", "potential_outcomes", "sampling", "assignment", "estimator")
  code <- list()
  for(i in 1:length(steps)){
    step_object <- get(steps[i], design)
    if(class(step_object) == "list"){
      code_list <- list()
      for(j in 1:length(step_object)){
        code_list[[j]] <- trim_spaces(paste(deparse(stitch_call(step_object[[j]]$call, formals(get(paste0("declare_", steps[i]))))), collapse = " "))
        if(steps[i] == "estimator" & !is.null(step_object[[j]]$estimand)){
          code_list[[j]] <- paste_skip_lines(list(code_list[[j]], trim_spaces(paste(deparse(stitch_call(step_object[[j]]$estimand$call, formals(get(paste0("declare_estimand"))))), collapse = " "))))
        }
      }
      code[[i]] <- paste_skip_lines(code_list)
    } else {
      code[[i]] <- trim_spaces(paste(deparse(stitch_call(step_object$call, formals(get(paste0("declare_", steps[i]))))), collapse = " "))
    }
  }
  cat(paste_skip_lines(code))
}

#' @export
print.summary.design <- function(x, ...){
  ## prints paragraph describing assignment
  ##cat_bk <- function(x) cat(x, sep = "\n\n")
  do.call(cat, x)
  
}


# POPULATION ----------------------------------------------------------------------------------

#' Summarizing population
#'
#' Summary method for population object created by \code{\link{declare_population}}.
#'
#' @param object A population object as created by \code{\link{declare_population}}.
#' @param extended Logical. Whether to print extended summary, including variable summary statistics and levels. Default is \code{TRUE}.
#' @param stat_list If \code{extended = TRUE}, a list of named summary statistics, which should be reported for each variable declared in population. By default returns \code{min}, \code{max}, \code{mean}, \code{median} and \code{sd}.
#' @param digits Integer. Number of decimal points to prin for summary statistics. Default is 2.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return If \code{extended = FALSE} function returns list with one element, \code{summary_text}, which contains text explanation of population structure. If \code{extended = TRUE}, function returns list with 4 elements: \code{summary_text} as explained above, \code{stat}, which contains named matrix of summary statistics specified in \code{stat_list} argument, \code{code}, which contains function call which creates population object supplied, and \code{internal}, which contains list of all necessary data and options to re-create the population object using the \code{code}.
#' 
#' @examples
#' # ADD EXAMPLES HERE
#' 
#' @export

summary.population <- function(object, extended = TRUE, 
                               stat_list = list(max = max,
                                                min = min,
                                                mean = mean,
                                                median = median,
                                                sd = sd),
                               digits = 2,...) {
  
  object <- clean_inputs(object, "population", accepts_list = FALSE)
  
  if (length(environment(object$population)) == 6) {
    size <- get("size", envir = environment(object$population))
    levels <- get_level_names(object$population)
    level_sizes <- ifelse(is.null(levels), "individuals", paste(size, levels, collapse = " in "))
    
    short_text <- paste0("The population is defined as ", 
                            ifelse(length(size) > 1, level_sizes, paste(size, "units")), ".")
    
    data <- draw_population(object)
    var_structure <- environment(object$population)$expressions
    extended_text <- 
      paste0("\n\nVariable summary (summary statistics for one draw of population)\n",
             paste(sapply(names(var_structure), 
                          FUN = function(x) if (length(var_structure[[x]]) != 0) { 
                            paste(
                              ifelse(length(var_structure[[x]]) == 1, "variable", "variables"), 
                              paste(names(var_structure[[x]]), collapse = ", "), 
                              ifelse(length(var_structure[[x]]) == 1, "changes", "change"),
                              "at the level of the", x) 
                          } else {
                            paste("no variables change at the level of the", x)
                          }, simplify = FALSE), 
                   collapse = ", "),
             ".\n")
    
  } else if (length(environment(object$population)) == 3) {
    size <- get("size_internal", envir = environment(object$population))
    short_text <- 
      paste0("The population is defined via custom DGP function as ", 
             ifelse(length(size) > 1, 
                    paste(size, paste0("unit_level_", 1:length(size)), collapse = " in "), 
                    paste(size, "units")), 
             " (level labels are not specified).")
    
    if (!is.null(environment(object$population)$data_internal)) {
      data <- environment(object$population)$data_internal
      extended_text <- 
        summary_text(data = data,
                     pre = "\n\nVariable summary (summary statistics for user-provided data)\n",
                     mid = "specified via user data:")
      
    } else if (is.null(environment(object$population)$data_internal)) {
      data <- draw_population(object)
      extended_text <- 
        summary_text(data = draw_population(object),
                     pre = "\n\nVariable summary (summary statistics for one draw of population)\n",
                     mid = "specified via custom population function:")
    }
  }
  
  if (!extended) {
    summary_text <- short_text
    structure(list(summary_text = summary_text), class = c("summary.population", "population"))
  } else if (extended & length(stat_list) != 0) {
    summary_text <- paste0(short_text,extended_text)
    variable_summaries <- 
      round(multi.sapply(data = data[,!grepl("ID", names(data), ignore.case = TRUE)],
                         stat_list = stat_list), 
            digits = digits)
    
    
    
    structure(list(summary_text = summary_text, 
                   stat = variable_summaries,
                   code = paste0(deparse(object$call), collapse = "\n"),
                   internal = list(options = environment(object$population)$options,
                                   data = environment(object$population)$data_internal)), 
              class = c("summary.population", "population", "extended"))
    
  } else {
    stop("Please provide at least one summary statistic in stat_list.")
  }
}

#' @export
print.summary.population <- function(x, ...){
  if ("extended" %in% class(x)) { 
    cat(x$summary_text) 
    print(x$stat) 
    if (!is.null(x$code)) cat("\n\nPopulation call", x$code, sep = "\n")
  } else {
    cat(x$summary_text)
  }
  attr(x, "class") <- c("summary.population", "population", "list") 
  invisible(x)
}

# POTENTIAL OUTCOMES --------------------------------------------------------------------------



#' Summarizing potential outcomes
#'
#' Summary method for potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#'
#' @param object A potential_outcomes object as created by \code{\link{declare_potential_outcomes}}.
#' @param extended Logical. Whether to print extended summary including labels of potential outcomes delcared and function call which re-creates the potential outcomes object. Default is \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return If \code{extended = FALSE} function returns list with one element, \code{summary_text}, which contains text explanation of potential outcomes structure. If \code{extended = TRUE}, function returns list with 3 elements: \code{summary_text} as explained above, \code{po_labels}, which contains a named list of labels for all declared potential outcomes, and \code{code}, which contains function call which creates potential outcomes object supplied.
#' @examples
#' # ADD EXAMPLES HERE
#' 
#' @export

summary.potential_outcomes <- function(object, extended = TRUE, ...) {
  object <- clean_inputs(object, "potential_outcomes", accepts_list = TRUE)
  
  summary_text <- code <- c()
  po_labels <- list()
  
  for(i in 1:length(object)){
    
    if ( 
      ("formula" %in% ls(environment(object[[i]]$potential_outcomes_function))) & 
      !is.null(environment(object[[i]]$potential_outcomes_function)$formula) 
    ){
      
      po_formula <- 
        formula_as_character(get("formula", 
                                 envir = environment(object[[i]]$potential_outcomes_function)))
      
      summary_text[i] <- paste0("An outcome ", object[[i]]$outcome_variable_name, 
                              " is defined by the formula\n", po_formula)
      
    } else if ( 
      ("formula" %in% ls(environment(object[[i]]$potential_outcomes_function))) &
      !is.null(environment(object[[i]]$potential_outcomes_function)$potential_outcomes_function) 
    ) {
      
      summary_text[i] <- 
        paste0("An outcome ", object[[i]]$outcome_variable_name, 
               " is defined by a custom function\n",
               paste0(
                 deparse(
                   environment(object[[i]]$potential_outcomes_function)$potential_outcomes_function),
                 collapse = " "))
      
      object[[i]]$call[["potential_outcomes_function"]] <- 
        environment(object[[i]]$potential_outcomes_function)$potential_outcomes_function
      
    }
    
    if (!is.null(object[[i]]$condition_names) & 
        !is.null(object[[i]]$assignment_variable_name) &
        !is.null(object[[i]]$sep)) {
      
      po_labels[[i]] <- 
        sapply(X = object[[i]]$assignment_variable_name, 
               FUN = function(x) { if (is.list(object[[i]]$condition_names)){
                 paste(x, object[[i]]$condition_names[[x]], sep = object[[i]]$sep)
               } else {
                 paste(x, object[[i]]$condition_names, sep = object[[i]]$sep)
               }
               },
               simplify = TRUE)
      
      po_labels[[i]] <- 
        apply(X = expand.grid(split(x = po_labels[[i]], 
                                    f = rep(1:ncol(po_labels[[i]]),
                                            each = nrow(po_labels[[i]])))), 
              MARGIN = 1, FUN = paste0, 
              collapse = object[[i]]$sep)
      
    } else if (object[[i]]$inherit_condition_names) {
      po_labels[[i]] <- "inherit_condition_names = TRUE. The labels of potential outcomes will be inherited from the first potential_outcomes object created by declare_potential_outcomes with specified condition names."
    }
    
    names(po_labels)[i] <- object[[i]]$outcome_variable_name
    
    code[i] <- paste0(deparse(object[[i]]$call), collapse = "\n")
    
  }
  
  if (!extended) {
    structure(list(summary_text = summary_text), 
              class = c("summary.potential_outcomes", "potential_outcomes"))
  } else if (extended) {
    structure(list(summary_text = summary_text,
                   po_labels = po_labels,
                   code = code), 
              class = c("summary.potential_outcomes", "potential_outcomes", "extended"))
  }
}

#' @export
print.summary.potential_outcomes <- function(x, ...){
  if ("extended" %in% class(x)) { 
    cat(paste0(x$summary_text, 
               "\n\nLabels of potential outcomes\n", 
               sapply(x$po_labels, paste0, collapse = ", ", simplify = TRUE), 
               "\n\nPotential outcomes call\n", 
               x$code), 
        sep = "\n\n")
  } else {
    cat(x$summary_text)
  }
  attr(x, "class") <- c("summary.potential_outcomes", "potential_outcomes", "list") 
  invisible(x)
}


# SAMPLING ------------------------------------------------------------------------------------


#' @export
summary.sampling <- function(object, ...) {
  object <- clean_inputs(object, "sampling", accepts_list = FALSE)
  summary_text <- paste0("The sampling strategy is ", object$sampling_type, " random assignment.")
  structure(summary_text, class = c("summary.sampling", "sampling"))
}


#' @export
print.summary.sampling <- function(x, ...){
  cat(x)
}


# ASSIGNMENT ----------------------------------------------------------------------------------

#' @export
summary.assignment <- function(object, ...) {
  object <- clean_inputs(object, "assignment", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    summary_text[[i]] <- paste0("The assignment strategy is ", object[[i]]$assignment_type, " random assignment for the treatment labeled ", object[[i]]$assignment_variable_name,
                                ", and the possible treatment conditions are ", paste(object[[i]]$condition_names, collapse = " and "), ".")
  }
  summary_text <- do.call(paste0, summary_text)
  structure(summary_text, class = c("summary.assignment", "assignment"))
}

#' @export
print.summary.assignment <- function(x, ...){
  ## prints paragraph describing assignment
  cat(x)
}



# ESTIMATOR -----------------------------------------------------------------------------------

summary.estimator <- function(object, ...) {
  object <- clean_inputs(object, "estimator", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    if(!is.null(object[[i]]$model)){
      formula <- formula_as_character(get("formula", envir = environment(object[[i]]$model)))
      if(identical(get("model", envir = environment(object[[i]]$model)), lm)){
        model <- "linear regression"
      } else if(identical(get("model", envir = environment(object[[i]]$model)), glm)){
        model <- "generalized linear model"
      } else {
        model <- object[[i]]$model_name
      }
      if(identical(get("estimates", envir = environment(object[[i]]$estimates)), DeclareDesign::get_regression_coefficient)){
        summary_text[[i]] <- paste0("the coefficient for ", get("estimates_options", envir = environment(object[[i]]$estimates))$coefficient_name, " from a ", model)
      }
    } else {
      if(identical(object[[i]]$estimates, DeclareDesign::difference_in_means)){
        estimator <- "difference-in-means"
      } else if(identical(get("estimates", envir = environment(object[[i]]$estimates)), DeclareDesign::difference_in_means_blocked)) {
        estimator <- "block-adjusted difference-in-means"
      } else {
        estimator <- object[[i]]$estimates_name
      }
      summary_text[[i]] <- paste0(estimator)
    }
    summary_text[[i]] <-  paste0(ifelse(length(object) > 1, paste0("(", i, ifelse(is.null(object$label), "", paste(", labeled", object$label)), ") "), ""), 
                                 "calculated using ", summary_text[[i]])
  }
  paste_semi <- function(...) paste(... = ..., sep = "; ")
  summary_text <- paste0(ifelse(length(object) == 1, "There is one estimator: ", paste0("There are ", length(object), " estimators: ")), do.call(paste_semi, summary_text), ".")
  structure(summary_text, class = c("summary.estimator", "estimator"))
}

#' @export
print.summary.estimator <- function(x, ...){
  ## prints paragraph describing assignment
  cat(x)
}



# USED FUNCTIONS ------------------------------------------------------------------------------

#' @export
flatten_list <- function(list) {
  if (is.list(list)) {
    lapply(list, flatten_list) 
  } else {
    enquote(list)
  }
}

#' @export
summary_text <- function(data, 
                         pre = "\n\nVariable summary\n", 
                         mid = "specified via user data:", 
                         post = ".\n\n") {
  return(
    paste0(pre,
           paste("there are", ncol(data), 
                 ifelse(ncol(data) == 1, "variable", "variables"), 
                 mid,
                 paste(colnames(data), collapse = ", ")),
           post)
  )
}

#' @export
multi.sapply <- function(data,stat_list) {
  var.names <- sapply(stat_list, deparse)
  has.name <- ( names(stat_list) != "" )
  var.names[has.name] <- names(stat_list)[has.name]
  result <- sapply(stat_list, function (FUN, x) sapply(x, FUN), x = data)
  colnames(result) <- var.names
  return(result)
}

#' @export
get_level_names <- function(population){
  level_ID_variables <- get_level_IDs(expression_list = get("expressions", envir = environment(population)), 
                                      level_IDs = get("level_IDs", envir = environment(population)), 
                                      N_levels = get("N_levels", envir = environment(population)))
  return(gsub("[[:space:]]", "", tolower(gsub("_", " ", gsub("_ID", "", level_ID_variables)))))
}
