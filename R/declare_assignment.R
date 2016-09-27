#' Declare the experimental assignment
#'
#' @param condition_names A vector describing the conditions to which subjects can be assigned. Alternatively, condition_names can be obtained from a potential_outcomes object. 
#' @param potential_outcomes potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. The conditions to which subjects can be assigned is obtained from the condition_names stored in a potential outcomes object.  If you prefer, you can use the condition_names argument.
#' @param block_variable_name The name of the variable according to which block random assignment should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random assignment should be conducted.
#' @param m The number of units (or clusters) to be assigned to treatment in a two-arm trial.
#' @param m_each A vector describing the number of units (or clusters) to be assigned to each treatment arm in a multi-arm trial.  Must sum to N (for individually randomized experments) or N_clusters (for cluster randomized experiments).
#' @param probability_each A vector describing the probability of units (or clusters) being assigned to each treatment arm. Must sum to 1.
#' @param block_m A vector describing the number of units to be assigned to treatment in each block in a two-arm trial.
#' @param block_m_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm.
#' @param block_probabilities A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilities of assignment to each treatment arm.
#' @param baseline_condition The value of condition_names that represents the "baseline" condition.  This is the condition against which treatment effects will be assessed. Defaults to the first value of condition_names.
#' @param assignment_variable_name The name of the treatment variable.  Defaults to "Z"
#' @param noncompliance An optional noncomplinance object, as created by \code{\link{declare_noncompliance}}.
#' @param custom_transform_function A function to transform assignments into one or more variables.
#' @param transform_options Options sent to the \code{custom_transform_function}.
#' @param custom_assignment_function A function of data that returns an assignment vector of length n.
#' @param custom_blocking_function  A function of data that returns a blocking vector of length n.
#' @param custom_clustering_function A function of data that returns a cluster vector of length n.
#' @param existing_assignment_variable_name The name of an already-assigned treatment variable.
#' @param description A description of the assignment procedure in words.
#' @param ... options passed to custom assignment function
#'
#' @return assignment object
#' 
#' @examples 
#' 
#' population <- declare_population(individuals = list(noise = "rnorm(n_)",
#'                                    ideo_3 = "sample(c('Liberal', 'Moderate', 'Conservative'), 
#'                                    size = n_, prob = c(.2, .3, .5), replace = T)"),
#'                                  villages = list(elevation = "rnorm(n_)",
#'                                    high_elevation = "1*(elevation > 0)"), 
#'                                  size = c(1000, 100))
#'
#' sampling <- declare_sampling(n = 10, cluster_variable_name = "villages_ID")
#' 
#' potential_outcomes <- declare_potential_outcomes(
#'    formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
#'    condition_names = c(0, 1, 2),
#'    assignment_variable_name = "Z")
#' 
#' # Complete Random Assignment assignments
#' assignment_1 <- declare_assignment(potential_outcomes = potential_outcomes)
#' assignment_2 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m = 60, condition_names = c(0, 1))
#' assignment_3 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 30, 50))
#' assignment_4 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    m_each =c(20, 80), condition_names = c(0, 1))
#' assignment_5 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    probability_each = c(.2, .3, .5))
#' 
#' # Blocked assignments
#' assignment_6 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3")
#' assignment_7 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3", 
#'                                    probability_each = c(.3, .6, .1))
#' assignment_8 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    block_variable_name = "ideo_3", 
#'                                    condition_names = c(0, 1))
#' 
#' block_probabilities <- rbind(c(.1, .2, .7),
#'                     c(.1, .7, .2),
#'                     c(.7, .2, .1),
#'                     c(.7, .1, .2),
#'                     c(.2, .1, .7))
#' assignment_8.5 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                      block_variable_name = "ideo_3",
#'                                      block_probabilities = block_probabilities)
#' 
#' # Clustered assignments 
#' assignment_9 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID")
#' assignment_10 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    condition_names = c(0, 1))
#' assignment_11 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                    cluster_variable_name = "villages_ID", 
#'                                    probability_each = c(.1, .3, .6))
#' 
#' # Blocked and Clustered assignments
#' assignment_12 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation")
#' assignment_13 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation", 
#'                                     condition_names = c(0,1))
#' assignment_14 <- declare_assignment(potential_outcomes = potential_outcomes, 
#'                                     cluster_variable_name = "villages_ID", 
#'                                     block_variable_name = "high_elevation", 
#'                                     probability_each = c(.1, .3, .6))
#' 
#' # Draw Data
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment_1)
#' 
#' # Attempt to Assign
#' smp_draw$Z1 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_1) 
#' smp_draw$Z2 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_2) 
#' smp_draw$Z3 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_3) 
#' smp_draw$Z4 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_4) 
#' smp_draw$Z5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_5) 
#' 
#' smp_draw$Z6 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_6) 
#' smp_draw$Z7 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_7) 
#' smp_draw$Z8 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8) 
#' smp_draw$Z8_5 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_8.5) 
#' 
#' with(smp_draw, table(ideo_3, Z6))
#' with(smp_draw, table(ideo_3, Z7))
#' with(smp_draw, table(ideo_3, Z8))
#' with(smp_draw, table(ideo_3, Z8_5))
#' 
#' smp_draw$Z9 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_9) 
#' smp_draw$Z10 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_10) 
#' smp_draw$Z11 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_11) 
#' 
#' with(smp_draw, table(Z9 ,villages_ID))
#' with(smp_draw, table(Z10,villages_ID))
#' with(smp_draw, table(Z11,villages_ID))
#' 
#' smp_draw$Z12 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_12) 
#' smp_draw$Z13 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_13) 
#' smp_draw$Z14 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_14) 
#' 
#' with(smp_draw, table(Z12, villages_ID))
#' with(smp_draw, table(Z12, high_elevation))
#' 
#' with(smp_draw, table(Z13, villages_ID))
#' with(smp_draw, table(Z13, high_elevation))
#' 
#' with(smp_draw, table(Z14, villages_ID))
#' with(smp_draw, table(Z14, high_elevation))
#' @export
declare_assignment <- 
  function(condition_names = NULL,
           m = NULL, 
           m_each = NULL, 
           probability_each = NULL, 
           block_m = NULL,
           block_m_each = NULL, 
           block_probabilities = NULL,
           baseline_condition = NULL,
           assignment_variable_name = "Z",
           block_variable_name = NULL, 
           cluster_variable_name = NULL,
           potential_outcomes = NULL,
           noncompliance = NULL,
           custom_transform_function = NULL, 
           transform_options = NULL,
           custom_assignment_function = NULL,
           custom_blocking_function = NULL,
           custom_clustering_function = NULL,
           existing_assignment_variable_name = NULL,
           description = NULL,
           assignment = TRUE,
           ...) {
    
    # Determine assignment type
    assignment_type <- "complete"   
    if(!is.null(block_variable_name)) {assignment_type <- "blocked"}
    if(!is.null(cluster_variable_name)) {assignment_type <- "clustered"}
    if(!is.null(cluster_variable_name) & !is.null(block_variable_name)) {
      assignment_type <- "blocked and clustered"
    }
    
    if(!assignment){
      assignment_type <- "none"
    } else {
      
    
    # Checks ------------------------------------------------------------------
    if(assignment_type == "blocked" & !is.null(m)){
      stop("Please do not specify m in a blocked assignment.  Use block_m_each or block_probabilities instead.")
    }
    
    if(!is.null(custom_blocking_function) & !is.character(block_variable_name)){
      stop("If you supply a custom block function, you must supply the name of the block variable.")
    }
    
    if(!is.null(custom_clustering_function) & !is.character(cluster_variable_name)){
      stop("If you supply a custom cluster function, you must supply the name of the cluster variable.")
    }
    
    if(is.null(custom_assignment_function) & is.null(potential_outcomes$condition_names) & is.null(condition_names)){
      stop("Please provide an input to condition_names or a potential_outcomes object with condition_names.")
    }
    
    # Checks -------------------------------------------------
    potential_outcomes <- clean_inputs(potential_outcomes, "potential_outcomes", accepts_list = FALSE)
    noncompliance <- clean_inputs(noncompliance, "noncompliance", accepts_list = FALSE)
    
    if(!is.null(condition_names)){
      condition_names <- clean_condition_names(condition_names)
    }
    
    if(!is.null(potential_outcomes) & !is.null(potential_outcomes$condition_names) & is.null(condition_names)){
      # Obtain Condition Names
      if(class(potential_outcomes) == "list"){
        if(length(unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$outcome_variable_name})))) != length(potential_outcomes)){
          stop("Please use different outcome names in each potential outcomes object.")
        }
        condition_names <- 
          unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$condition_names})))
      }else{
        condition_names <- potential_outcomes$condition_names
      }
    } 
    
    # Figure out baseline condition
    if(!is.null(baseline_condition)){
      if(!(baseline_condition %in% condition_names))
        stop("The baseline treatment condition must match one of the treatment conditions specified in condition_names.")
    }
    if(is.null(baseline_condition)){
      baseline_condition <- condition_names[1]
    }
    
    # add options to custom assignment function
    
    if(!is.null(custom_assignment_function)){
      custom_assignment_function_options <- list(...)
        argument_names <- names(formals(custom_assignment_function))
        if(!is.null(condition_names) & "condition_names" %in% argument_names)
          custom_assignment_function_options$condition_names <- condition_names
        if(!is.null(potential_outcomes) & "potential_outcomes" %in% argument_names)
          custom_assignment_function_options$potential_outcomes <- potential_outcomes
        if(!is.null(block_variable_name) & "block_variable_name" %in% argument_names)
          custom_assignment_function_options$block_variable_name <- block_variable_name
        if(!is.null(cluster_variable_name) & "cluster_variable_name" %in% argument_names)
          custom_assignment_function_options$cluster_variable_name <- cluster_variable_name
        if(!is.null(m) & "m" %in% argument_names)
          custom_assignment_function_options$m <- m
        if(!is.null(m_each) & "m_each" %in% argument_names)
          custom_assignment_function_options$m_each <- m_each
        if(!is.null(probability_each) & "probability_each" %in% argument_names)
          custom_assignment_function_options$probability_each <- probability_each
        if(!is.null(block_m_each) & "block_m_each" %in% argument_names)
          custom_assignment_function_options$block_m_each <- block_m_each
        if(!is.null(block_probabilities) & "block_probabilities" %in% argument_names)
          custom_assignment_function_options$block_probabilities <- block_probabilities
    }
    
    }
    
    if(is.null(custom_assignment_function) & is.null(existing_assignment_variable_name)){
      return.object <- list(block_variable_name = block_variable_name,
                            cluster_variable_name = cluster_variable_name,
                            condition_names = condition_names,
                            m = m,
                            m_each = m_each,
                            probability_each = probability_each,
                            block_m = block_m,
                            block_m_each = block_m_each,
                            block_probabilities = block_probabilities,
                            assignment_type = assignment_type,
                            noncompliance = noncompliance,
                            custom_blocking_function = custom_blocking_function,
                            custom_clustering_function = custom_clustering_function,
                            baseline_condition = baseline_condition,
                            assignment_variable_name = assignment_variable_name,
                            custom_transform_function = custom_transform_function, transform_options = transform_options,
                            description = description,
                            call = match.call())
    } else if(!is.null(custom_assignment_function)) {
      return.object <- list(
        custom_assignment_function = custom_assignment_function,
        custom_assignment_function_options = custom_assignment_function_options,
        condition_names = condition_names,
        baseline_condition = baseline_condition,
        assignment_variable_name = assignment_variable_name,
        noncompliance = noncompliance,
        custom_transform_function = custom_transform_function, transform_options = transform_options,
        assignment_type = "custom",
        description = description,
        call = match.call())
    } else {
      return.object <- list(
        existing_assignment_variable_name = existing_assignment_variable_name,
        condition_names = condition_names,
        baseline_condition = baseline_condition,
        assignment_variable_name = assignment_variable_name,
        noncompliance = noncompliance,
        custom_transform_function = custom_transform_function, transform_options = transform_options,
        assignment_type = "existing assignment",
        description = description,
        call = match.call())
    }
    class(return.object) <- "assignment"
    return(return.object)
  }

# random assignment functions

complete_assignment <- 
  function(N, m = NULL, m_each = NULL, probability_each = NULL, condition_names = NULL, baseline_condition=NULL) {
    
    # Checks
    
    if(!is.null(probability_each) & !is.null(m_each)) {
      stop("Do not specify probability_each and m_each together. Use one or the other.")
    }
    if(!is.null(probability_each) & !is.null(m)) {
      stop("Do not specify probability_each and m together. Use one or the other.")
    }
    if(!is.null(m_each) & !is.null(m)) {
      stop("Do not specify m_each and m together. Use one or the other.")
    }
    if(!is.null(m) & length(condition_names) > 2) {
      stop("Do not specify m when there are more than 2 conditions. Use m_each instead.")
    }
    if(!is.null(m_each) & length(m_each) != length(condition_names)){
      stop("The length of m_each must match the length of condition names. Either exclude some conditions with the excluded_conditions argument or add some conditions in declare_potential_outcomes.")
    }
    if(!is.null(probability_each) & length(probability_each) != length(condition_names)){
      stop("The length of probability_each must match the length of condition names. Either exclude some conditions with the excluded_conditions argument or add some conditions in declare_potential_outcomes.")
    }
    if(all(!is.null(m_each), sum(m_each) != N)) {
      stop("The sum of number assigned to each condition (m_each) must equal the total number of units (N)")
    }
    
    # Setup: obtain number of arms
    num_arms <- length(condition_names)
    
    # Case 0: If there's only one unit and two arms, flip a coin
    if(is.null(m_each) & is.null(probability_each) & num_arms ==2 & N ==1) {
      assign <- sample(condition_names, N, replace = FALSE)
      return(assign)
    }
    if (N < num_arms) {
      assign <- sample(condition_names, N, replace = FALSE)
      return(assign)
    }
    
    # Case 1: If there are two arms, and nothing else is specified, get as close to 50/50 as possible
    if(is.null(m_each) & is.null(probability_each) & num_arms ==2 & N >1) {
      if(is.null(m)) {
        coin_flip <- rbinom(1, 1, 0.5)
        if (coin_flip == 0) 
          m <- floor(N/2)
        if (coin_flip == 1) 
          m <- ceiling(N/2)
      }
      if (m >= N) {
        stop("The number of units assigned to treatment (m) must be less than the total number of units (N)")
      }
      assign <- ifelse(1:N %in% sample(1:N, m),  condition_names[condition_names!=baseline_condition], baseline_condition)
      return(assign)
    }
    
    # Case 2: using m_each (or determining m_each from probability_each)
    
    # Figure out m_each
    if (!is.null(probability_each)) {
      if (sum(probability_each) != 1) {
        stop("If specified, the sum of probability_each must equal 1")
      }
      m_each <- floor(N * probability_each)
      remainder <- N - sum(m_each)
      m_each <- m_each + ifelse(1:length(probability_each) %in% sample(1:length(probability_each), remainder), 1, 0)
    }
    
    # Correct m_each if there is a remainder
    if (is.null(m_each)) {
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <- N%%num_arms
      m_each <- m_each + ifelse(1:num_arms %in% sample(1:num_arms, remainder), 1, 0)
    }
    
    # Conduct complete_assignment with multiple arms
    rand_order <- sample(1:N, replace = FALSE)
    assign <- rep(NA, N)
    for (i in 1:num_arms) {
      assign[rand_order[(sum(m_each[0:(i - 1)]) + 1):sum(m_each[0:i])]] <- condition_names[i]
    }
    return(assign)
  }

blocked_assignment <- 
  function(block_variable, block_m = NULL, block_m_each=NULL, block_probabilities = NULL, probability_each = NULL, condition_names = NULL, baseline_condition=NULL){
    
    # Checks
    
    if(!is.null(block_m) & (!is.null(probability_each) | !is.null(block_m_each) | !is.null(block_probabilities))){
      stop("Do not specify block_m at the same time as probability_each, block_m_each, or block_probabilities.")      
    }
    
    if(!is.null(block_m_each) & !is.null(probability_each)){
      stop("Do not specify both block_m_each and probability_each at the same time.")      
    }
    
    if(!is.null(block_m_each) & !is.null(block_probabilities)){
      stop("Do not specify both block_m_each and block_probabilities at the same time.")      
    }
    
    if(!is.null(probability_each) & !is.null(block_probabilities)){
      stop("Do not specify both probability_each and block_probabilities at the same time.")      
    }
    
    # Setup (obtain unique blocks and create assignment vector)
    
    blocks <- sort(unique(block_variable))
    assign <- rep(NA, length(block_variable))
    
    # Case 1: Assumes equal probabilties for each condition in all block
    # Does complete_assignment() by block
    
    if(is.null(block_m_each) & is.null(probability_each) & is.null(block_probabilities) & is.null(block_m)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_variable==blocks[i])
        assign[block_variable==blocks[i]] <- 
          complete_assignment(N = N_block, 
                              condition_names=condition_names, 
                              baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 2: User specifies exactly how many units will be assigned to each condition, by block
    
    if(!is.null(block_m_each)){
      for(i in 1:length(blocks)){
        if(nrow(block_m_each)!=length(unique(blocks))){
          stop("block_m_each should have the same number of rows as there are unique blocks in block_variable")
        }
        N_block <- sum(block_variable==blocks[i])
        assign[block_variable==blocks[i]] <- complete_assignment(N = N_block, 
                                                                 m_each = block_m_each[i,], 
                                                                 condition_names=condition_names, 
                                                                 baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 3: User specifies the probability of assignment to each condition, but it doesn't vary by block
    
    if(!is.null(probability_each)){
      for(i in 1:length(blocks)){
        if(sum(probability_each)!=1){
          stop("probability_each must sum to 1.")
        }
        N_block <- sum(block_variable==blocks[i])
        assign[block_variable==blocks[i]] <- complete_assignment(N = N_block, 
                                                                 probability_each = probability_each, 
                                                                 condition_names=condition_names, 
                                                                 baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 4: User specifies the probability of assignment to each condition, and it does vary by block
    
    if(!is.null(block_probabilities)){
      for(i in 1:length(blocks)){
        probability_each_local <- block_probabilities[i,]
        if(sum(probability_each_local)!=1){
          stop("Each row of block_probabilities must sum to 1.")
        }
        N_block <- sum(block_variable==blocks[i])
        assign[block_variable==blocks[i]] <- complete_assignment(N = N_block, 
                                                                 probability_each = probability_each_local, 
                                                                 condition_names=condition_names, 
                                                                 baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 5: User specifies block_m
    
    if(!is.null(block_m)){
      for(i in 1:length(blocks)){
        if(length(block_m)!=length(unique(blocks))){
          stop("The length of block_m should be equal to the number unique blocks in block_variable")
        }
        N_block <- sum(block_variable==blocks[i])
        assign[block_variable==blocks[i]] <- complete_assignment(N = N_block, 
                                                                 m = block_m[i], 
                                                                 condition_names=condition_names, 
                                                                 baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
  }

clustered_assignment <- function(cluster_variable, m=NULL, m_each = NULL, probability_each = NULL, condition_names = NULL, baseline_condition=NULL){
  
  # Setup: get unique clusters and the number of clusters
  unique_clus <- unique(cluster_variable)
  n_clus <- length(unique_clus)
  
  # Conduct assignment at the cluster level
  z_clus <- complete_assignment(N = n_clus, 
                                m = m,
                                m_each = m_each, 
                                probability_each = probability_each,
                                condition_names = condition_names,
                                baseline_condition = baseline_condition)
  
  # Merge back up to the individual level, maintaining original ordering
  merged <- merge(x = data.frame(cluster_variable, init_order = 1:length(cluster_variable)), 
                  y = data.frame(cluster_variable=unique_clus, z_clus, stringsAsFactors=FALSE), by="cluster_variable")
  merged <- merged[order(merged$init_order),]
  return(merged$z_clus)
}

blocked_and_clustered_assignment <- 
  function(cluster_variable, block_variable, block_m = NULL,block_m_each=NULL, probability_each=NULL, block_probabilities=NULL,condition_names = NULL, baseline_condition=NULL) {
    
    # confirm that all units within clusters are in the same block
    # is there a computationally faster way to confirm this (possible c++ loop?)
    
    if(!all(rowSums(table(cluster_variable, block_variable) != 0)==1)){
      stop("All units within a cluster must be in the same block.")
    }
    
    # Setup: obtain unique clusters
    unique_clust <- unique(cluster_variable)
    
    # get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clust))
    for(i in 1:length(unique_clust)){
      clust_blocks[i] <- unique(block_variable[cluster_variable==unique_clust[i]])  
    }
    
    # Conduct random assignment at cluster level
    z_clust <- blocked_assignment(block_variable = clust_blocks, 
                                  block_m = block_m,
                                  block_m_each = block_m_each, 
                                  probability_each = probability_each,
                                  block_probabilities = block_probabilities,
                                  condition_names = condition_names, 
                                  baseline_condition = baseline_condition)
    
    # Merge back up to the individual level, maintaining original ordering
    merged <- merge(x = data.frame(cluster_variable, init_order = 1:length(cluster_variable)), 
                    y = data.frame(cluster_variable=unique_clust, z_clust), by="cluster_variable")
    merged <- merged[order(merged$init_order),]
    return(merged$z_clust)
  }

