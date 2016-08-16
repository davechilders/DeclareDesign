#' @title Template to generate designs with spillovers
#'
#'
#' @details
#' This templating function generates a design in which two units are sampled from a population of 36 units that are aligned in a grid in space. The units can be sampled such that they are a minimum distance from one another using a spatial buffer: each unit is one unit of measurement distant from its vertical or horizontal neighbor and sqrt(2) from its diagonal neighbors. One unit is assigned to treatment and one to control. The user can specify the average treatment effect, the effect of the spillover, the degree to which spatial position (latitude) is correlated with potential outcomes, and the size of the spatial buffer separating units. 

#' @param treatment_effect A real number that indicates the size of the average treatment effect.
#' @param spillover_effect A real number that indicates the size of the spillover effect. 
#' @param latitude_effect A real number that correlates latitude with potential outcomes. The higher the value, the more correlated potential outcomes are with spatial position.
#' @param buffer The minimum units of distance between the two sampled units. 0 by default.
#'
#' @return A design
#' @export

spillover_template <- function(treatment_effect = 1,
                               spillover_effect = 0,
                               latitude_effect = 2,
                               buffer = 0) {
  spatial_data <-
    expand.grid(x = 1:6, y = 1:6)
  spatial_data$id <- 1:nrow(spatial_data)
  
  # Calculate distance of each point to each other point
  distmat <- with(spatial_data, {
    sapply(
      X = 1:length(x),
      FUN = function(i) {
        sapply(
          X = 1:length(x),
          FUN = function(j) {
            sqrt((x[i] - x[j]) ^ 2 + (y[i] - y[j]) ^ 2)
          }
        )
      }
    )
  })
  
  diag(distmat) <- 0
  
  assign(x = "distmat",
         value = distmat,
         envir = globalenv())
  
  # Generate all possible pairs
  pairs <- t(with(spatial_data, combn(id, 2)))
  
  # Generate distances of pairs
  dists <-
    distmat[pairs]
  
  # Generate indicator for all samples that meet the big and small buffers
  pairs <-
    data.frame(unit_1 = pairs[, 1], unit_2 = pairs[, 2])
  
  pairs$outside <- dists > buffer
  
  
  spatial_data$buffer_prob <- with(subset(pairs, subset = outside),
                                   sapply(
                                     X = spatial_data$id,
                                     FUN = function(unit_id)
                                       mean(c(unit_1, unit_2) %in% unit_id)
                                   ))
  
  spatial_data$total_dist <- spatial_data$y ^ 2 / 10
  
  
  population <- declare_population(
    noise = "rnorm(n_)",
    buffer_prob = "spatial_data$buffer_prob",
    distance = "spatial_data$total_dist",
    size = 36,
    options = list(spatial_data = spatial_data,
                   distmat = distmat),
    super_population = TRUE
  )
  
  sample_and_assign <- function(data) {
    sample_probs <- data$buffer_prob
    ids <- 1:length(sample_probs)
    to_sample <-
      sample(
        x = ids,
        size = 2,
        replace = FALSE,
        prob = sample_probs
      )
    to_treat <- sample(x = to_sample, size = 1)
    sampled <- ids %in% to_sample
    treated <- ids %in% to_treat
    return(sampled + treated)
  }
  
  
  assignment <- declare_assignment(condition_names = c(0, 1, 2),
                                   custom_assignment_function = sample_and_assign)
  
  interference <- declare_potential_outcomes(
    formula = E ~ (distmat %*% I(Z == 2) < 4) * I(Z != 2),
    condition_names = c(0, 1, 2)
  )
  
  PO_formula <-
    as.formula(
      paste0(
        "Y ~ ",
        treatment_effect,
        "*I(Z==2) + E_Z_2 *",
        spillover_effect,
        " + noise + distance *",
        latitude_effect
      )
    )
  
  potential_outcomes <- declare_potential_outcomes(
    formula = PO_formula,
    condition_names = c(0, 1, 2),
    assignment_variable_name = "Z"
  )
  
  sampling <- declare_sampling(sampling = FALSE)
  
  estimand <- declare_estimand(
    estimand_text = "1",
    potential_outcomes = potential_outcomes,
    fixed = TRUE,
    label = "fixed_estimand"
  )
  
  IPW_est <- function(data) {
    probs <- data$buffer_prob
    treated <- data$Z == 2
    sampled <- data$Z > 0
    weights <- 1 / probs
    
    lm_fit <- lm(
      formula = Y ~ (Z == 2),
      data = data,
      subset = Z > 0,
      weights = weights
    )
    
    lm_fit$df.residual <- 1
    
    return(lm_fit)
    
  }
  
  
  estimator <- declare_estimator(
    model = IPW_est,
    estimates = get_regression_coefficient,
    coefficient_name = "Z == 2TRUE",
    estimand = estimand
  )
  
  
  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment,
    estimator = estimator,
    potential_outcomes = list(interference, potential_outcomes),
    label = "Spillover Buffer Design"
  )
  
  return(design)
}
