#' Default plot method for diagnosis object
#'
#' @param x Design diagnosis object created by \link{diagnose_design}.
#' @param type Type of the plot to draw. Currently only supports \code{type = "coverage"},
#' @param ... Additional parameters passed to ggplot \code{_facet} function.
#'
#' @return Diagnosis plot produced by ggplot2.
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange facet_wrap facet_grid scale_alpha_manual scale_fill_manual scale_shape_manual position_dodge labs theme element_text element_rect element_blank guides geom_text guide_legend
#' @importFrom ggthemes theme_few
#' @importFrom plyr llply ddply summarize
#'
#' @export

plot.diagnosis <- function(x, type = "coverage",...) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("ggthemes", quietly = TRUE)
  requireNamespace("plyr", quietly = TRUE)

  if (class(x) != "diagnosis") stop("Argument should be of class diagnosis, as an output of diagnose_design()")
  
  if (type == "coverage") {
    if (!("coverage" %in% names(x$simulations))) stop("Coverage diagnosand is not defined with label 'coverage'.")
    
    pars <- list(...)
    pars_default <- list(facet_formula = ~ Estimand + Estimator + Estimate,
                         facet_type = "wrap", 
                         cols = 3, rows = NULL)
    
    for (i in names(pars_default)) {
      if ( i %in% names(pars) ) {
        assign(i, pars[[i]])
      } else {
        assign(i, pars_default[[i]]) 
      }
    } 
    
    ci_search <- grepl("ci|confidence|interval|c_i", 
                       names(x$simulations), ignore.case=TRUE)
    est_estimand_search <- grepl("^estimand$|^est$", 
                                 names(x$simulations), ignore.case=TRUE)
    if ( all(!ci_search) ) {
      stop("Confidence intervals are not specified in diagnostic statistics with names containing any of the following: 'ci', 'confidence', 'interval', 'c_i'.")
    } else if (sum(ci_search) > 2) {
      stop("Non-unique confidence intervals specified in diagnostic statistics with names containing any of the following: 'ci', 'confidence', 'interval', 'c_i'.")
    } else if (sum(est_estimand_search) != 2) {
      stop("Estimand or estimator labels are misspecified or are non-unique in simulations.")
    } else {
      
      names(x$simulations)[ci_search] <- c("ci_lower", "ci_upper")
      
      x$simulations <- rename(df = x$simulations, 
                              from = c("estimand_label",
                                       "estimator_label",
                                       "estimate_label"),
                              to = c("Estimand",
                                     "Estimator",
                                     "Estimate"))
      
      dat_cov <- 
        plyr::ddply(.data = x$simulations, 
                    .variables = c("Estimand",
                                   "Estimator",
                                   "Estimate"),
                    .fun = plyr::summarize,
                    cover = paste("Pr(covered) =", 
                                  round(sum(coverage)/length(coverage), digits = 3) ),
                    max_y_point = roundup(max(ci_upper),digit = 1),
                    min_x_point = 1)
      
      dat_cov$max_y_point <- rep(max(dat_cov$max_y_point), times = nrow(dat_cov))
      
      dat <- prepare_coverage_data(simulations = x$simulations)
      coverage_plot <- 
        ggplot(data = dat) +
        geom_linerange(aes(x = sim, 
                           ymin = ci_lower, 
                           ymax = ci_upper,
                           alpha = coverage),
                       color = "#1f78b4",
                       position = position_dodge(0.1)) +
        geom_point(aes(x = sim, y = value, 
                       fill = est_estimand,
                       shape = est_estimand), 
                   position = position_dodge(0.1),
                   size = 2*scaler(count_simulations(x$simulations)), 
                   stroke = 0.3*scaler(count_simulations(x$simulations)),
                   alpha = 1*scaler(count_simulations(x$simulations))) +
        ggthemes::theme_few(base_size = 12, base_family = "Helvetica")
      
      if (facet_type == "wrap") {
        coverage_plot <- 
          coverage_plot +
          facet_wrap(facet_formula,
                     labeller = "label_both",
                     scales = "fixed", ncol = cols, nrow = rows)
      } else if (facet_type == "grid") {
        coverage_plot <- 
          coverage_plot +
          facet_grid(facet_formula,
                     labeller = "label_both",
                     scales = "fixed")
      }
      
      if (all(dat_cov$cover == "Pr(covered) = 1")) {
        coverage_plot <- 
          coverage_plot +
          scale_alpha_manual(name = "Estimand is",
                             labels = c("in CI"),
                             values = c(0.3*scaler(count_simulations(x$simulations))))
      } else {
        coverage_plot <- 
          coverage_plot +
          scale_alpha_manual(name = "Estimand is",
                             labels = c("not in CI", "in CI"),
                             values = c(.8*scaler(count_simulations(x$simulations)), 
                                        0.3*scaler(count_simulations(x$simulations))))
      }
      
      coverage_plot <- 
        coverage_plot +
        geom_text(data = dat_cov, aes(x = min_x_point, y = max_y_point, label = cover),
                  vjust = "inward", hjust = "inward",
                  inherit.aes = FALSE, parse=FALSE) +
        scale_shape_manual(name = "Value of", 
                           labels = c("Estimator", "Estimand"),
                           values = c(21,23)) +
        scale_fill_manual(name = "Value of", 
                          values = c("#33a02c","#e31a1c"),
                          labels=c("Estimator", "Estimand")) +
        labs(title = "Coverage plot",
             y = "Value",
             x = "Simulations (sorted by estimate value)") +
        theme(legend.position = "top",
              legend.title = element_text(size=10),
              legend.background = element_rect(colour = "black", size = 0.1),
              legend.box = "horizontal",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
      if (all(dat_cov$cover == "Pr(covered) = 1")) {
        coverage_plot +
          guides(shape = guide_legend(override.aes = list(size=2, alpha = 1)),
                 alpha = guide_legend(override.aes = list(size=3, alpha = .4)))
      } else {
        coverage_plot +
          guides(shape = guide_legend(override.aes = list(size=2, alpha = 1)),
                 alpha = guide_legend(override.aes = list(size=3, alpha = c(1,.4))))
      }
    }
  }
}

#' @export
roundup <- function(x, digit) {
  if(length(x) != 1) stop("'x' must be of length 1")
  return(ceiling(x*10^digit)/10^digit)
}

#' @export
scaler <- function(x) {
  if (x > 200) {
    return(1/4)
  } else if (x > 100) {
    return(2/4)
  } else if (x > 50) {
    return(3/4)
  } else {
    return(1) }
}

#' @export
count_simulations <- function(simulations) {
  simulations$comb <- 
    paste(simulations$Estimand,
          simulations$Estimator,
          simulations$Estimate)
  sims <- nrow(simulations)/length(unique(simulations$comb))
  if (sims - floor(sims) != 0) stop("Number of rows in simulations of diagnosis object is not a multiple of unique combinations of estimand, estimator and estimate.")
  return(sims)
}

#' @export
prepare_coverage_data <- function(simulations) {
  Reduce(function(...) merge(..., all=T), 
         lapply(
           split(simulations, f = paste(simulations$Estimand,
                                        simulations$Estimator,
                                        simulations$Estimate)),
           FUN = function(x){
             x <- x[order(x$est,x$coverage),]
             x$sim <- 1:nrow(x)
             x <- stats::reshape(data = x, varying = c("est", "estimand"),
                                 v.names = "value",
                                 timevar = "est_estimand",
                                 times = c("est", "estimand"), direction = "long")
             return(x)}))
}

#' @export
rename <- function(df, from, to) {
  if (length(from) != length(to)) stop("Arguments 'from' and 'to' should be vectors of the same length")
  for (i in 1:length(from)) {
    names(df)[names(df) == from[i]] <- to[i]
  }
  return(df)
}
