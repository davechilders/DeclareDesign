#' Default plot method for diagnosis object
#'
#' @param x Design diagnosis object created by \link{diagnose_design}.
#' @param type Type of the plot to draw. Currently only supports \code{type = "coverage"},
#' @param ... Additional parameters passed to ggplot \code{_facet} function.
#'
#' @return Diagnosis plot produced by ggplot2.
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange facet_wrap facet_grid scale_fill_manual position_dodge labs theme element_text element_rect element_blank
#' @importFrom ggthemes theme_few
#' 
#' @export

plot.diagnosis <- function(x, type = "coverage",...) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("ggthemes", quietly = TRUE)
  
  if (class(x) != "diagnosis") stop("Argument should be of class diagnosis, as an output of diagnose_design()")
  
  if (type == "coverage") {
    
    pars <- list(...)
    pars_default <- list(facet_formula = ~ estimand_label + estimator_label + estimate_label,
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
      ci_names <- names(x$simulations)[ci_search]
      
      dat <- prepare_coverage_data(x$simulations)
      coverage_plot <- 
        ggplot(data = dat) +
        geom_linerange(aes(x = sim, 
                           ymin= get(ci_names[1]), 
                           ymax= get(ci_names[2])), 
                       colour="black", position = position_dodge(0.1)) +
        geom_point(aes(x = sim, y = value, fill = est_estimand), position = position_dodge(0.1),
                   size=1.5, shape=21) +
        theme_few(base_size = 12, base_family = "Helvetica")
      
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
      
      coverage_plot +
        scale_fill_manual(values = c("white", "red"),
                          name= "Value of", 
                          labels=c("Estimator", "Estimand")) +
        labs(title = "Coverage plot",
             y = "Value",
             x = "Simulation") +
        theme(legend.position="top",
              legend.title=element_text(size=10),
              legend.background = element_rect(colour = "black"),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }
  }
}

#' @export
prepare_coverage_data <- function(x) {
  Reduce(function(...) merge(..., all=T), 
         lapply(
           split(x, f = paste(x$estimand_label,x$estimator_label,x$estimate_label)),
           FUN = function(x){
             x <- x[order(x$est),]
             x$sim <- 1:nrow(x)
             x <- stats::reshape(data = x, varying = c("est", "estimand"),
                                 v.names = "value",
                                 timevar = "est_estimand",
                                 times = c("est", "estimand"), direction = "long")
             # x <- tidyr::gather(x, "est_estimand", "value", est, estimand)
             return(x)}))
}


#' @export
mgrep <- function(patterns, x, positions = FALSE,...) {
  if (positions) {
    unname(
      unlist(sapply(patterns, 
                    grep, x, ... = ...)))
  } else {
    apply(
      sapply(patterns, 
             grepl, x, ... = ...), 2, any)
  }
}
