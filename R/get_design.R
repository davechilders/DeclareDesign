
#' Get Design from the DeclareDesign Library of Research Designs
#' 
#' @param name Name of the design
#'  
#' @return A design object or template function
#' 
#' @export
get_design <- function(name){
  design_library_url_R <- "http://blair-data-share.s3-website-us-east-1.amazonaws.com/" ##"https://declaredesign.org/library/R/"
  design_URL <- paste0(design_library_url_R, name, ".rds")
  readRDS(gzcon(url(design_URL)))
}
  
#' Get Design Template from the DeclareDesign Library of Research Designs
#' 
#' @param name Name of the design template
#'  
#' @return An R function
#' 
#' @export
get_template <- function(name){
  design_library_url_R <- "http://blair-data-share.s3-website-us-east-1.amazonaws.com/" ##"https://declaredesign.org/library/R/"
  design_URL <- paste0(design_library_url_R, name, ".rds")
  readRDS(gzcon(url(design_URL)))
}
