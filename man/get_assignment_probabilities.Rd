% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignment_probabilities.R
\name{get_assignment_probabilities}
\alias{get_assignment_probabilities}
\title{Calculate probabilities of assignment}
\usage{
get_assignment_probabilities(data, assignment)
}
\arguments{
\item{data}{A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.}

\item{assignment}{A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment}
}
\value{
A matrix of probabilities of assignment to treatment.
}
\description{
Description
}
\examples{
population <- declare_population(size = 850)
sampling <- declare_sampling(n=500)
potential_outcomes <- declare_potential_outcomes(condition_names = c(0, 1),
                                                 formula = Y ~ 1 + 1*Z)
assignment <- declare_assignment(potential_outcomes = potential_outcomes, m = 200)

design <- declare_design(population = population, 
                         sampling = sampling, 
                         assignment = assignment, 
                         potential_outcomes = potential_outcomes)

smp_draw <- draw_data(design)

assignment_probabilities <- get_assignment_probabilities(data = smp_draw, assignment = assignment)
head(assignment_probabilities)
}

