% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_functions.r
\name{pareto_scale}
\alias{pareto_scale}
\title{apply pareto scaling}
\usage{
pareto_scale(in_data, sub_mean = FALSE)
}
\arguments{
\item{in_data}{the matrix of original data}

\item{sub_mean}{should mean subtraction be performed}
}
\value{
matrix
}
\description{
auto scaling scales by subtracting the mean and dividing by the standard
deviation
}

