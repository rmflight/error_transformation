% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_functions.r
\name{auto_scale}
\alias{auto_scale}
\title{apply auto scaling}
\usage{
auto_scale(in_data, sub_mean = FALSE)
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

