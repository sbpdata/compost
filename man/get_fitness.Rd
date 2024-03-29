% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fitness.R
\name{get_fitness}
\alias{get_fitness}
\title{Helper function for fitness-algorithm functions for multi-year data}
\usage{
get_fitness(df, algorithm, N)
}
\arguments{
\item{df}{data frame containing the necessary columns:
time, region, unit, rca}

\item{algorithm}{character listing the algorithm to be applied.
Possible values are: "fitness2012" or "fitness2018"}

\item{N}{number of iterations to run algorithm}
}
\value{
see algorithm functions
}
\description{
Nests data by year and applies the chosen algorithm
}
