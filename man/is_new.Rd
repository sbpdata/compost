% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_new.R
\name{is_new}
\alias{is_new}
\title{Find "new" entries in time series data}
\usage{
is_new(data, t0_threshold = 0.5, t1_threshold = 1)
}
\arguments{
\item{data}{Data frame containing variables for time (fx year),
region (fx country), unit (fx product) and rca (revealed comparative
advantage or location quotient). All variables must be present. Data
should follow the tidy format.}

\item{t0_threshold}{Numeric of length 1. The value that rca should be below in
current period, in order to be 'new'.}

\item{t1_threshold}{Numeric of length 1. The value that rca should be above in
next period, in order to be 'new' (only if t0_threshold also holds).}
}
\value{
data frame that contains the original values plus two new:
previous_rca, which is the rca value for the region in the unit the
previous time-period, and is_new, which takes the value 1 if the
threshold conditions are passed, 0 if not. is_new is NA if there is no
next rca value present (i.e. for all values in the latest time period).
}
\description{
Calculates if a unit is 'new' in a region through two threshold conditions.
t0 threshold refer to the current time period, t1 refer
to the next. A product is given value 1 for "new" in the t0 period. In other
words, if the unit is newly introduced in t1, the t0 value has a "new" value.
This seems unintutive, but this makes it easier to extract the relatedness
metrics for the t0-period in analysis. The input data must contain
the four variables listed.
}
