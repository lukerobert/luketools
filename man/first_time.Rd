% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{first_time}
\alias{first_time}
\title{Find first or last time condition was met}
\usage{
first_time(cond, time_col)
}
\arguments{
\item{cond}{Condition to be evaluated}

\item{time_col}{Unquoted column with timestamps}
}
\description{
This function is designed to be used as part of dplyr pipelines.
}
\details{
This is often useful inside a grouped tibble / data.frame to
  calculate the first or last occurence of something per group.
}
