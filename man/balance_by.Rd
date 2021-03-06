% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{balance_by}
\alias{balance_by}
\title{Balance a data.frame by a variable's values}
\usage{
balance_by(data, var)
}
\arguments{
\item{data}{A tbl or data.frame. Internally, this function uses dplyr verbs,
so it will work for local tables and remote tables in the warehouse.}

\item{var}{Unquoted variable name to use for balancing.}
}
\value{
`data` balanced by `var`
}
\description{
A common use-case for this is when you have imbalanced classes in your
training data for a classifier.
}
\details{
Although you might most commonly use this function for binary
  outcomes, it will also work if `var` has more than two values. In that case,
  the subset for each value of `var` will be sampled down to match the
  number of rows in the least common value of `var`.

  Note, however, that the set of unique values of `var` is pulled into local
  memory when working with remote tbls. As such, you probably shouldn't try
  to balance by a categorical variable with many, many values.
}
