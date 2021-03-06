% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/databases.R
\name{file_to_sql}
\alias{file_to_sql}
\title{Convert a file to a SQL list}
\usage{
file_to_sql(path, con)
}
\arguments{
\item{path}{Path to a file of inputs}

\item{con}{Database connection object}
}
\value{

}
\description{
Convert a file to a SQL list
}
\details{
The file should be a plain text file with one entry per line.
See [list_to_sql()] for more details.
}
