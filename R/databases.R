#' Convert an R vector to a SQL list
#'
#' @param x Vector or list to be converted
#' @param con Database connection object
#' @return
#' @export
list_to_sql <- function(x, con) {
  glue::glue_sql("{x*}", .con = con)
}

#' Convert a file to a SQL list
#'
#' @param path Path to a file of inputs
#' @param con Database connection object
#' @details The file should be a plain text file with one entry per line.
#' See [list_to_sql()] for more details.
#' @return
#' @export
file_to_sql <- function(path, con) {
  x <- readr::readlines(path)
  list_to_sql(x, con)
}

#' Submit a query over a connection
#'
#' @param query An atomic character vector giving either a raw SQL query or
#' the file path to a .sql file
#' @param con A database connection object usable by the `DBI` package
#' @param params A named list of parameters to pass to the query. Use "{param_name}"
#' in `query` to indicate a parameter.
#' @return A data.frame with the result of running `query`
#' @export
submit_query <- function(query, con, params = NULL) {
  # Read query from file if it's a SQL file
  if (tolower(tools::file_ext(query)) == "sql") {
    query <- readr::read_file(query)
  }
  # Otherwise assume it's a SQL string
  # TODO: test / fix passing vectors as input params to SQL lists
  con <- pool::poolCheckout(con)
  if (!is.null(params)) {
    glue_params <- append(query, params)
    query <- purrr::lift(glue::glue_sql)(glue_params, .con = con)
  }
  query_obj <- DBI::dbSendQuery(conn = con, statement = query)
  result <- DBI::dbFetch(query_obj)
  pool::poolReturn(con)
  DBI::dbClearResult(query_obj)
  result
}
