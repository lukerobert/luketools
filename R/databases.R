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

#' Retrieve or set keychain credentials
#'
#' This function queries the system keychain for credentials for a given service
#' and gives user prompts (in RStudio) if no existing credentials are found.
#' @return A list with two elements: "username" and "password"
#' @export
get_or_set_credentials <- function(service, force_ask = FALSE) {
  if (force_ask || nrow(keyring::key_list(service)) == 0) {
    # Ask user for credentials if they don't exist
    # Register credentials with system keychain
    rstudioapi::showDialog(
      "Keychain credential setup",
      message = paste0(
        "Let's set up your credentials for ",
        service, ". ",
        "You'll be prompted for a username and password, which will be ",
        "stored in the system keychain."
      )
    )
    username <- rstudioapi::showPrompt(
      "Username",
      message = paste("Enter your username for", service),
      default = ""
    )
    password <- keyring::key_set(
      service = service, username = username
    )
  }
  username <- keyring::key_list(service)[1, 2]
  password <- keyring::key_get(service)
  list(username = username, password = password)
}

# Evaluate an expression with creds, re-asking if there's an error
with_credentials <- function(service, expr, n_tries = 3) {
  for (i in 1:n_tries) {
    tryCatch({
      if (i > 1) {
        get_or_set_credentials(service, force_ask = TRUE)
      } else {
        get_or_set_credentials(service)
      }
      eval(expr)
    },
    error = function(e) {
      if (i < n_tries) {
        warning(
          paste(
            "Failed to evaluate with given credentials.",
            e,
            "Try again.",
            sep = "\n"
          )
        )
      } else {
        stop("Unable to connect with given credentials.")
      }
    }
    )
  }
}

