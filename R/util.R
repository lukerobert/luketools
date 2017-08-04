#' Find the mode of a vector
#'
#' Given a numeric vector returns the most frequently occurring value.
#' @param x Numeric vector
#' @param na.rm A logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds.
#' @return A unit numeric vector representing the most frequent value in \code{x}
#' @export
stat_mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else {
    if (any(is.na(x))) return(NA)
  }
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

#' Compute a summary function over a rolling window
#'
#' blah
#' @param fun Bare, unquoted function to be applied over the window
#' @param x Vector to which \code{fun} will be applied
#' @param n Size of the window
#' @param default Default value used for non-existent values on either end of
#'     \code{x}
#' @param forward A logical value indicating whether the window should go
#'     forwards from each value or backwards (the default)
#' @param fun_value A generalized vector to serve as a template for the return
#'     value from \code{fun}. If \code{NULL} (the default) it is assumed that
#'     the return value is the same type as \code{x}. \code{fun_value} is
#'     simply passed to \code{vapply}, so it uses the same format.
#' @return A vector with the same length as \code{x}.
#' @examples
#'   rolling(mean, rnorm(50), n = 10, default = 0.5)
#'   rolling(paste, 1:10, n = 3, forward = TRUE, fun_value = character, collapse = ",")
#' @export
rolling <- function(fun, x, n, default = NA, forward = FALSE, fun_value = NULL,
                    ...) {
  # Expand x with default values.
  def_vals <- rep(default, n - 1)
  if (forward) {
    x_exp <- c(x, def_vals)
  } else {
    x_exp <- c(def_vals, x)
  }
  if (is.null(fun_value)) {
    fun_value <- get(mode(x))
  }
  vapply(1:length(x), function(x) fun(x_exp[x:(x + n - 1)], ...),
         FUN.VALUE = fun_value(1))
}


#' Print the time taken to run an expression
#'
#' Sometimes you just want to know how long something took. There are many
#' more elaborate solutions for more formal benchmarking (like microbenchmark),
#' but for some applications you don't want to repeat an action or do formal
#' benchmarking. This function simply evaluates the expression in \code{expr}
#' and outputs the time taken as a \code{message}.
#' @param expr An expression to be evaluated.
#' @param format Time formatting. Valid values are:
#' @param message A string describing what you're doing. This will be appended
#' to the start of the printed message. For example, if you set
#' \code{message = "run things"} the output will look like
#' \code{Time taken to run things: 5.15 seconds}.
#' @return The result of evaluating \code{expr} in the environment from which
#' \code{run_time} was called.
#' @seealso \code{\link{system.time}} base R solution with more basic output.
#' @export
run_time <- function(expr, format = "auto", digits = 2, message = NULL) {
  preamble <- "Time taken"
  if (!is.null(message)) {
    preamble <- paste(preamble, "to", message)
  }
  start_time <- Sys.time()
  tryCatch({
      result <- eval(expr, envir = parent.frame())
    },
    finally = {
      end_time <- Sys.time()
      time_taken <- round(end_time - start_time, digits)
      time_units <- attr(time_taken, "units")
      preamble <- paste0(preamble, ":")
      message(paste(preamble, time_taken, time_units))
    }
  )
  result
}

#' Parallelize the evaluation of an expression
#'
#' A convenient wrapper around the parallel processing backend from
#' \code{doParallel}. Simply pass in the expression you'd like to be run in
#' parallel and the result will be returned. Of course, this only helps if the
#' code in \code{expr} is written to take advantage of a parallel backend; good
#' examples of this are training models with \code{caret::train} or custom code
#' making use of the \code{parApply} family of functions.
#'
#' Once the code has evaluated successfully or crashed the CPU cluster is shut
#' down, ensuring you don't keep requesting additional resources without freeing
#' any up.
#' @param expr An expression to be evaluated.
#' @param num_cores The number of cores to parallelize across. By default the
#' number of available cores minus one (i.e., \code{parallel::detectCores() - 1})
#' is used.
#' @param run_time Should the time taken to run \code{expr} be printed?
#' @param ... Additional arguments to be passed to \code{\link{run_time}}
#' @return The result of evaluating \code{expr}
#' @export
parallelize <- function(expr, num_cores = NULL, run_time = TRUE, ...) {
  if (is.null(num_cores)) num_cores <- parallel::detectCores() - 1
  result <- tryCatch({
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    if (run_time) {
      run_time(expr, ...)
    } else {
      eval(expr, envir = parent.frame())
    }
  }, finally = {
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  })
  result
}


#' Get dates of holidays
#'
#' Gives a vector of each holiday date
#' @param year A vector of one or more years for which to list holiday dates
#' @param country Two-letter country code
#' @return Named character vector of holiday dates as "YYYY-MM-DD".
#' The vector's names will be the concatenation of the country code, the
#' holiday name, and the year.
#' @export
holiday_dates <- function(year, country = "US") {
  timeDate_env <- environment(timeDate::listHolidays)
  holiday_funcs <- country %>% purrr::map(timeDate::listHolidays) %>% unlist()
  holiday_names <- holiday_funcs %>%
    purrr::map(~ rep(.x, length(year))) %>%
    unlist() %>%
    paste0(year)
  holidays <- holiday_funcs %>%
    purrr::map(~ eval(call(.x, year), timeDate_env)) %>%
    purrr::map(as.character) %>%
    unlist() %>%
    as.Date() %>%
    magrittr::set_names(holiday_names)

  holidays
}
