#' Find the mode of a vector
#'
#' Given a numeric vector returns the most frequently occurring value.
#' @param x Numeric vector
#' @param na.rm A logical value indicating whether `NA` values should be
#' stripped before the computation proceeds.
#' @return A unit numeric vector representing the most frequent value in `x`
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

#' Scale a vector to be between zero and one
#'
#' Given a numeric vector returns the vector scaled so it ranges between zero
#' and one.
#' @param x Numeric vector
#' @param ... Additional arguments passed to `min` and `max`. Most commonly,
#' `na.rm`.
#' @return `x` transformed to range between zero and one.
#' @export
scale_zero_to_one <- function(x, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}

#' Rolling window computations
#'
#' Computes the value of a summary function over a rolling window of nearby
#' values in the vector.
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
  vapply(
    1:length(x), function(x) fun(x_exp[x:(x + n - 1)], ...),
    FUN.VALUE = fun_value(1)
  )
}


#' Print the time taken to run an expression
#'
#' Sometimes you just want to know how long something took. There are many
#' more elaborate solutions for more formal benchmarking (like microbenchmark),
#' but for some applications you don't want to repeat an action or do formal
#' benchmarking. This function simply evaluates the expression in `expr`
#' and outputs the time taken as a `message`.
#' @param expr An expression to be evaluated.
#' @param message A string describing what you're doing. This will be appended
#' to the start of the printed message. For example, if you set
#' `message = "run things"` the output will look like
#' `Time taken to run things: 5.15 seconds`.
#' @return The result of evaluating `expr` in the environment from which
#' `run_time` was called.
#' @seealso `\link{system.time}` base R solution with more basic output.
#' @export
run_time <- function(expr, message = NULL, digits = 2) {
  preamble <- "Time taken"
  if (!is.null(message)) {
    preamble <- paste(preamble, "to", message)
  }
  start_time <- Sys.time()
  tryCatch(
    {result <- eval(expr, envir = parent.frame())},
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
#' the doParallel package. Simply pass in the expression you'd like to be run in
#' parallel and the result will be returned. Of course, this only helps if the
#' code in `expr`` is written to take advantage of a parallel backend; good
#' examples of this are training models with `caret::train`` or custom code
#' making use of the `parApply` family of functions.
#'
#' Once the code has evaluated successfully or crashed the CPU cluster is shut
#' down, ensuring you don't keep requesting additional resources without freeing
#' any up.
#' @param expr An expression to be evaluated.
#' @param num_cores The number of cores to parallelize across. By default the
#' number of available cores minus one (i.e., `parallel::detectCores() - 1`)
#' is used.
#' @param run_time Should the time taken to run `expr` be printed?
#' @param ... Additional arguments to be passed to [run_time()]
#' @return The result of evaluating `expr`
#' @export
parallelize <- function(expr, num_cores = NULL, run_time = TRUE, ...) {
  if (is.null(num_cores)) num_cores <- parallel::detectCores() - 1
  result <- tryCatch({
    doParallel::registerDoParallel(num_cores)
    if (run_time) {
      run_time(expr, ...)
    } else {
      eval(expr, envir = parent.frame())
    }
  }, finally = {
    doParallel::stopImplicitCluster()
  })
  result
}

#' Compute parallel means
#'
#' Computes the element-wise mean of multiple vectors.
#' @details The motivation behind this function is the base R functions `pmin`
#'   and `pmax`.
#'
#' A more general way to approach this type of problem is with
#'   `[purrr::lift()]`.
#' @export
pmean <- function(..., na.rm = TRUE) {
  inputs <- list(...)
  purrr::pmap_dbl(inputs, ~ mean(c(...), na.rm = na.rm))
}

#' Compute parallel medians
#'
#' Computes the element-wise median of multiple vectors.
#' @details The motivation behind this function is the base R functions `pmin`
#'   and `pmax`.
#'
#' A more general way to approach this type of problem is with
#'   `[purrr::lift()]`.
#' @export
pmedian <- function(..., na.rm = TRUE) {
  inputs <- list(...)
  purrr::pmap_dbl(inputs, ~ median(c(...), na.rm = na.rm))
}

#' Balance a data.frame by a variable's values
#'
#' A common use-case for this is when you have imbalanced classes in your
#' training data for a classifier.
#'
#' @param data A tbl or data.frame. Internally, this function uses dplyr verbs,
#' so it will work for local tables and remote tables in the warehouse.
#' @param var Unquoted variable name to use for balancing.
#' @details Although you might most commonly use this function for binary
#'   outcomes, it will also work if `var` has more than two values. In that case,
#'   the subset for each value of `var` will be sampled down to match the
#'   number of rows in the least common value of `var`.
#'
#'   Note, however, that the set of unique values of `var` is pulled into local
#'   memory when working with remote tbls. As such, you probably shouldn't try
#'   to balance by a categorical variable with many, many values.
#' @return `data` balanced by `var`
#' @export
balance_by <- function(data, var) {
  value_count <- dplyr::count(data, {{var}}, name = "n")
  least_common <- dplyr::filter(value_count, n == min(n))
  least_common_value <- dplyr::pull(least_common, {{var}})
  least_common_count <- dplyr::pull(least_common, n)
  other_values <- value_count %>%
    dplyr::filter(n != min(n)) %>%
    dplyr::pull({{var}})
  least_common_value_data <- dplyr::filter(data, {{var}} == least_common_value)
  downsample_value <- function(value, data, n = least_common_count) {
    data %>%
      dplyr::filter({{var}} == value) %>%
      dplyr::sample_n(n)
  }

  purrr::map(other_values, downsample_value, data = data) %>%
    dplyr::bind_rows(least_common_value_data)
}

#' Find first or last time condition was met
#'
#' This function is designed to be used as part of dplyr pipelines.
#' @param cond Condition to be evaluated
#' @param time_col Unquoted column with timestamps
#' @details This is often useful inside a grouped tibble / data.frame to
#'   calculate the first or last occurence of something per group.
#' @export
first_time <- function(cond, time_col) {
  suppressWarnings(
    dplyr::if_else(cond, {{time_col}}, as.POSIXct(NA)) %>% min(na.rm = TRUE)
  )
}

#' @export
last_time <- function(cond, time_col) {
  suppressWarnings(
    dplyr::if_else(cond, {{time_col}}, as.POSIXct(NA)) %>% max(na.rm = TRUE)
  )
}

#' Extract a single match group
#'
#' @param group Which capture group to extract. Integer.
#' @details Meant to make the common pattern of `str_match()[, 2]` easier to
#'   write and easier to read.
#' @export
str_extract_group <- function(string, pattern, group = 1) {
  stringr::str_match(string, pattern)[, group + 1]
}

#' Extract parameters from a URL
#'
#' @param url Character vector of URLs
#' @param param The name of the parameter to extract as a string
#' @export
extract_url_param <- function(url, param) {
  str_extract_group(
    url,
    glue::glue("\\?.*{param}=([^&]+)", param = param),
    group = 1
  )
}

#' Match strings with SQL-like syntax
#'
#' This is a convenience wrapper around `stringr::str_detect`.
#' @return A logical vector indicating whether each element had a match.
#' @details Inspired by `data.table::like`, which uses `base::grepl` instead.
#'   Because this function uses `stringr::str_detect` the second argument
#'   (the pattern) can also be a vector.
#'
#'   See the help page for [stringr::str_detect()] for more details.
#' @export
`%like%` <- function(string, pattern) {
  stringr::str_detect(string = string, pattern = pattern)
}

#' Check if elements are between two values
#'
#' This is a convenience syntax for the common pattern of `x >= y & x <= z`.
#' The right hand side can be supplied as a simple vector: e.x., `x %between% c(y, z)`.
#' The min and max of the right hand side vector are used for the comparison, so
#' the order of the elements in the vector doesn't matter. This also means you
#' could use the function to succinctly find whether or not an element falls
#' within the range of another vector.
#'
#' @return A logical vector indicating whether each element of the left hand
#' side was between the min and max of the right hand side.
#' @details Both sides of the comparison are inclusive. For example,
#' `1 %between% c(1, 3)` evaluates to `TRUE`, and so does `3 %between% c(1, 3)`.
#'
#' @export
`%between%` <- function(x, y) {
  x >= min(y) & x <= max(y)
}
