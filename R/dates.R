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

#' Fiscal year of a date
#'
#' Many organizations use "fiscal years" as their primary time period for
#' calculating and reporting metrics. This function lets you specify the month
#' and day of the fiscal year and calculate the fiscal year of each date in a
#' vector.
#' @param date A vector of dates
#' @param start_month Integer giving the first month of the fiscal year
#' @param start_day Integer giving the first day of the fiscal year
#' @return An integer vector the same length as `date` giving the fiscal year
#' for each respective date.
#' @export
fiscal_year <- function(date, start_month = 7, start_day = 1) {
  dplyr::if_else(
    date < as.Date(
      paste(lubridate::year(date), start_month, start_day, sep = "-")
    ),
    true = lubridate::year(date),
    false = lubridate::year(date) + 1
  )
}

#' Number of days since start of fiscal year
#'
#' @param date A vector of dates
#' @param start_month Integer giving the first month of the fiscal year
#' @param start_day Integer giving the first day of the fiscal year
#' @return A numeric vector the same length as `dates` giving
#' @export
fiscal_year_day <- function(date, start_month = 7, start_day = 1) {
  fy_single_date <- function(date) {
    date <- as.Date(date)
    fy_start <- as.Date(paste(fiscal_year(date), start_month, start_day, sep = "-"))
    as.numeric(date - fy_start, unit = "days")
  }
  vapply(date, fy_single_date, FUN.VALUE = numeric(1))
}

#' Fiscal year and quarter
#'
#' @param date A vector of dates
#' @param sep Character separator passed on to `paste`
#' @return A character vector the same length as `date` giving the quarter and
#'   year within which each date falls.
#' @details Since this is not a standard date format the return result is a
#'   character vector. By default the year and quarter are separated by a "-",
#'   e.x., "2020-Q2".
#' @export
fiscal_year_quarter <- function(date, sep = "-") {
  paste(fiscal_year(date), fiscal_quarter(date), sep = sep)
}
