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

#' Quarter within fiscal year
#'
#' @param date A vector of dates
#' @return An integer vector the same length as `date` giving the quarter in the
#' fiscal year that each date falls in.
#' @details Assumes that quarters are each exactly three months and start on
#'   the same day of the month.
#'   If you want both fiscal year and quarter you can use `fiscal_year`
#'   together with `fiscal_quarter`.
#' @export
fiscal_quarter <- function(date, start_month = 7, start_day = 1) {
  # TODO: make this work with other fiscal year definitions.
  q1_bounds <- lurbidate::interval()
  date_month <- lubridate::month(date)
  dplyr::case_when(
    date_month %in% 7:9 ~ 1,
    date_month %in% 10:12 ~ 2,
    date_month %in% 1:3 ~ 3,
    date_month %in% 4:6 ~ 4
  )
}

# TODO: fill this out and fix timezone package upstream
find_local_time <- function(unix_time, lat, lon) {
  with_tz_v <- Vectorize(lubridate::with_tz, SIMPLIFY = FALSE)
  # The timezone package is not correctly implemented so we need to manually
  # load some of the functions it uses internally.
  SpatialPoints <- sp::SpatialPoints
  timezone <- dplyr::na_if(timezone::find_tz(y = latitude, x = longitude), "NA")
  # Make this work with normal timestamps (not just unixtime)
  timestamp <- as.POSIXct(unix_time, origin = "1970-01-01", tz = "GMT")
  # There's a few things going on here:
  # 1: lubridate::with_tz is not vectorized, so we need to fix that
  # 2: POSIXct objects have timezones as an attribute which would be lost
  #    in a normal "unlist" operation, so we need to convert to character
  #    (including timezone strings) and then back to POSIXct
  login_timestamp_local <- with_tz_v(login_timestamp, tzone = timezone) %>%
    purrr::map_chr(as.character, usetz = TRUE) %>%
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S")
  login_hour_local = lubridate::hour(login_timestamp_local)
  login_minute_local = lubridate::minute(login_timestamp_local)
}

# Add region shading and periodic summaries to a time series plot.
# TODO: actually do this
geom_time_summary <- function() {
  for (fy in years) {
    # Alternate background panel colors.
    color <- dplyr::if_else(fy %% 2 == 0, "deepskyblue", "mediumpurple")
    fy_total_bookings <- forecast %>%
      dplyr::filter(fiscal_year == fy) %>%
      dplyr::summarize(!! pred_var := sum(!! pred_var_sym)) %>%
      dplyr::mutate(
        bookings_str = paste0(
          "$", round(!! pred_var_sym / 1e6, digits = 1), "M"
        )
      ) %>%
      dplyr::pull(bookings_str)
    # Add annotations with projected bookings for each year.
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = as.Date(paste0(fy - 1, "-07-01")),
        xmax = as.Date(paste0(fy, "-06-30")),
        ymin = -Inf, ymax = Inf,
        fill = color, alpha = 0.2
      ) +
      ggplot2::annotate(
        "text",
        y = annotation_y,
        x = as.Date(paste0(fy, "-01-01")),
        label = paste0("FY", stringr::str_sub(fy, start = 3)),
        color = paste0(color, 3)
      ) +
      ggplot2::annotate(
        "text",
        y = annotation_y - annotation_y * 0.06,
        x = as.Date(paste0(fy, "-01-01")),
        label = fy_total_bookings, color = paste0(color, 3)
      )
  }
}
