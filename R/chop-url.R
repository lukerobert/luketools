#' Chop off URL subdomains and directories
#'
#' @param url Vector of URLs to process
#' @param n_dirs Number of directories (e.x., lukedav.is/_blog_) to keep.
#'   Pass `NULL` (the default) to keep all and 0 to remove all.
#' @param n_subdomains Number of subdomains (e.x., _blog_.lukedav.is) to keep.
#'   #'   Pass `NULL` (the default) to keep all and 0 to remove all.
#' @param show_regex Should the generated regex be printed? This can be useful
#'   if you want to repeat the same "chopping" in another system that supports
#'   regex, like a SQL query.
#' @details For more general URL extraction tasks check out the urltools package.
#'   This function should _not_ be used to verify if an input is a valid URL;
#'   Its rules for TLDs (e.x., ".com", ".org") are extremely generous and it
#'   isn't intended to validate the URL format more broadly.
#' @return A vector of URLs the same length as the input with directories
#'   and / or subdomains truncated.
#' @export
chop_url <- function(url, n_dirs = 1, n_subdomains = 1, show_regex = FALSE) {
  protocol <- "(?:https?:\\/\\/)?(?:www\\.)?"
  subdomain_start <- "((?:[^\\.]+\\.)"  # Domains above the root
  subdomain_end <- ")?"
  if (is.null(n_subdomains) || n_subdomains == 0) {
    subdomain_rep <- "*"
  } else if (n_subdomains == 1) {
    subdomain_rep <- "{{{n_subdomains}}}"
  } else {
    subdomain_rep <- "{{1,{n_subdomains}}}"
  }
  root_domain <- "([^\\.]+\\.(?:org|com))"
  dir_start <- "((?:/[^/\\?]*)"  # Directories past the domain
  dir_end <- ")?"
  if (is.null(n_dirs) || n_dirs == 0) {
    dir_rep <- "*"
  } else if (n_dirs == 1) {
    dir_rep <- "{{{n_dirs}}}"
  } else {
    dir_rep <- "{{1,{n_dirs}}}"
  }
  # Compile full regex pattern string.
  pattern <- glue::glue(
    protocol,
    subdomain_start, subdomain_rep, subdomain_end,
    root_domain,
    dir_start, dir_rep, dir_end,
    n_dirs = n_dirs,
    n_subdomains = n_subdomains
  )
  if (show_regex) print(pattern)
  full_match <- stringr::str_match(url, pattern = pattern)
  if (is.na(full_match[, 1])) return(NA)
  # Extract constituent parts of match and reconstruct "chopped" url.
  subdomains <- full_match[, 2]
  match <- full_match[, 3]
  dirs <- full_match[, 4]
  if (!is.null(n_subdomains) && n_subdomains > 0) {
    if (is.na(subdomains)) subdomains <- ""
    match <- paste0(subdomains, match)
  }
  if (!is.null(n_dirs) && n_dirs > 0) {
    if (is.na(dirs)) dirs <- ""
    match <- paste0(match, dirs)
  }
  match
}
