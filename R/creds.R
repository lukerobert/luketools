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
  # TODO: fix this
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
