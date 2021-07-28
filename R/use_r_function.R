#'Save a function in the global environment to an R file
#' @param function_object The name of the function object in the Global environment
#' @param path Directory to save function script in. Default is working directory
#' @return Checks to see whether packages are already installed or loaded. If not installed, will install. If already loaded, will skip loading. Otherwise, will load libraries and provide a quick summary.
#' @export
#' @examples
#' \dontrun{
#' hello <- function() { print("hello, world!")}
#'
#' use_r_function("hello")
#'
#' }


use_r_function <- function(function_object, path = NULL) {

  if (is.null(path)) {

    resp <- readline(paste0("save in ", getwd(), "? (y/n) "))

    resp <- tolower(substring(resp, 1, 1))

    if (resp == "n") {

      stop("function not saved. change working directory or specify path via `path` argument")

    }

    path <- getwd()

  }

  tmp_text <- c(paste0(function_object, " <- "), deparse(get(x =  function_object, envir = .GlobalEnv)))
  l <- length(tmp_text)

  if (l > 2) {
    tmp_out <- c(paste(tmp_text[1:2], collapse = ""), tmp_text[3:l])
  } else {
    tmp_out <- paste(tmp_text, collapse = "")
  }

  file_con <- file(paste0(path, "/", function_object, ".R"))
  writeLines(
    tmp_out,
    file_con
  )
  close(file_con)

  message(paste0(function_object,".R saved to ", path))

}
