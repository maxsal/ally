#' Initialize a diary for recording periodic project notes
#' @param path The file path at which to make a diary. Default is current working directory
#' @return Checks to see whether a diary exists in the working directory and if one does not exist, create one
#' @importFrom cli cli_alert_success
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' new_diary()
#' }

new_diary <- function(path = NULL) {

  # check for path argument -----------
  if (is.null(path)) {

    path <- getwd()

  }

  # check for existing diary -----------
  if (file.exists(paste0(path, "/.diary.rds"))) {

    stop(paste0(".diary.rds already exists at: ", path))

  } else {

    init      <- readline(prompt = paste0("create diary at: ", path, "? (y/n) "))
    init_resp <- tolower(substring(init, 1, 1))

    if (init_resp == "n") {

      stop("diary not created")

    }
  }

  saveRDS(
    tibble::tibble(
      entry_id = 0,
      entry_date = Sys.Date(),
      entry_time = format(Sys.time(), "%H:%M"),
      entry      = "initialize diary"
    ),
    file = paste0(path, "/.diary.rds")
  )

  cli::cli_alert_success("congrats on your new diary :)")

}

#' Make a diary entry
#' @param entry The entry you want to make in your diary
#' @param path File path corresponding to diary. Default is working directory
#' @return Checks to see whether packages are already installed or loaded. If not installed, will install. If already loaded, will skip loading. Otherwise, will load libraries and provide a quick summary.
#' @importFrom cli cli_alert_success
#' @importFrom tibble add_row
#' @export
#' @examples
#' \dontrun{
#' dear_diary("why won't my R code run? :(")
#' }

dear_diary <- function(entry, path = NULL) {

  if (is.null(path)) {
    path <- getwd()
  }

  tmp_file_path <- paste0(path, "/.diary.rds")

  if (!file.exists(tmp_file_path)) {
    stop(paste0("diary does not exist at: ", path,". Change `path` argument or create a diary using `new_diary()`"))
  }

  tmp_diary <- readRDS(tmp_file_path)

  new_id <- max(tmp_diary$entry_id) + 1

  saveRDS(tmp_diary %>%
            tibble::add_row(
              "entry_id"   = new_id,
              "entry_date" = Sys.Date(),
              "entry_time" = format(Sys.time(), "%H:%M"),
              "entry"      = entry
            ),
          file = tmp_file_path
          )

  cli::cli_alert_success("Entry {new_id} recorded")

}

#' Read diary
#' @param path File path corresponding to diary. Default is working directory
#' @param id Read a specific diary entry by id
#' @param date Read diary entries on a specific date
#' @return Read diary entries
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom cli cli_alert_success
#' @importFrom tibble add_row
#' @export
#' @examples
#' \dontrun{
#' read_diary()
#' }
#'

read_diary <- function(path = NULL, id = NULL, date = NULL) {

  if (is.null(path)) {
    path <- getwd()
  }

  tmp_file_path <- paste0(path, "/.diary.rds")

  if (!file.exists(tmp_file_path)) {
    stop(paste0("diary does not exist at: ", path,". Change `path` argument or create a diary using `new_diary()`"))
  }

  if (!is.null(id) & !is.null(date)) {

    stop("Specify only one of `id` or `date`. Set both to NULL (i.e., `read_diary()`) to read full diary")

  } else if (!is.null(id)) {

    readRDS(tmp_file_path) %>%
      dplyr::filter(entry_id == id)

  } else if (!is.null(date)) {

    readRDS(tmp_file_path) %>%
      dplyr::filter(entry_date == date)

  } else {
    readRDS(tmp_file_path) %>%
      dplyr::arrange(desc(entry_id))
  }

}

#' Destroy diary
#' @param path File path corresponding to diary. Default is working directory
#' @return Delete diary file
#' @importFrom cli cli_alert_success
#' @export
#' @examples
#' \dontrun{
#' destroy_diary()
#' }
#'

destroy_diary <- function(path = NULL) {

  if (is.null(path)) {
    path <- getwd()
  }

  tmp_file_path <- paste0(path, "/.diary.rds")

  if (!file.exists(tmp_file_path)) {
    stop(paste0("diary does not exist at: ", path,". Change `path` argument or create a diary using `new_diary()`"))
  }

  init <- readline(prompt = paste0("are you sure you want to destroy the diary at: ", path, "? (y/n) "))

  init_resp <- tolower(substring(init, 1, 1))

  if (init_resp == "n") {

    stop("diary not destroyed")

  }

  unlink(tmp_file_path)

  cli::cli_alert_success("diary destroyed")

}
