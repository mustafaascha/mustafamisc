#' Wide screen
#'
#' Adjust R output to current terminal width (UNIX)
#'
#' @param howWide Accepts numeric input, but it's better to
#'  let the function do its work
#'
#'  @example wide_screen()
#'
#'  @export
wide_screen <- function(howWide = Sys.getenv("COLUMNS")) {
  options(width = as.integer(howWide))
}
