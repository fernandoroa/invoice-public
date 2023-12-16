#' @export
mon_span <- c(31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31)

#' @export
leap_years <- seq(2000, 2096, by = 4)

#' @export
pattern_a <- "([[:lower:]]+)([[:upper:]])([[:alpha:]]+)([[:digit:]]?)"

#' @export
pattern_b <- "\\1 \\2\\3 \\4"

#' @export
delete_pattern <- "\\bunlink\\b|\\bfile.remove\\b|\\w*(?<!\\w|\\.)rm|\\w*(?<!\\w|\\.)mv|file\\.(?!p)"

#' @export
command_pattern <- "\\binstall.packages\\b|\\bsystem2\\b|\\bprintf\\b|\\bCMD\\b|\\bRscript\\b"

#' @export
root_names <- c(
  "currency_exchange_to_Final_Currency", "use",
  "GeneralName", "currency"
)
