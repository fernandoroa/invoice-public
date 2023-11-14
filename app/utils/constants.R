#' @export
mon_span <- c(31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31)

#' @export
pattern_a <- "([[:lower:]]+)([[:upper:]])([[:alpha:]]+)([[:digit:]]?)"

#' @export
pattern_b <- "\\1 \\2\\3 \\4"

#' @export
delete_pattern <- "\\bunlink\\b|\\bfile.remove\\b|\\w*(?<!\\w|\\.)rm"

#' @export
command_pattern <- "\\binstall.packages\\b|\\bsystem2\\b|\\bprintf\\b"

#' @export
root_names <- c(
  "currency_exchange_to_Final_Currency", "use",
  "GeneralName", "currency"
)
