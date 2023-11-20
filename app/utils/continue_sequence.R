box::use(
  shiny[isTruthy]
)

continue_sequence <- function(chr_vector, sep = "_", factor = 1) {
  end <- sub(paste0(".*", sep, "([[:alnum:]]+)"), "\\1", grep(paste0(sep, "([[:alnum:]]*)$"),
    chr_vector,
    value = TRUE
  ))
  next_one <- 1
  numeric_end <- as.numeric(end)

  if (isTruthy(numeric_end)) {
    next_one <- max(numeric_end, na.rm = TRUE) + 1 * factor
  } else if (is.character(end)) {
    pos <- grep(end, letters)
    if (length(pos)) {
      next_one <- letters[pos + 1 * factor]
    } else {
      pos <- grep(end, LETTERS)
      next_one <- LETTERS[pos + 1 * factor]
    }
  }

  beg <- sub(paste0("(.*)", sep, "[[:alnum:]]+"), "\\1", grep(paste0(sep, "([[:alnum:]]*)$"),
    chr_vector,
    value = TRUE
  ))

  last_beg <- beg[length(beg)]

  chr_vector <- c(chr_vector, paste0(last_beg, sep, next_one))
}

duplicate_last_list_element <- function(list) {
  list_names <- names(list)
  list_with_appended_name <- continue_sequence(list_names)
  new_name <- list_with_appended_name[length(list_with_appended_name)]
  last_element <- list[length(list)]
  names(last_element) <- new_name
  last_element
}
