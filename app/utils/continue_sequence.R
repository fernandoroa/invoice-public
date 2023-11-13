continue_sequence <- function(chr_vector, sep = "_") {
  end <- sub(paste0(".*_([0-9]+)"), "\\1", grep(paste0("_([0-9]*)$"),
    chr_vector,
    value = TRUE
  ))

  next_one <- max(as.numeric(end)) + 1
  next_one <- ifelse(is.na(next_one), 1, next_one)

  beg <- sub(paste0("(.*)_[0-9]+"), "\\1", grep(paste0("_([0-9]*)$"),
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
