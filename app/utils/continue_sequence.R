box::use(
  shiny[isTruthy],
  lubridate[month, day, year]
)

box::use(
  .. / utils / constants[...],
)

continue_sequence <- function(chr_vector, sep = "_", factor = 1) {
  last_from_vector <- chr_vector[length(chr_vector)]

  if (is.numeric(suppressWarnings(as.numeric(last_from_vector))) &&
    isTruthy(suppressWarnings(as.numeric(last_from_vector))) && sep == "") {
    return(c(chr_vector, as.numeric(last_from_vector) + 1 * factor))
  }

  last_beg <- sub(paste0("(.*)", sep, "([[:alnum:]]+)?"), "\\1", grep(paste0(sep, "([[:alnum:]]*)$"),
    last_from_vector,
    value = TRUE
  ))

  last_end <- sub(paste0(".*", sep, "([[:alnum:]]+)"), "\\1", grep(paste0(sep, "([[:alnum:]]*)$"),
    last_from_vector,
    value = TRUE
  ))

  next_one <- 1
  numeric_end <- as.numeric(last_end)

  if (isTruthy(numeric_end)) {
    next_one <- (numeric_end + 1 * factor) |> abs()
  } else if (is.character(last_end) && length(last_end)) {
    split <- strsplit(last_end, "") |> unlist()
    last_char <- split[length(split)]
    last_char_beg <- sub(paste0(last_char, "$"), "", last_end)
    pos <- grep(last_char, letters)
    pos_A <- grep(last_char, LETTERS)
    if (length(pos)) {
      if (factor > 0) {
        next_one <- ifelse(last_char == "z", "aa", letters[pos + 1 * factor])
      } else {
        next_one <- ifelse(last_char == "a", "z", letters[pos + 1 * factor])
      }
      next_one <- paste0(last_char_beg, next_one)
    } else if (length(pos_A)) {
      if (factor > 0) {
        next_one <- ifelse(last_char == "Z", "AA", LETTERS[pos_A + 1 * factor])
      } else {
        next_one <- ifelse(last_char == "A", "Z", LETTERS[pos_A + 1 * factor])
      }
      next_one <- paste0(last_char_beg, next_one)
    } else {
      last_char <- sub(".*?([[:digit:]]+)$", "\\1", last_end)
      last_char_beg <- sub(paste0(last_char, "$"), "", last_end)
      next_one <- (as.numeric(last_char) + 1 * factor) |> abs()
      if (is.numeric(as.numeric(next_one)) && isTruthy(as.numeric(next_one))) {
        next_one <- paste0(last_char_beg, next_one)
      } else {
        next_one <- 1
      }
    }
  } else {
    warning("sep was not found in strings")
  }

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



date_bump_month <- function(date, decrease = FALSE) {
  year_int <- year(date)
  if (year_int %in% leap_years) {
    mon_span[3] <- 29
  }
  mon <- month(date)
  day_int <- day(date)

  modifier <- ifelse(day_int == mon_span[mon + 1], 1, 0)

  if (!decrease) {
    return(date + mon_span[mon + 1 + modifier])
  } else {
    subtract <- ifelse(day_int > mon_span[mon], day_int, mon_span[mon + modifier])
    date - subtract
  }
}

get_last_symbol <- function(string) {
  ifelse(grepl("[^[:alnum:]]", string), sub(".*([^[:alnum:]]).*", "\\1", string), "")
}

substitute_invalid_days <- function(year_int, day_int, month_int, mon_span) {
  last_day_of_month <- get_last_day_of_month(year_int, month_int, mon_span)
  end_of_month_exceeded <- ifelse(day_int > last_day_of_month, TRUE, FALSE)
  if (year_int %in% leap_years) {
    mon_span[3] <- 29
  }
  if (end_of_month_exceeded) {
    return(mon_span[month_int + 1])
  }
  day_int
}

get_last_day_of_month <- function(year_int, month_int, mon_span) {
  if (year_int %in% leap_years) {
    mon_span[3] <- 29
  }
  mon_span[month_int + 1]
}

is_last_day_of_month <- function(date, mon_span) {
  if (year(date) %in% leap_years) {
    mon_span[3] <- 29
  }
  if (day(date) == mon_span[month(date) + 1]) {
    return(TRUE)
  } else {
    FALSE
  }
}

get_new_date <- function(date_input, year_int, month_int, mon_span) {
  invoice_date_day <- day(date_input)
  invoice_date_day <- substitute_invalid_days(year_int, invoice_date_day, month_int, mon_span)

  invoice_is_last_day_of_month_bool <- is_last_day_of_month(date_input, mon_span)

  last_day_of_month <- get_last_day_of_month(year_int, month_int, mon_span)

  if (invoice_is_last_day_of_month_bool) {
    new_invoice_date <- paste0(c(year_int, month_int, last_day_of_month), collapse = "-") |> as.Date()
    return(new_invoice_date)
  } else {
    paste0(c(year_int, month_int, invoice_date_day), collapse = "-") |> as.Date()
  }
}

get_current_month_year <- function() {
  year_int <- current_year <- format(Sys.Date(), "%Y") |> as.numeric()
  month_int <- current_month <- format(Sys.Date(), "%m") |> as.numeric()
  current_day <- format(Sys.Date(), "%d") |> as.numeric()

  if (current_day <= 20) {
    month_int <- current_month - 1
  }
  if (current_month == 12 && current_day > 20) {
    year_int <- current_year + 1
    month_int <- 1
  }
  return(c(year_int, month_int))
}
