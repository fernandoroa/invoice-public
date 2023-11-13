box::use(
  jsonlite[validate, read_json, toJSON]
)

box::use(
  .. / utils / constants[...]
)

validate_json_files <- function(folder) {
  states <- c()
  for (file in list.files(folder, full.names = TRUE)) {
    R_json <- try(read_json(file), silent = TRUE)
    if (inherits(R_json, "try-error")) {
      states <- c(states, FALSE)
    } else {
      json <- R_json |> toJSON(pretty = TRUE)
      states <- c(states, validate(json))
    }
  }
  all_true <- all(states)

  return(all_true)
}

validate_json_file <- function(file) {
  R_json <- try(read_json(file), silent = TRUE)
  if (inherits(R_json, "try-error")) {
    return(FALSE)
  } else {
    json <- R_json |> toJSON(pretty = TRUE)
    return(validate(json))
  }
}

check_rmd <- function(file_path_or_file_content) {
  if (file.exists(file_path_or_file_content)) {
    lines <- try(readLines(file_path_or_file_content), silent = TRUE)
  } else {
    lines <- strsplit(file_path_or_file_content, "\n") |> unlist()
  }
  if (inherits(lines, "try-error")) {
    return(FALSE)
  } else {
    state <- !any(grepl(paste(delete_pattern, command_pattern, sep = "|"), lines, perl = TRUE))
  }
  if (state) {
    return(grepl("---", lines[1]))
  }
  state
}
