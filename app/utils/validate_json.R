box::use(
  jsonlite[validate, read_json, toJSON]
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
