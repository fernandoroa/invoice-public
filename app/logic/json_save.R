box::use(
  purrr[map, discard, keep],
  shiny[reactiveValuesToList]
)
#' @export
plain_json_save <- function(input, plain_list, folders, file_name, useNS = FALSE, namespace = "") {
  list <- list()
  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  list <- lapply(names(plain_list), function(x) {
    input[[paste0(namespace, x)]]
  })

  names(list) <- names(plain_list)
  list$file_identifier <- sub(".json$", "", file_name)
  json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}

#' @export
nested_json_save <- function(input, nested_list, prefix, folders, file_name) {
  list <- list()
  nested_list_names <- names(nested_list)
  for (name in nested_list_names) {
    list[[name]] <- lapply(names(nested_list[[name]]), function(x) {
      input[[paste0(prefix, name, x)]]
    })
    names(list[[name]]) <- names(nested_list[[name]])
  }
  list$file_identifier <- sub(".json$", "", file_name)
  json_data <- jsonlite::toJSON(x = list, pretty = TRUE)

  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}

#' @export
nested_and_root_save <- function(input, nested_list, prefix, folders, file_name) {
  list <- list()

  nested_list_root <- nested_list |>
    map(~ is.list(.)) |>
    discard(~.x)
  nested_list_sublists <- nested_list |>
    map(~ is.list(.)) |>
    keep(~.x)
  nested_list_names_root <- names(nested_list_root)
  nested_list_names_sublists <- names(nested_list_sublists)

  list <- lapply(nested_list_names_root, function(x) {
    input[[paste0(prefix, x)]]
  })
  names(list) <- nested_list_names_root

  for (sublist_name in nested_list_names_sublists) {
    list[[sublist_name]] <- lapply(names(nested_list[[sublist_name]]), function(x) {
      input[[paste0(prefix, sublist_name, x)]]
    })
    names(list[[sublist_name]]) <- names(nested_list[[sublist_name]])
  }
  list$file_identifier <- sub(".json$", "", file_name)
  json_data <- jsonlite::toJSON(x = list, pretty = TRUE)

  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}
