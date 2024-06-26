box::use(
  purrr[map, discard, keep],
  shiny[showNotification, tagList, p],
  magrittr[`%>%`],
  jsonlite[validate, toJSON],
  tools[file_ext]
)
box::use(
  .. / utils / validate[...]
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
  json_data <- toJSON(x = list, pretty = TRUE, digits = 15)
  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}

#' @export
ace_save <- function(input, input_name, folders, file_name, useNS = FALSE, namespace = "") {
  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  content <- input[[paste0(namespace, input_name)]]
  extension <- file_ext(file_name) |> tolower()

  if (extension == "json") {
    is_valid <- validate(content)
  } else if (extension == "rmd") {
    is_valid <- check_rmd(content)
    if (!is_valid) {
      showNotification("invalid content or forbidden strings found in .Rmd",
        type = "error",
        duration = 15
      )
    }
  }

  if (is_valid) {
    for (folder in folders) {
      file_path <- file.path(folder, file_name)
      write(content, file_path)
    }
  } else {
    showNotification(
      tagList(
        p(paste0("invalid ", file_name, ", changes not saved")),
        p("A previous version of the file was saved/used !!"),
        p("click on 'Discard unsaved' button (Main tab) or fix problems")
      ),
      type = "error",
      duration = 15
    )
  }
  return(is_valid)
}

#' @export
nested_json_save <- function(
    input, nested_list, folders, file_name, useNS = FALSE, namespace = "",
    to_remove = "") {
  list <- list()
  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  nested_list_names <- names(nested_list)
  for (name in nested_list_names) {
    list[[name]] <- lapply(names(nested_list[[name]]), function(x) {
      input[[paste0(namespace, name, "-", x)]]
    })
    names(list[[name]]) <- names(nested_list[[name]])
  }
  list <- list %>% discard(names(.) %in% to_remove)

  list$file_identifier <- sub(".json$", "", file_name)
  json_data <- toJSON(x = list, pretty = TRUE, digits = 15)

  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}

#' @export
nested_and_root_save <- function(
    input, nested_list, folders, file_name, useNS = FALSE,
    namespace = "",
    to_remove) {
  list <- list()

  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  nested_list_root <- nested_list |>
    map(~ is.list(.)) |>
    discard(~.x)

  nested_list_sublists <- nested_list |>
    map(~ is.list(.)) |>
    keep(~.x)

  nested_list_names_root <- names(nested_list_root)
  nested_list_names_sublists <- names(nested_list_sublists)

  list <- lapply(nested_list_names_root, function(x) {
    input[[paste0(namespace, x)]]
  })
  names(list) <- nested_list_names_root

  for (sublist_name in nested_list_names_sublists) {
    list[[sublist_name]] <- lapply(names(nested_list[[sublist_name]]), function(x) {
      input[[paste0(namespace, sublist_name, "-", x)]]
    })
    names(list[[sublist_name]]) <- names(nested_list[[sublist_name]])
  }
  list <- list %>% discard(names(.) %in% to_remove)

  list$file_identifier <- sub(".json$", "", file_name)
  json_data <- toJSON(x = list, pretty = TRUE, digits = 15)

  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    write(json_data, file_path)
  }
}

#' @export
save_all <- function(inputs, folders, rv_json_lists, oneliner_to_remove, grouped_to_remove) {
  plain_json_save(
    inputs,
    plain_list = rv_json_lists$consultant_business_list,
    folders = folders, file_name = "consultant_business.json",
    useNS = TRUE,
    namespace = "consultant_business_ns"
  )
  if (is.null(rv_json_lists$consultant_account_list$show)) {
    rv_json_lists$consultant_account_list$show <- TRUE
  }
  plain_json_save(
    inputs,
    plain_list = rv_json_lists$consultant_account_list,
    folders = folders, file_name = "consultant_account.json",
    useNS = TRUE,
    namespace = "account_ns"
  )
  plain_json_save(
    inputs,
    plain_list = rv_json_lists$business_to_bill_list,
    folders = folders, file_name = "business_to_bill.json",
    useNS = TRUE,
    namespace = "bill_to_ns"
  )
  plain_json_save(
    inputs,
    plain_list = rv_json_lists$final_currency_list,
    folders = folders,
    file_name = "invoice_and_final_currency.json",
    useNS = TRUE,
    namespace = "currency_date_ns"
  )
  nested_json_save(
    inputs,
    nested_list = rv_json_lists$salary_list,
    folders = folders,
    file_name = "salary.json",
    useNS = TRUE,
    namespace = "salary_ns"
  )

  to_remove <- c()
  for (e in oneliner_to_remove) {
    to_remove <- c(to_remove, e())
  }
  nested_json_save(
    inputs,
    nested_list = rv_json_lists$oneliners_list,
    folders = folders,
    file_name = "oneliner_costs.json",
    useNS = TRUE,
    namespace = "oneliner_ns",
    to_remove = to_remove
  )

  to_remove_g <- c()
  for (e in grouped_to_remove) {
    to_remove_g <- c(to_remove_g, e())
  }

  nested_and_root_save(
    inputs,
    nested_list = rv_json_lists$grouped_list,
    folders = folders,
    file_name = "grouped_costs.json",
    useNS = TRUE,
    namespace = "grouped_ns",
    to_remove = to_remove_g
  )
  ace_save(
    inputs, "ace", folders,
    file_name = "field_names.json", useNS = TRUE,
    namespace = "json_ace_ns"
  )
}
