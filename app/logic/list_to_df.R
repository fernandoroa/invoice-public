box::use(
  magrittr[`%>%`],
  dplyr[rename, mutate, relocate, select, bind_rows, filter],
  tibble[as_tibble],
  utils[write.csv],
  tidyselect[all_of],
  purrr[map, discard, keep, list_flatten, list_rbind],
  stats[na.omit]
)

remove_from_list <- function(list, name) {
  lapply(list, function(sublist) {
    sublist[names(sublist) != name]
  })
}

prepare_row <- function(tibble, new_col_names_row, var_name, col_before, column_set) {
  if (all(new_col_names_row %in% colnames(tibble))) {
    tibble |>
      rename(all_of(new_col_names_row)) |>
      mutate(var = var_name) |>
      relocate(var, .before = col_before) |>
      select(all_of(column_set))
  } else {
    data.frame()
  }
}

#' @export
get_consultant_name <- function(input, plain_list, useNS = FALSE, namespace = "") {
  list <- list()
  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  list <- lapply(names(plain_list), function(x) {
    input[[paste0(namespace, x)]]
  })

  names(list) <- names(plain_list)
  list <- list %>% keep(names(.) %in% "name")
  new_col_names <- c(text = "name")
  list |>
    list_flatten() |>
    as_tibble() |>
    rename(all_of(new_col_names)) |>
    mutate(var = "consultant") |>
    relocate(var, .before = text)
}

#' @export
get_invoice_details <- function(input, plain_list, useNS = FALSE, namespace = "") {
  list <- list()
  if (useNS) {
    namespace <- paste0(namespace, "-")
  }

  list <- lapply(names(plain_list), function(x) {
    input[[paste0(namespace, x)]]
  })

  names(list) <- names(plain_list)

  list <- list %>% discard(names(.) %in% "file_identifier")
  new_col_names_row_1 <- c(text = "invoice_number", date = "invoiceDate", currency = "final_currency")
  new_col_names_row_2 <- c(date = "exchangeDate")

  tibble <- list |>
    list_flatten() |>
    as_tibble()

  tibble_row_1 <- tibble |>
    rename(all_of(new_col_names_row_1)) |>
    mutate(var = "invoice") |>
    relocate(var, .before = text) |>
    select(!exchangeDate)

  tibble_row_2 <- tibble |>
    rename(all_of(new_col_names_row_2)) |>
    mutate(var = "exchange") |>
    relocate(var, .before = date) |>
    select(c(var, date))

  bind_rows(tibble_row_1, tibble_row_2)
}

#' @export
get_nested_lists_details_salary <- function(
    input, nested_list, useNS = FALSE, namespace = "",
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

  use_override <- list$main$use_salary_and_related

  if (!use_override) {
    list$modified_days <- NULL
    list$non_working_days <- NULL
  }

  list <- list |>
    map(~.) |>
    keep(~ is.list(.x) && length(.x))

  list <- list |>
    map(~.) |>
    keep(~ .$use)

  list <- remove_from_list(list, "use")
  list <- remove_from_list(list, "use_salary_and_related")
  list <- remove_from_list(list, "section_title")
  list <- remove_from_list(list, "common_days_text")

  new_col_names_row_1 <- c(
    days = "main_total_days", amount = "main_salary",
    exchange_factor = "main_currency_exchange_to_Final_Currency", currency = "main_currency"
  )
  new_col_names_row_2 <- c(text = "single_text", date = "single_date", month_year_only = "single_show_month/year_only")

  new_col_names_row_3 <- c(date = "dates_start", date_end = "dates_end")

  new_col_names_row_4 <- c(percentage = "modified_days_percentage", text = "modified_days_text", days = "modified_days_days")

  new_col_names_row_5 <- c(days = "non_working_days_days")

  tibble <- list |>
    list_flatten() |>
    as_tibble()

  tibble_row_1 <- prepare_row(tibble, new_col_names_row_1, "basic_salary", "days",
    column_set = c("var", "days", "amount", "exchange_factor", "currency")
  )

  tibble_row_2 <- prepare_row(tibble, new_col_names_row_2, "single_date", "text",
    column_set = c("var", "text", "date", "month_year_only")
  )

  tibble_row_3 <- prepare_row(tibble, new_col_names_row_3, "date_range", "date",
    column_set = c("var", "date", "date_end")
  )

  tibble_row_4 <- prepare_row(tibble, new_col_names_row_4, "modified_days", "text",
    column_set = c("var", "text", "days", "percentage")
  )

  tibble_row_5 <- prepare_row(tibble, new_col_names_row_5, "non_working_days", "days",
    column_set = c("var", "days")
  )

  bind_rows(tibble_row_1, tibble_row_2, tibble_row_3, tibble_row_4, tibble_row_5)
}

#' @export
get_nested_lists_details <- function(
    input, nested_list, useNS = FALSE, namespace = "",
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
  list <- list |>
    map(~.) |>
    keep(~ is.list(.x) && length(.x))

  list <- list |>
    map(~.) |>
    keep(~ .$use)

  list <- remove_from_list(list, "use")
  list <- remove_from_list(list, "use_salary_and_related")
  list <- remove_from_list(list, "section_title")
  list <- remove_from_list(list, "common_days_text")

  tibble <- list |>
    list_flatten() |>
    as_tibble()

  new_col_names_row_1 <- c(
    exchange_factor = "currency_exchange_to_Final_Currency",
    amount = "value"
  )

  tibble_list <- list()
  for (i in seq_along(list)) {
    sublist <- list[[i]]
    sublist_name <- list[i] |> names()
    sublist_tibble <- sublist |>
      list_flatten() |>
      as_tibble() |>
      rename(all_of(new_col_names_row_1)) |>
      mutate(var = paste0("oneliner_", sublist_name)) |>
      relocate(var, .before = exchange_factor)
    tibble_list <- c(tibble_list, list(sublist_tibble))
  }
  list_rbind(tibble_list)
}

get_nested_and_root_details <- function(
    input, nested_list, useNS = FALSE,
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

  if (!list$use) {
    return(data.frame())
  }
  list <- list %>% discard(names(.) %in% c("file_identifier", "use"))

  root_list <- list |>
    map(~.) |>
    discard(~ is.list(.x))

  tibble <- root_list |>
    list_flatten() |>
    as_tibble()

  new_col_names_row_2 <- c(exchange_factor = "currency_exchange_to_Final_Currency", text = "GeneralName")

  general_row <- prepare_row(tibble, new_col_names_row_2, "grouped_costs", "text",
    column_set = c("var", "text", "currency", "exchange_factor")
  )

  costs_list <- list |>
    map(~.) |>
    keep(~ is.list(.x) && length(.x))

  new_col_names_row_1 <- c(text = "name", amount = "value")

  tibble_list <- list()
  for (i in seq_along(costs_list)) {
    sublist <- costs_list[[i]]
    sublist_name <- costs_list[i] |> names()
    sublist_tibble <- sublist |>
      list_flatten() |>
      as_tibble() |>
      rename(all_of(new_col_names_row_1)) |>
      mutate(var = paste0("grouped_", sublist_name)) |>
      relocate(var, .before = text)
    tibble_list <- c(tibble_list, list(sublist_tibble))
  }
  costs_tibble <- list_rbind(tibble_list)
  bind_rows(costs_tibble, general_row)
}

#' @export
save_data.frame <- function(inputs, rv_json_lists, oneliner_to_remove, grouped_to_remove, folder) {
  consultant_name <- get_consultant_name(
    inputs,
    plain_list = rv_json_lists$consultant_business_list,
    useNS = TRUE,
    namespace = "consultant_business_ns"
  )
  invoice_details <- get_invoice_details(
    inputs,
    plain_list = rv_json_lists$final_currency_list,
    useNS = TRUE,
    namespace = "currency_date_ns"
  )
  salary_details <- get_nested_lists_details_salary(
    inputs,
    nested_list = rv_json_lists$salary_list,
    useNS = TRUE,
    namespace = "salary_ns"
  )

  to_remove <- c()
  for (e in oneliner_to_remove) {
    to_remove <- c(to_remove, e())
  }

  oneliner_details <- get_nested_lists_details(
    inputs,
    nested_list = rv_json_lists$oneliners_list,
    useNS = TRUE,
    namespace = "oneliner_ns",
    to_remove = to_remove
  )
  to_remove_g <- c()
  for (e in grouped_to_remove) {
    to_remove_g <- c(to_remove_g, e())
  }

  grouped_details <- get_nested_and_root_details(
    inputs,
    nested_list = rv_json_lists$grouped_list,
    useNS = TRUE,
    namespace = "grouped_ns",
    to_remove = to_remove_g
  )

  df_list <- list(consultant_name, invoice_details, salary_details, oneliner_details, grouped_details)

  costs_df <- bind_rows(lapply(df_list, function(x) if (NROW(x) == 0) NULL else x)) |>
    select(any_of(c("var", "text", "days", "percentage", "date", "date_end", "amount", "exchange_factor", "currency")))

  if (length(unique(na.omit(costs_df$currency))) == 1) {
    costs_df <- costs_df |>
      filter(var != "exchange") |>
      select(-exchange_factor)
  }
  file_name <- "costs.csv"
  file_path <- file.path(folder, file_name)
  write.csv(costs_df, file_path, row.names = FALSE, na = "")
}
