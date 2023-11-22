box::use(
  quantmod[...],
  dplyr[...],
  purrr[...],
  stringr[...],
  rlang[set_names],
  tibble[rownames_to_column]
)
#' @export
get_exchange_rates_symbol <- function(from_curr, to_curr, from_date, to_date = from_date) {
  exchanges <- paste0(from_curr, "/", to_curr)

  result_getSymbols <- mapply(
    function(from_curr, to_curr) {
      ready_name <- paste0(from_curr, to_curr)
      getSymbols(paste0(ready_name, "=X"),
        src = "yahoo", auto.assign = FALSE,
        from = as.Date(from_date), to = as.Date(to_date)
      ) |>
        as.list() |>
        as.data.frame() |>
        rownames_to_column(var = "date") |>
        set_names(~ (.) |> str_replace_all(".X", "")) |>
        rowwise() |>
        mutate("{ready_name}.avg_low_high_this_fun" := mean(c(
          .data[[paste0(ready_name, ".Low")]],
          .data[[paste0(ready_name, ".High")]]
        ), na.rm = TRUE)) |>
        set_names(~ (.) |> str_replace_all(paste0(ready_name, "."), "")) |>
        set_names(~ (.) |> paste0("_Sy")) |>
        mutate(date_input = ifelse(
          as.Date(date_Sy) < as.Date("2023-10-30"),
          as.character(as.Date(date_Sy) + 1),
          date_Sy
        )) |>
        relocate(date_input, .before = date_Sy) |>
        relocate(avg_low_high_this_fun_Sy, .after = date_input)
    },
    from_curr, to_curr,
    SIMPLIFY = FALSE
  )
  names(result_getSymbols) <- exchanges
  result_getSymbols <- result_getSymbols |>
    bind_rows(.id = "exchange") |>
    as.data.frame()
  result_getSymbols
}

#' @export
try_exchange_rates_direct_and_indirect <- function(date, from_curr, to_curr, tries = 8) {
  date <- date_orig <- as.character(date)
  adjusted_value <- "symbol or exchange not found"
  ready_name <- paste0(from_curr, to_curr)
  i <- 0
  while (i < tries) {
    exchange_df <- try(get_exchange_rates_symbol(
      toupper(from_curr),
      toupper(to_curr), date
    ), silent = TRUE)
    date <- as.character(as.Date(date) - 1)
    i <- i + 1
    if (!inherits(exchange_df, "try-error")) break
  }

  if (inherits(exchange_df, "data.frame")) {
    adjusted_value <- exchange_df$Adjusted_Sy
    names(adjusted_value) <- ready_name
  }

  i <- 0
  date <- date_orig
  if (inherits(exchange_df, "try-error") && from_curr != "USD" && to_curr != "USD") {
    while (i < tries) {
      exchange_df_USD_to_from_curr <- try(get_exchange_rates_symbol(
        toupper("USD"),
        toupper(from_curr), date
      ), silent = TRUE)
      date <- as.character(as.Date(date) - 1)
      i <- i + 1
      if (!inherits(exchange_df_USD_to_from_curr, "try-error")) break
    }

    i <- 0
    date <- date_orig
    if (!inherits(exchange_df_USD_to_from_curr, "try-error") && from_curr != to_curr) {
      while (i < tries) {
        exchange_df_USD_to_to_curr <- try(get_exchange_rates_symbol(
          toupper("USD"),
          toupper(to_curr), date
        ), silent = TRUE)
        date <- as.character(as.Date(date) - 1)
        i <- i + 1
        if (!inherits(exchange_df_USD_to_to_curr, "try-error")) break
      }
    }

    if (!inherits(exchange_df_USD_to_from_curr, "try-error") && !inherits(exchange_df_USD_to_to_curr, "try-error")) {
      adjusted_value <- 1 / (exchange_df_USD_to_from_curr$Adjusted_Sy / exchange_df_USD_to_to_curr$Adjusted_Sy)
      names(adjusted_value) <- ready_name
    }
  }

  return(adjusted_value)
}
