box::use(
  quantmod[...],
  dplyr[...],
  purrr[...],
  stringr[...],
  rlang[set_names],
  tibble[rownames_to_column]
)
#' @export
get_exchange_rates <- function(from_curr, to_curr, from_date, to_date = from_date) {
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
        # Open and Close columns' values seem equal to Adjusted, removed!
        select(-ends_with(c("Open", "Close", "Volume"))) |>
        rownames_to_column(var = "date") |>
        set_names(~ (.) |> str_replace_all(".X", "")) |>
        rowwise() |>
        mutate("{ready_name}.avg_low_high_this_fun" := mean(c(
          .data[[paste0(ready_name, ".Low")]],
          .data[[paste0(ready_name, ".High")]]
        ), na.rm = TRUE)) |>
        set_names(~ (.) |> str_replace_all(paste0(ready_name, "."), "")) |>
        set_names(~ (.) |> paste0("_Sy")) |>
        mutate(date_input = as.character(as.Date(date_Sy) + 1))
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
try_exchange_rates <- function(date, final_currency, currency_to_convert, tries = 8) {
  date <- as.character(date)
  i <- 0
  while (i < tries) {
    exchange_df <- try(get_exchange_rates(
      toupper(final_currency),
      toupper(currency_to_convert), date
    ), silent = TRUE)
    date <- as.character(as.Date(date) - 1)
    i <- i + 1
    if (!inherits(exchange_df, "try-error")) break
  }
  return(exchange_df)
}

#' @export
try_exchange_rates_direct_and_indirect <- function(date, final_currency, currency_to_convert, tries = 8) {
  date <- date_orig <- as.character(date)
  adjusted_value <- "symbol or exchange not found"
  i <- 0
  while (i < tries) {
    exchange_df <- try(get_exchange_rates(
      toupper(final_currency),
      toupper(currency_to_convert), date
    ), silent = TRUE)
    date <- as.character(as.Date(date) - 1)
    i <- i + 1
    if (!inherits(exchange_df, "try-error")) break
  }

  if (inherits(exchange_df, "data.frame")) {
    adjusted_value <- exchange_df$Adjusted_Sy
  }

  i <- 0
  date <- date_orig
  if (inherits(exchange_df, "try-error") && final_currency != "USD" && currency_to_convert != "USD") {
    while (i < tries) {
      exchange_df_final_to_USD <- try(get_exchange_rates(
        toupper("USD"),
        toupper(final_currency), date
      ), silent = TRUE)
      date <- as.character(as.Date(date) - 1)
      i <- i + 1
      if (!inherits(exchange_df_final_to_USD, "try-error")) break
    }

    i <- 0
    date <- date_orig
    if (!inherits(exchange_df_final_to_USD, "try-error") && final_currency != currency_to_convert) {
      while (i < tries) {
        exchange_df_start_currency_to_USD <- try(get_exchange_rates(
          toupper("USD"),
          toupper(currency_to_convert), date
        ), silent = TRUE)
        date <- as.character(as.Date(date) - 1)
        i <- i + 1
        if (!inherits(exchange_df_start_currency_to_USD, "try-error")) break
      }
    }

    if (!inherits(exchange_df_final_to_USD, "try-error") && !inherits(exchange_df_start_currency_to_USD, "try-error")) {
      adjusted_value <- (1 / exchange_df_final_to_USD$Adjusted_Sy) * exchange_df_start_currency_to_USD$Adjusted_Sy
    }
  }

  return(adjusted_value)
}
