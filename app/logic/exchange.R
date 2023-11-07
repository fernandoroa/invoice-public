box::use(
  quantmod[...],
  dplyr[...],
  purrr[...],
  stringr[...],
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
        tibble::rownames_to_column(var = "date") |>
        purrr::set_names(~ (.) |> stringr::str_replace_all(".X", "")) |>
        rowwise() |>
        mutate("{ready_name}.avg_low_high_this_fun" := mean(c(
          .data[[paste0(ready_name, ".Low")]],
          .data[[paste0(ready_name, ".High")]]
        ), na.rm = TRUE)) |>
        purrr::set_names(~ (.) |> stringr::str_replace_all(paste0(ready_name, "."), "")) |>
        purrr::set_names(~ (.) |> paste0("_Sy")) |>
        mutate(date_input = as.character(as.Date(date_Sy) + 1))
    },
    from_curr, to_curr,
    SIMPLIFY = FALSE
  )
  names(result_getSymbols) <- exchanges
  result_getSymbols <- result_getSymbols |>
    dplyr::bind_rows(.id = "exchange") |>
    as.data.frame()
  result_getSymbols
}

#' @export
try_exchange_rates <- function(date, final_currency, currency_to_convert, tries = 8) {
  date <- as.character(date)
  i <- 0
  while (i < tries) {
    exchange_df <- try(get_exchange_rates(toupper(final_currency), toupper(currency_to_convert), date), silent = TRUE)
    date <- as.character(as.Date(date) - 1)
    i <- i + 1
    if (!inherits(exchange_df, "try-error")) break
  }
  return(exchange_df)
}
