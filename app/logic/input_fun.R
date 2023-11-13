box::use(
  shiny[textInput, numericInput, checkboxInput, actionButton, div]
)

box::use(
  .. / utils / constants[...]
)

create_text_input <- function(char_names_currency_this_name, oneliners_list_active, ns, idx) {
  lapply(char_names_currency_this_name, function(x) {
    textInput(
      ns(x),
      if (idx == 1) {
        div(
          class = "wrap",
          sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
        )
      } else {
        ""
      },
      oneliners_list_active[[x]]
    )
  })
}

create_numeric_input <- function(num_names_currency_this_name, oneliners_list_active, ns, idx) {
  lapply(num_names_currency_this_name, function(x) {
    numericInput(
      ns(x),
      if (idx == 1) {
        div(
          class = "wrap",
          gsub("_", " ", x, perl = TRUE)
        )
      } else {
        ""
      },
      oneliners_list_active[[x]]
    )
  })
}

create_numeric_input_nc <- function(num_names_not_currency_this_name, oneliners_list_active, ns, idx) {
  lapply(num_names_not_currency_this_name, function(x) {
    numericInput(
      ns(x),
      if (idx == 1) {
        gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE))
      } else {
        ""
      },
      oneliners_list_active[[x]]
    )
  })
}

create_text_input_nc <- function(char_names_not_currency_this_name, oneliners_list_active, ns, idx) {
  lapply(char_names_not_currency_this_name, function(x) {
    textInput(
      ns(x),
      if (idx == 1) {
        gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x))
      } else {
        ""
      },
      oneliners_list_active[[x]]
    )
  })
}

create_check_box_input <- function(logic_names_oneliners_this_name, oneliners_list_active, ns) {
  lapply(logic_names_oneliners_this_name, function(x) {
    checkboxInput(
      ns(x),
      gsub("_", " ", gsub(pattern_a, pattern_b, x)),
      oneliners_list_active[[x]]
    )
  })
}

create_drop_button <- function(ns) {
  div(
    class = "go-bottom",
    actionButton(ns("remove_oneliner"), "Drop")
  )
}
