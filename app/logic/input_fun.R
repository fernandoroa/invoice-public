box::use(
  shiny[textInput, numericInput, checkboxInput, actionButton, div]
)

box::use(
  .. / utils / constants[...]
)

create_text_input <- function(char_field_names, field_list, ns, idx) {
  lapply(char_field_names, function(x) {
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
      field_list[[x]]
    )
  })
}

create_text_input_wrap <- function(char_field_names, field_list, ns, useChildNS = FALSE, child_namespace = "") {
  if (useChildNS) {
    list <- lapply(char_field_names, function(x) {
      textInput(
        ns(paste0(child_namespace, "-", x)),
        div(
          class = "wrap",
          sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
        ),
        field_list[[x]]
      )
    })
    return(list)
  }
  lapply(char_field_names, function(x) {
    textInput(
      ns(x),
      div(
        class = "wrap",
        sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
      ),
      field_list[[x]]
    )
  })
}

create_text_input_with_patterns_container <- function(filtered_sublist, useLabel, ns) {
  className <- ifelse(useLabel, "", "form-group-container")
  lapply(seq_along(filtered_sublist), function(x) {
    div(
      class = className,
      textInput(ns(names(filtered_sublist[x])),
        {
          if (useLabel) {
            gsub("_", " ", gsub(pattern_a, pattern_b, names(filtered_sublist[x])))
          } else {
            ""
          }
        },
        value = filtered_sublist[[x]]
      )
    )
  })
}

create_text_input_nc <- function(char_names_not_currency_this_name, field_list, ns, idx) {
  lapply(char_names_not_currency_this_name, function(x) {
    textInput(
      ns(x),
      if (idx == 1) {
        gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x))
      } else {
        ""
      },
      field_list[[x]]
    )
  })
}

create_text_input_nc_simple <- function(
    char_field_names, field_list, ns, useChildNS = FALSE,
    child_namespace = "") {
  if (useChildNS) {
    list <- lapply(char_field_names, function(x) {
      textInput(
        ns(paste0(child_namespace, "-", x)),
        gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
        field_list[[x]]
      )
    })
    return(list)
  }

  lapply(char_field_names, function(x) {
    textInput(
      ns(x),
      gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
      field_list[[x]]
    )
  })
}

create_text_input_with_patterns <- function(char_fields, field_list, ns) {
  lapply(char_fields, function(x) {
    textInput(ns(x),
      gsub("_", " ", gsub(pattern_a, pattern_b, x)),
      value = field_list[[x]]
    )
  })
}

create_text_input_with_child_patterns <- function(char_fields, field_list, ns, child_namespace = "") {
  lapply(char_fields, function(x) {
    textInput(
      ns(paste0(child_namespace, "-", x)),
      gsub("_", " ", gsub(pattern_a, pattern_b, x)),
      field_list[[x]]
    )
  })
}

create_numeric_input <- function(
    num_names_currency_this_name, field_list, ns,
    idx, useChildNS = FALSE, child_namespace = "") {
  if (useChildNS) {
    list <- lapply(num_names_currency_this_name, function(x) {
      numericInput(
        ns(paste0(child_namespace, "-", x)),
        "",
        field_list[[x]]
      )
    })
    return(list)
  }

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
      field_list[[x]]
    )
  })
}

create_numeric_input_wrap <- function(
    num_field_names, field_list, ns, useChildNS = FALSE,
    child_namespace = "") {
  if (useChildNS) {
    list <- lapply(num_field_names, function(x) {
      numericInput(
        ns(paste0(child_namespace, "-", x)),
        div(
          class = "wrap",
          gsub("_", " ", x, perl = TRUE)
        ),
        field_list[[x]]
      )
    })
    return(list)
  }
  lapply(num_field_names, function(x) {
    numericInput(
      ns(x),
      div(
        class = "wrap",
        gsub("_", " ", x, perl = TRUE)
      ),
      field_list[[x]]
    )
  })
}

create_numeric_input_nc <- function(
    num_names_not_currency_this_name, field_list, ns,
    idx, useChildNS = FALSE, child_namespace = "") {
  if (useChildNS) {
    list <- lapply(num_names_not_currency_this_name, function(x) {
      numericInput(
        ns(paste0(child_namespace, "-", x)),
        gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
        field_list[[x]]
      )
    })
    return(list)
  }
  lapply(num_names_not_currency_this_name, function(x) {
    numericInput(
      ns(x),
      if (idx == 1) {
        gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE))
      } else {
        ""
      },
      field_list[[x]]
    )
  })
}

create_numeric_input_nc_child_bottom <- function(
    num_names_not_currency_this_name, field_list, ns,
    child_namespace = "") {
  lapply(num_names_not_currency_this_name, function(x) {
    div(
      class = "go-bottom",
      numericInput(
        ns(paste0(child_namespace, "-", x)),
        div(
          class = "wrap",
          gsub("_", " ", x, perl = TRUE)
        ),
        field_list[[x]]
      )
    )
  })
}

create_numeric_input_child_pattern <- function(
    num_names_not_currency_this_name, field_list, ns,
    child_namespace = "") {
  lapply(num_names_not_currency_this_name, function(x) {
    numericInput(
      ns(paste0(child_namespace, "-", x)),
      gsub("_", " ", gsub(pattern_a, pattern_b, x)),
      field_list[[x]]
    )
  })
}


create_check_box_input <- function(logic_fields, field_list, ns, useChildNS = FALSE, child_namespace = "") {
  if (useChildNS) {
    list <- lapply(logic_fields, function(x) {
      checkboxInput(
        ns(paste0(child_namespace, "-", x)),
        gsub("_", " ", gsub(pattern_a, pattern_b, x)),
        field_list[[x]]
      )
    })
    return(list)
  }
  lapply(logic_fields, function(x) {
    checkboxInput(
      ns(x),
      gsub("(^[[:alpha:]])", "\\U\\1", gsub("_", " ", gsub(pattern_a, pattern_b, x)), perl = TRUE),
      field_list[[x]]
    )
  })
}

create_check_box_input_child <- function(
    logic_fields, field_list, ns,
    child_namespace = "") {
  lapply(logic_fields, function(x) {
    checkboxInput(
      ns(paste0(child_namespace, "-", x)),
      div(
        class = "wrap",
        gsub("_", " ", gsub(pattern_a, pattern_b, x))
      ),
      field_list[[x]]
    )
  })
}

create_drop_button <- function(ns) {
  div(
    class = "go-bottom",
    actionButton(ns("remove_row"), "Drop")
  )
}
