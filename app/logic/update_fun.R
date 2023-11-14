box::use(
  shiny[updateCheckboxInput, updateTextInput, updateNumericInput]
)

update_checkbox_list <- function(
    session, logic_fields, field_list, useChildNS = FALSE,
    child_namespace) {
  if (useChildNS) {
    list <- lapply(logic_fields, function(x) {
      updateCheckboxInput(
        session,
        paste0(child_namespace, "-", x),
        value = field_list[[x]]
      )
    })
    return(list)
  }
  lapply(logic_fields, function(x) {
    updateCheckboxInput(
      session,
      x,
      value = field_list[[x]]
    )
  })
}

update_text_input_list <- function(
    session, char_fields, field_list,
    useChildNS = FALSE, child_namespace) {
  if (useChildNS) {
    list <- lapply(char_fields, function(x) {
      updateTextInput(
        session,
        paste0(child_namespace, "-", x),
        value = field_list[[x]]
      )
    })
    return(list)
  }

  lapply(char_fields, function(x) {
    updateTextInput(
      session,
      x,
      value = field_list[[x]]
    )
  })
}

update_text_input_list_by_idx <- function(session, filtered_sublist) {
  lapply(seq_along(filtered_sublist), function(x) {
    updateTextInput(
      session,
      names(filtered_sublist[x]),
      value = filtered_sublist[[x]]
    )
  })
}

update_numeric_input_list <- function(
    session,
    num_list_names, field_list, useChildNS = FALSE,
    child_namespace = "") {
  if (useChildNS) {
    list <- lapply(num_list_names, function(x) {
      updateNumericInput(
        session,
        paste0(child_namespace, "-", x),
        value = field_list[[x]]
      )
    })
    return(list)
  }
  lapply(num_list_names, function(x) {
    updateNumericInput(
      session,
      x,
      value = field_list[[x]]
    )
  })
}
