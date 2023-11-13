box::use(
  shiny[updateCheckboxInput]
)

update_checkbox_list <- function(session, logic_fields, field_list) {
  lapply(logic_fields, function(x) {
    updateCheckboxInput(
      session,
      x,
      value = field_list[[x]]
    )
  })
}

update_text_input_list <- function(session, char_fields, field_list) {
  lapply(char_fields, function(x) {
    updateTextInput(
      session,
      x,
      value = field_list[[x]]
    )
  })
}
