create_files <- function() {
  temp_folder_session <- paste0(gsub("file", "folder_", tempfile()))
  dir.create(file.path(temp_folder_session, "json"), recursive = TRUE)

  file.copy("app/invoice.Rmd", file.path(temp_folder_session, "invoice.Rmd"))
  json_files <- file.path("app", "json", list.files("app/json"))
  lapply(json_files, function(x) {
    basename <- sub("^app/", "", x)
    file.copy(x, file.path(temp_folder_session, basename), overwrite = TRUE)
  })
  return(temp_folder_session)
}
