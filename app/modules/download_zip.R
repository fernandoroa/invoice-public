box::use(
  shiny[...],
  utils[zip],
)

box::use(
  .. / logic / json_save[...],
)

ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    div(
      class = "generate_buttons",
      h4("Download Source files as .zip"),
      helpText("with current changes"),
      span(code(".zip"), "contains", code(".json"), "files"),
      downloadButton(ns("downloadPresets"), "Download .zip", class = "btn-success")
    )
  )
}

server <- function(id, rv_json_lists, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$downloadPresets <- downloadHandler(
      filename = function() {
        "json.zip"
      },
      content = function(file) {
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_consultant_business_list,
          folders = c(folder, "app/json"), file_name = "consultant_contact.json",
          useNS = TRUE,
          namespace = "consultant_business_ns"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_consultant_account_list,
          folders = c(folder, "app/json"), file_name = "consultant_account.json"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_business_to_bill_list,
          folders = c(folder, "app/json"), file_name = "business_to_bill.json",
          useNS = TRUE,
          namespace = "bill_to_ns"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_final_currency_list,
          folders = c(folder, "app/json"),
          file_name = "final_currency_inv_date.json",
          useNS = TRUE,
          namespace = "currency_date_ns"
        )
        nested_json_save(
          inputs,
          nested_list = rv_json_lists$json_salary_list,
          prefix = "",
          folders = c(folder, "app/json"),
          file_name = "salary.json",
          useNS = TRUE,
          namespace = "salary_ns"
        )
        nested_json_save(
          inputs,
          nested_list = rv_json_lists$json_oneliners_list,
          prefix = "",
          folders = c(folder, "app/json"),
          file_name = "oneliner_costs.json",
          useNS = TRUE,
          namespace = "oneliner_ns"
        )
        nested_and_root_save(
          inputs,
          nested_list = rv_json_lists$json_grouped_list,
          prefix = "grouped",
          folders = c(folder, "app/json"),
          file_name = "grouped_costs.json"
        )


        file.copy("app/json/fieldNames.json", folder)

        zip_path <- file.path(folder, "json.zip")
        files_to_zip <- dir(folder, full.names = TRUE)
        zip(zipfile = zip_path, files = files_to_zip, flags = "-0jrm")
        file.copy(zip_path, file)
      },
      contentType = "zip"
    )
  })
}
