box::use(
  shiny[...],
  rmarkdown[render],
)

box::use(
  .. / logic / json_save[...],
)

ui <- function(id) {
  ns <- NS(id)
  div(
    class = "generate_buttons",
    br(),
    helpText("Save changes and generates .pdf"),
    div(
      downloadButton(ns("report"), "Render Document")
    )
  )
}

server <- function(id, rv_json_lists, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$report <- downloadHandler(
      filename = function() {
        "invoice.pdf"
      },
      content = function(file) {
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_consultant_business_list,
          folders = "app/json", file_name = "consultant_contact.json",
          useNS = TRUE,
          namespace = "consultant_business_ns"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_consultant_account_list,
          folders = "app/json", file_name = "consultant_account.json"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_business_to_bill_list,
          folders = "app/json", file_name = "business_to_bill.json",
          useNS = TRUE,
          namespace = "bill_to_ns"
        )
        plain_json_save(
          inputs,
          plain_list = rv_json_lists$json_final_currency_list,
          folders = "app/json",
          file_name = "final_currency_inv_date.json",
          useNS = TRUE,
          namespace = "currency_date_ns"
        )
        nested_json_save(
          inputs,
          nested_list = rv_json_lists$json_salary_list,
          prefix = "",
          folders = "app/json",
          file_name = "salary.json",
          useNS = TRUE,
          namespace = "salary_ns"
        )
        nested_json_save(inputs,
          nested_list = rv_json_lists$json_oneliners_list,
          prefix = "",
          folders = "app/json",
          file_name = "oneliner_costs.json",
          useNS = TRUE,
          namespace = "oneliner_ns"
        )
        nested_and_root_save(inputs,
          nested_list = rv_json_lists$json_grouped_list,
          prefix = "",
          folders = "app/json",
          file_name = "grouped_costs.json",
          useNS = TRUE,
          namespace = "grouped_ns"
        )

        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)
        temp_report <- file.path(folder, "inv_md_dont_modify.Rmd")
        file.copy("app/invoice.Rmd", temp_report, overwrite = TRUE)
        app_path <- file.path(getwd(), "app")
        all_params <- reactiveValuesToList(inputs)
        params <- list(invoiceNumber = all_params$invoiceNumber, lang = all_params$lang, app_path = app_path)

        render(temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      },
      contentType = "pdf"
    )
  })
}
