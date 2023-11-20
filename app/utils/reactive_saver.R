box::use(
  rjson[rjson_fromJSON = fromJSON]
)
reactive_saver <- function(rv_temp_folder_session, rv_json_lists) {
  rv_json_lists$final_currency_list <- rjson_fromJSON(
    file = file.path(rv_temp_folder_session(), "json", "invoice_and_final_currency.json")
  )
  rv_json_lists$business_to_bill_list <- rjson_fromJSON(
    file = file.path(rv_temp_folder_session(), "json", "business_to_bill.json")
  )
  rv_json_lists$consultant_account_list <- rjson_fromJSON(
    file = file.path(rv_temp_folder_session(), "json", "consultant_account.json")
  )
  rv_json_lists$consultant_business_list <- rjson_fromJSON(
    file = file.path(rv_temp_folder_session(), "json", "consultant_business.json")
  )
  rv_json_lists$salary_list <- rjson_fromJSON(file = file.path(rv_temp_folder_session(), "json", "salary.json"))
  rv_json_lists$oneliners_list <- rjson_fromJSON(file = file.path(rv_temp_folder_session(), "json", "oneliner_costs.json"))
  rv_json_lists$grouped_list <- rjson_fromJSON(file = file.path(rv_temp_folder_session(), "json", "grouped_costs.json"))
  return(rv_json_lists)
}
