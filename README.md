# invoice creation with Shiny and RMarkdown

This repository was copied from https://github.com/shubham-maurya/shiny-invoice-generator/
by Fernando Roa

It has several modifications, and now it is for single person use.

### Files

1. The `app.R` file is the Shiny interface to the invoice generator, which has inputs (boxes) corresponding to
`.json` information.

2. The `invoice.Rmd` file is the RMarkdown file, which you can modify to suit your own particular invoice. 

3. each `.json` file can be updated in the app. It is mandatory
to save them after modifications done in the app, using buttons, before creating the `.pdf`

### Inner logic

- The first box is manually built, and passes its values to the `.Rmd` via `params` in `yaml` header. 
- The second box is manually built and is related to the `main.json`file 
- The other boxes are based on the other `.json` files, one on one. A form field for each json field is built in the app.  
- The `.Rmd` reads the `.json` files directly, that is why it is necessary to save them.
- If you modify the `.json` outside the app, during app use, reload the app.
- Includes benefits, sick options, FEDEX options
- Includes bilingual option
- Names of 'field' names in `.Rmd` depend on the `fieldNames.json` for bilingual management

# Link
https://ferapps.shinyapps.io/open-invoice/

