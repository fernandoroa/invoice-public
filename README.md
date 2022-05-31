# shiny-invoice-generator

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

- The content of `.json` files will build a field for each element in the app.   
- The first box is manually built, and passes its values to the `.Rmd` via `params` in `yaml` header. 
- The `.Rmd` reads the `.json` files directly, that is why it is necessary to save them.
- Includes benefits, sick options, FEDEX options
- Names of 'fields' in `.Rmd` depend on the `fieldNames.json`

