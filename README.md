# shiny-invoice-generator

This repository was copied from https://github.com/shubham-maurya/shiny-invoice-generator/
by Fernando Roa

It has several modifications, and now it is for single person use.

## Files

1. The `app.R` file is the Shiny interface to the invoice generator, which has inputs for the monthly changes, and to the right the stable information.

2. The `invoice.Rmd` file is the RMarkdown file, which you can modify to suit your own particular invoice. 

3. `input.json` and `input_salary.json` can be updated in the app. It is mandatory
to save them, using buttons, while in the app, before creating the `.pdf`

## Inner logic

The content of `.json` files will build a field for each element.  
The first box is manually built, and passes its values to the `.Rmd` via `params` in `yaml` header.

