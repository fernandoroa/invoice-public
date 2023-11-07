# invoice creation with Shiny and RMarkdown

This repository modifies https://github.com/shubham-maurya/shiny-invoice-generator/
by Fernando Roa

It has a lot of modifications, and now it is for single person use.

### Files

1. The `app/main.R` module file is the Shiny interface to the invoice generator, which has inputs (boxes) corresponding to
   `.json` information. Each panel/box and its logic are handled in submodules.

2. The `invoice.Rmd` file is the RMarkdown file, which you can modify to suit your own particular invoice. This file also
   processes data.

3. each `.json` file can be updated in the app. It is mandatory
   to save them after modifications done in the app, using buttons, before creating the `.pdf`

### Inner logic

- A couple of inputs are passed to the `.Rmd` via `params` in `yaml` header.
- Other inputs/boxes are held in `.json` files
- The `.Rmd` reads the `.json` files directly.
- If you modify the `.json` outside the app, during app use, there is a reload button to use.
- Includes oneliners and grouped costs
- Includes bilingual option
- Names of 'field' names in `.Rmd` depend on the `fieldNames.json` for bilingual management

### Exchange rates

- Salary values, one-liner and grouped costs can be in a different currency.
- It is possible to get a exchange rate for a defined date, for those.
- Package `quantmod` and function `getSymbol` is used for the exchange.

### `use` inputs

- The table consists of 3 big blocks:

  - Salary
  - Oneliners
  - Grouped costs

- Not only those parts can be selectively used with checkBoxes but also
  parts of them

### Link

https://ferapps.shinyapps.io/open-invoice/
