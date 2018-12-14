# shiny-invoice-generator
A Shiny app which makes it super easy to render multiple (LaTeX formatted) invoices!

This was born out of the need to generate error-free  bi-monthly invoices for a large project team I was working with at EPIC India. The old way of modifying Doc files was slow, error-prone and extremely mundane. 

## Files

1. The *app* file is the Shiny interface to the invoice generator, which has buttons for choosing whose invoice to create, what dates and other such parameters. It also lets you see that you're choosing the right person, through the details view. 
2. The *invoice* file is the RMarkdown file, which you can modify to suit your own particular invoice. 
3. *Data* is a folder of how the details should be organized, to use it straight out of the box. 
4. *Scott_Invoice_2018* is a sample invoice, created for the one and only Michael Scott. 

## Requirements 

1. A working R environment (I used RStudio 1.1.463 and R 3.5) 
2. Packages: Shiny, dplyr, readr, rmarkdown (and related dependencies) 
3. LaTeX engine: MikTek for Windows, MacTek for Mac
