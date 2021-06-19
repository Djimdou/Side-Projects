library(shiny)

ui <- fluidPage("Household Spending in 2017",
                selectInput(inputId="prov",
                            label="Select a province",
                            choices=c("14 - Atlantic provinces",
                                      "24 - Quebec",
                                      "35 - Ontario",
                                      "46 - Manitoba",
                                      "47 - Saskatchewan",
                                      "48 - Alberta",
                                      "59 - British Columbia",
                                      "63 - Territorial capitals")
                              #sort(unique(diary$Prov))
                            ),
                plotOutput(outputId="pie",
                           pie(diary[diary$Prov==14,'HHType6']))
                )

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

# # Data: Enquête sur les dépenses des ménages, at 
# https://www150.statcan.gc.ca/n1/pub/62m0004x/62m0004x2017001-fra.htm


#setwd("C:/Users/djimd/Downloads/SHS_EDM_2017-fra/")

diary <- read.csv(file="C:/Users/djimd/Downloads/SHS_EDM_2017-fra/SHS_EDM_2017/Data - Données/CSV/SHS-62M004X-E-2017-Diary_F1.csv",
                  header=TRUE)
  