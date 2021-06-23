library(shiny)

ui <- fluidPage("Household Spending",
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
                            ),
                # Household characteristic
                plotOutput(outputId="pie_type"),
                plotOutput(outputId="hist"),
                plotOutput(outputId="pie_phone")
                # Household income # on map of Canada
                # Household expenditure # on map of Canada
                # tableOutput(outputId=income)
                )

server <- function(input, output) {
  
  # # Data: Enquête sur les dépenses des ménages, at 
  # https://www150.statcan.gc.ca/n1/pub/62m0004x/62m0004x2017001-fra.htm
  
  
  #setwd("C:/Users/djimd/Downloads/SHS_EDM_2017-fra/")
  diary <- read.csv(file="C:/Users/djimd/Downloads/SHS_EDM_2017-fra/SHS_EDM_2017/Data - Données/CSV/SHS-62M004X-E-2017-Diary_F1.csv",
                    header=TRUE)
  output$pie_type <- renderPlot({
    pie(table(factor(diary[diary$Prov==as.integer(substr(input$prov,1,2)),'HHType6'],
                     levels = 1:6,
                     labels=c("One person household",
                              "Couple without children",
                              "Couple with children",
                              "Couple with other related or unrelated persons",
                              "Lone parent family with no additional persons",
                              "Other household with related or unrelated persons")))
    )
    title("Household type")
  })
  output$hist <- renderPlot({
    hist(diary[diary$Prov==as.integer(substr(input$prov,1,2)),'HHSize'],breaks=c(0.5,1.5,2.5,3.5,4.5),main="Household size",xlab=NULL)
  })
  output$pie_phone <- renderPlot({
    pie(table(factor(diary[diary$Prov==as.integer(substr(input$prov,1,2)),'LandlineYN'],
                     levels = 1:2,
                     labels=c("Yes","No"))))
    title("Landline telephone service")
  })
}

shinyApp(ui = ui, server = server)

#hist(diary[diary$Prov==14,'NumBedr'],breaks=c(0.5,1.5,2.5,3.5,4.5),main="Household size",xlab=NULL)
#
#title("Household size")

IncomeTable = data.frame(
  matrix(c(min(diary[diary$Prov==14,'HH_TotInc']),median(diary[diary$Prov==14,'HH_TotInc']),max(diary[diary$Prov==14,'HH_TotInc']),
           min(diary[diary$Prov==14,'HH_EarnInc']),median(diary[diary$Prov==14,'HH_EarnInc']),max(diary[diary$Prov==14,'HH_EarnInc']),
           min(diary[diary$Prov==14,'HH_InvInc']),median(diary[diary$Prov==14,'HH_InvInc']),max(diary[diary$Prov==14,'HH_InvInc']),
           min(diary[diary$Prov==14,'HH_GovInc']),median(diary[diary$Prov==14,'HH_GovInc']),max(diary[diary$Prov==14,'HH_GovInc']),
           min(diary[diary$Prov==14,'HH_OthInc']),median(diary[diary$Prov==14,'HH_OthInc']),max(diary[diary$Prov==14,'HH_OthInc'])),
         byrow = TRUE,ncol=3)
)
colnames(IncomeTable) <- c("Min", "Median", "Max")
rownames(IncomeTable) <- c("Total income", "Earnings", "Investment", "Gov. transfer", "Other")

median(diary[diary$Prov==14,'HH_TotInc'])#, min(diary[diary$Prov==14,'HH_TotInc']), max(diary[diary$Prov==14,'HH_TotInc']) # household total income
median(diary[diary$Prov==14,'HH_EarnInc']) # Earnings
median(diary[diary$Prov==14,'HH_InvInc']) # Investment income
median(diary[diary$Prov==14,'HH_GovInc']) # Government transfer payments
median(diary[diary$Prov==14,'HH_OthInc']) # Other income

hist(diary[diary$Prov==14,'HH_TotInc'])
