library(shiny)
library(sf) # for st_read

# functions

func_income <- function(province)
{
  IncomeTable <- data.frame(
    matrix(c(min(diary[diary$Prov==province,'HH_TotInc']),median(diary[diary$Prov==province,'HH_TotInc']),max(diary[diary$Prov==province,'HH_TotInc']),
             min(diary[diary$Prov==province,'HH_EarnInc']),median(diary[diary$Prov==province,'HH_EarnInc']),max(diary[diary$Prov==province,'HH_EarnInc']),
             min(diary[diary$Prov==province,'HH_InvInc']),median(diary[diary$Prov==province,'HH_InvInc']),max(diary[diary$Prov==province,'HH_InvInc']),
             min(diary[diary$Prov==province,'HH_GovInc']),median(diary[diary$Prov==province,'HH_GovInc']),max(diary[diary$Prov==province,'HH_GovInc']),
             min(diary[diary$Prov==province,'HH_OthInc']),median(diary[diary$Prov==province,'HH_OthInc']),max(diary[diary$Prov==province,'HH_OthInc'])),
           byrow = TRUE,ncol=3)
  )
  colnames(IncomeTable) <- c("Minimum", "Median", "Maximum")
  rownames(IncomeTable) <- c("Total income", "Earnings", "Investment", "Gov. transfer", "Other")
  return(IncomeTable)
}

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
                plotOutput(outputId="pie_phone"),
                dataTableOutput('table')
                ,plotOutput(outputId="map")
                )

server <- function(input, output) {
  
  # # Data: Enquête sur les dépenses des ménages, at 
  # https://www150.statcan.gc.ca/n1/pub/62m0004x/62m0004x2017001-fra.htm
  
  
  #Load data set
  diary <- read.csv(file="C:/Users/djimd/Downloads/SHS_EDM_2017-fra/SHS_EDM_2017/Data - Données/CSV/SHS-62M004X-E-2017-Diary_F1.csv",
                    header=TRUE)
  
  # Load GeoJSON file of Canada provinces (available here: https://exploratory.io/map)
  canada_prov <- st_read("C:/Users/djimd/Downloads/SHS_EDM_2017-fra/Household_Spending/canada_provinces/canada_provinces.geojson", quiet = TRUE)
  
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
  
  output$table <- renderDataTable(func_income(as.integer(substr(input$prov,1,2))))
  
  output$map <- renderPlot({
    map <- ggplot(data = canada_prov,mapping = aes(fill = c(0,0,0,1:10)))
    map + geom_sf()
  })
  
}

shinyApp(ui = ui, server = server)


#install.packages("rgdal")
#library(rgdal)
# help: https://www.r-bloggers.com/2018/12/canada-map/

#canada <- readOGR(dsn="C:/Users/djimd/Downloads/SHS_EDM_2017-fra/Household_Spending/canada_map/gpr_000b11g_e.gml")

