#install.packages(c('shiny','sf','ggplot2','gghighlight'))
library(shiny)
library(sf) # for st_read
library(ggplot2) # for ggplot
library(dplyr) # for full_join
library(gghighlight) # for gghighlight

# functions

func_income <- function(province,data)
{
  IncomeTable <- data.frame(
    matrix(c(min(data[data$Prov==province,'HH_TotInc']),median(data[data$Prov==province,'HH_TotInc']),max(data[data$Prov==province,'HH_TotInc']),
             min(data[data$Prov==province,'HH_EarnInc']),median(data[data$Prov==province,'HH_EarnInc']),max(data[data$Prov==province,'HH_EarnInc']),
             min(data[data$Prov==province,'HH_InvInc']),median(data[data$Prov==province,'HH_InvInc']),max(data[data$Prov==province,'HH_InvInc']),
             min(data[data$Prov==province,'HH_GovInc']),median(data[data$Prov==province,'HH_GovInc']),max(data[data$Prov==province,'HH_GovInc']),
             min(data[data$Prov==province,'HH_OthInc']),median(data[data$Prov==province,'HH_OthInc']),max(data[data$Prov==province,'HH_OthInc'])),
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
                plotOutput(outputId="map_highlight")
                # Household characteristic
                #,plotOutput(outputId="pie_type")
                #,plotOutput(outputId="hist")
                #,plotOutput(outputId="pie_phone")
                #,dataTableOutput('table')
                #,plotOutput(outputId="map")
                )

server <- function(input, output) {
  
  # # Data: Enquête sur les dépenses des ménages, at 
  # https://www150.statcan.gc.ca/n1/pub/62m0004x/62m0004x2017001-fra.htm
  
  
  #Load data set
  diary <- read.csv(file="C:/Users/djimd/Downloads/SHS_EDM_2017-fra/SHS_EDM_2017/Data - Données/CSV/SHS-62M004X-E-2017-Diary_F1.csv",
                    header=TRUE)
  
  # Load GeoJSON file of Canada provinces (available here: https://exploratory.io/map)
  canada_prov <- st_read("C:/Users/djimd/Downloads/SHS_EDM_2017-fra/Household_Spending/canada_provinces/canada_provinces.geojson", quiet = TRUE)
  
  # harmonize with survey data: 14 - Atlantic provinces =  13 - New Brunswick + 11 - Prince Edward Island
  canada_prov <- canada_prov %>%
    mutate(PRUID = case_when(PRUID %in% c("11","13") ~ "14",
                             TRUE ~ PRUID))
  
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
  
  output$table <- renderDataTable(func_income(as.integer(substr(input$prov,1,2)),diary))
  
  output$map_highlight <- renderPlot({
    # not working for 14, 63. Check # ------------------------------------------
    ggplot(data = canada_prov)+
      geom_sf(fill = "red")+
      gghighlight(grepl(substr(input$prov,1,2), PRUID))+
      ggtitle("Selected province")
    
  })
  
  output$map <- renderPlot({

    # Merge map data with data to plot
    data_to_plot <- data.frame(
      matrix(c(canada_prov$PRUID,rep(NA,length(canada_prov$PRUID))),byrow = FALSE,ncol=2)
    )
    
    colnames(data_to_plot) <- c('ID','Median')
    
    for(p in canada_prov$PRUID){
      if(p %in% unique(diary$Prov))
      {
        data_to_plot[data_to_plot$ID==p,"Median"] = func_income(p,diary)["Total income","Median"]
      }
    }
    
    map.data <- full_join(canada_prov, data_to_plot, by = c("PRUID"="ID"))
    
    ggplot() +
      geom_sf(data=map.data, aes(fill=Median))+
      ggtitle("Comparison between provinces")
  })
  
}

shinyApp(ui = ui, server = server)



# help: https://www.r-bloggers.com/2018/12/canada-map/

# how to megre data with map data: https://stackoverflow.com/questions/55185546/merging-countries-provinces-in-shapefile-for-plotting-with-ggplot2



# 63 Capital of territories --------------------------------------------------------------

ggplot(data = canada_prov)+
  geom_sf()+#fill = "red"
  geom_point(y=60.721188, x=-135.056839, col = 'red',size = 3)+ # Whitehorse
  geom_point(y=62.453972, x=-114.371789, col = 'red',size = 3)+ # Yellowknife
  geom_point(y=63.748611, x=-68.519722, col = 'red',size = 3) # Iqaluit