# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(terra)
library(sf)
library(ggpubr)
library(highcharter)
library(grid)
library(corrplot)


# Dataset
WHR <- read.csv("Data/WHR_Cleaned_Data.csv",sep=',', header = TRUE)

# Reading in the shape file
# https://thematicmapping.org/downloads/world_borders.php
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Now I have the Spdf object (spatial polygon data frame).The maps can now be produced
worldCountries <- sp::merge(world_spdf, WHR, by.x="NAME", by.y="Country", all.x=TRUE, duplicateGeoms = TRUE) 


# Create a color palette with handmade bins.
mypalette <- colorBin( palette="YlOrBr", domain=worldCountries@data$Score, bins=4)

# Define UI
# fluidPage
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # navbarPage
                navbarPage(
                  "World Happiness Report Analysis",
                  position = c("static-top"),
                  
                  # Data Tab
                  tabPanel("Data",
                           sidebarPanel(
                             selectInput("dataset", 
                                         label = "Select the year of the dataset: ",
                                         choices = c("All", unique(sort(WHR$Year))),
                                         selected = "All"),
                             
                           width = 3, 
                           
                           # Button
                           downloadButton("downloadData", "Download")
                           
                           
                           ), # sidebarPanel
                           mainPanel(
                             
                             tabsetPanel(
                               
                               tabPanel(
                                 title = "About the Dataset",
                                 h4("The context of the dataset:"),
                                 p("The World Happiness Report is a landmark survey of the state of global happiness.
                                 The happiness scores and rankings use data from the Gallup World Poll. 
                                   The scores are based on answers to the main life evaluation question asked in the poll, 
                                   with the best possible life for them being a 10 and the worst possible life being a 0 and to rate their own current lives on that scale. 
                                   The columns following the happiness score estimate the extent to which each of six factors – economic production, social support, life expectancy, freedom, absence of corruption, and generosity."),
                                 
                                 h4("Factors/Variables of the dataset:"),
                                 p("Country: Name of the Country.", tags$br(),
                                   "Region: The Region the Country belongs too.", tags$br(),
                                   "Year: The Year the data was collected on.", tags$br(),
                                   "Happiness Score/Score: A metric measured by asking the sampled people the question: How would you rate your happiness on a scale of 0 to 10 where 10 is the happiest.", tags$br(),
                                   "Economy (GDP Per Capita): GDP per capita is in terms of Purchasing Power Parity (PPP) adjusted to constant 2017 international dollars, taken from the World Development Indicators (WDI).", tags$br(),
                                   "Family (Social Support): Social support is the national average of the binary responses to the GWP question “If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?”", tags$br(),
                                   "Health (Life Expectancy): The time series for healthy life expectancy at birth is constructed based on data from the World Health Organization (WHO).", tags$br(),
                                   "Freedom: Freedom to make life choices based on the binary GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?” ", tags$br(),
                                   "Trust (Government Corruption): Perceptions of corruption are the average of binary responses to two GWP questions: “Is corruption widespread throughout the government in this country or not?” and “Is corruption widespread within businesses in this country or not?”", tags$br(),
                                   "Generosity: Generosity is the residual of regressing the national average of GWP responses to the donation question “Have you donated money to a charity in the past month?”", tags$br(),
                                   
                                   ),
                                  h4("References and Links:"),
                                 tags$a(href="https://www.kaggle.com/datasets/unsdsn/world-happiness", 
                                        "Link to the dataset used and Reference to the context", tags$br()),
                                 tags$a(href="https://worldhappiness.report/", 
                                        "Link to the World Happiness Report and Factors/Variable Descriptions"),
                                 
                                 h4("More Information"),
                                 p("For more information, about how I cleaned the data, how the the map was created and the code, link below."),
                                 tags$a(href="https://github.com/Ana-ishSodhi/R-Shiny-World-Happiness-Report", 
                                        "Link to my Github Repository", tags$br()),
                               ),
                               tabPanel(
                                 title = "Table",
                                 div(DT::dataTableOutput("tableOutput"), style = "font-size: 90%; width: 90%")
                                 
                               ),
                               
                               tabPanel(
                                 title = "Structure",
                                 h4('The structure of the dataset:'),
                                 verbatimTextOutput("structureOutput"),
                                 h4('The dimension of the dataset:'),
                                 verbatimTextOutput("dimensionOutput"),
                                 
                               ),
                               
                               tabPanel(
                                 title = "Summary",
                                 h4('The summary of the dataset:'),
                                 verbatimTextOutput("summaryOutput"),
                                 
                               ),
                               tabPanel(
                                 title = "Dataset",
                                 tableOutput("datasetOutput"),
                                 
                               ),
                             )
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Analysis",
                             tabsetPanel(
                               tabPanel(
                                 title = "Selected Year",
                                 
                                 selectInput("graph_year", 
                                             label = "Selected dataset: ",
                                             choices = c(unique(sort(WHR$Year))),
                                             selected = "2015"),
                                 h4("Apologies, it might take some time to load", align ="center"),
                                 h2("Top 5 Countries based on Happiness Score in the Selected Year", align = "center"),
                                 

                                  fluidRow(style='height:400px',
                                     column(3,
                                            plotlyOutput("Top_5_ANZ", width = "400px", height = "500px")
                                     ),
                                     column(3,
                                            plotlyOutput("Top_5_CEE", width = "400px", height = "500px"),
                                     ),
                                     column(3,
                                            plotlyOutput("Top_5_EA", width = "400px", height = "500px"),
                                     ),
                                     column(3,
                                            plotlyOutput("Top_5_LAC", width = "400px", height = "500px"),
                                     )
                                   ),
                                  fluidRow(style='height:400px',
                                      column(3,
                                            plotlyOutput("Top_5_MENA", width = "400px", height = "500px")
                                      ),
                                      column(3,
                                            plotlyOutput("Top_5_NA", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                            plotlyOutput("Top_5_SEA", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                            plotlyOutput("Top_5_SA", width = "400px", height = "500px"),
                                      )
                                 ),
                                   fluidRow(style='height:400px',
                                     column(4, 
                                            plotlyOutput("Top_5_SSA", width = "400px", height = "500px"),
                                     ),
                                     column(4,
                                            plotlyOutput("Top_5_WE", width = "400px", height = "500px"),
                                     ),
                                     column(4,
                                            plotlyOutput("Top_5_Yearly", width = "400px", height = "500px"),
                                   )
                                   
                                  ),
                                 
                                 h2("Number of Countries by Region in the Selected Year", align = "center"),
                                  
                                 fluidRow(style='height:400px',
                                          column(12, 
                                                 plotlyOutput("No_Of_Country", height = "500px"),
                                          )
                                          
                                 ),
                                 
                                 h2("Scatterplots of Factors vs Happiness Score in the Selected Year", align = "center"),
                                 
                                 fluidRow(style='height:400px',
                                          column(4, 
                                                 plotlyOutput("Scatter_Economy", width = "500px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotlyOutput("Scatter_Family", width = "500px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotlyOutput("Scatter_Health", width = "500px", height = "500px"),
                                          )
                                          
                                 ),
                                 fluidRow(style='height:400px',
                                          column(4, 
                                                 plotlyOutput("Scatter_Freedom", width = "500px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotlyOutput("Scatter_Govt", width = "500px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotlyOutput("Scatter_Generosity", width = "500px", height = "500px"),
                                          )
                                          
                                 ),
                                 
                                 h2("Correlation Heatmap on Factors by Region in Selected Year", align = "center"),
                                 
                                 fluidRow(style='height:400px',
                                          column(3,
                                                 plotOutput("Corr_ANZ", width = "400px", height = "500px")
                                          ),
                                          column(3,
                                                 plotOutput("Corr_CEE", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotOutput("Corr_EA", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotOutput("Corr_LAC", width = "400px", height = "500px"),
                                          )
                                 ),
                                 fluidRow(style='height:400px',
                                          column(3,
                                                 plotOutput("Corr_MENA", width = "400px", height = "500px")
                                          ),
                                          column(3,
                                                 plotOutput("Corr_NA", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotOutput("Corr_SEA", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotOutput("Corr_SA", width = "400px", height = "500px"),
                                          )
                                 ),
                                 fluidRow(style='height:400px',
                                          column(4, 
                                                 plotOutput("Corr_SSA", width = "400px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotOutput("Corr_WE", width = "400px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotOutput("Corr_Year", width = "400px", height = "500px"),
                                          )
                                          
                                 ),
                                 
                                 h2("Spread and Skewness on Factors by Region in Selected Year", align = "center"),
                                 
                                 fluidRow(style='height:400px',
                                          column(3,
                                                 plotlyOutput("Skew_Score", width = "400px", height = "500px")
                                          ),
                                          column(3,
                                                 plotlyOutput("Skew_Ecomony", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotlyOutput("Skew_Family", width = "400px", height = "500px"),
                                          ),
                                          column(3,
                                                 plotlyOutput("Skew_Health", width = "400px", height = "500px"),
                                          )
                                 ),
                                 fluidRow(style='height:400px',
                                          column(4,
                                                 plotlyOutput("Skew_Freedom", width = "400px", height = "500px")
                                          ),
                                          column(4,
                                                 plotlyOutput("Skew_Govt", width = "400px", height = "500px"),
                                          ),
                                          column(4,
                                                 plotlyOutput("Skew_Generosity", width = "400px", height = "500px"),
                                          )
                                 ),
                             
                           ),
                           tabPanel(
                             title = "All Years",
                             
                              h4("Apologies, it might take some time to load", align ="center"),
                              h2("Number of Unique Countries by Region in All The Years", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(12, 
                                             plotlyOutput("All_No_Of_Country", height = "500px"),
                                      )
                                      
                             ),
                             
                             h2("Scatterplots of Factors vs Happiness Score in All The Years", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(4, 
                                             plotlyOutput("Scatter_Economy_All", width = "500px", height = "500px"),
                                      ),
                                      column(4,
                                             plotlyOutput("Scatter_Family_All", width = "500px", height = "500px"),
                                      ),
                                      column(4,
                                             plotlyOutput("Scatter_Health_All", width = "500px", height = "500px"),
                                      )
                                      
                             ),
                             fluidRow(style='height:400px',
                                      column(4, 
                                             plotlyOutput("Scatter_Freedom_All", width = "500px", height = "500px"),
                                      ),
                                      column(4,
                                             plotlyOutput("Scatter_Govt_All", width = "500px", height = "500px"),
                                      ),
                                      column(4,
                                             plotlyOutput("Scatter_Generosity_All", width = "500px", height = "500px"),
                                      )
                                      
                             ),
                             
                             h2("Correlation Heatmap on Factors by Region in All The Years", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(3,
                                             plotOutput("Corr_ANZ_All", width = "400px", height = "500px")
                                      ),
                                      column(3,
                                             plotOutput("Corr_CEE_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotOutput("Corr_EA_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotOutput("Corr_LAC_All", width = "400px", height = "500px"),
                                      )
                             ),
                             fluidRow(style='height:400px',
                                      column(3,
                                             plotOutput("Corr_MENA_All", width = "400px", height = "500px")
                                      ),
                                      column(3,
                                             plotOutput("Corr_NA_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotOutput("Corr_SEA_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotOutput("Corr_SA_All", width = "400px", height = "500px"),
                                      )
                             ),
                             fluidRow(style='height:400px',
                                      column(4, 
                                             plotOutput("Corr_SSA_All", width = "400px", height = "500px"),
                                      ),
                                      column(4,
                                             plotOutput("Corr_WE_All", width = "400px", height = "500px"),
                                      ),
                                      column(4,
                                             plotOutput("Corr_Year_All", width = "400px", height = "500px"),
                                      )
                                      
                             ),
                             
                             h2("Distribution of Happiness Score Over The Years", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(12, 
                                             plotOutput("Dist_Happiness", height = "500px"),
                                      )
                                      
                             ),
                             
                             h2("Change in Factors Over Time", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(3,
                                             highchartOutput("Over_Time_Score", width = "400px", height = "500px")
                                      ),
                                      column(3,
                                             highchartOutput("Over_Time_Ecomony", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             highchartOutput("Over_Time_Family", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             highchartOutput("Over_Time_Health", width = "400px", height = "500px"),
                                      )
                             ),
                             fluidRow(style='height:400px',
                                      column(4,
                                             highchartOutput("Over_Time_Freedom", width = "400px", height = "500px")
                                      ),
                                      column(4,
                                             highchartOutput("Over_Time_Govt", width = "400px", height = "500px"),
                                      ),
                                      column(4,
                                             highchartOutput("Over_Time_Generosity", width = "400px", height = "500px"),
                                      )
                             ),
                             
                             h2("Spread and Skewness on Factors by Region in All The Year", align = "center"),
                             
                             fluidRow(style='height:400px',
                                      column(3,
                                             plotlyOutput("Skew_Score_All", width = "400px", height = "500px")
                                      ),
                                      column(3,
                                             plotlyOutput("Skew_Ecomony_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotlyOutput("Skew_Family_All", width = "400px", height = "500px"),
                                      ),
                                      column(3,
                                             plotlyOutput("Skew_Health_All", width = "400px", height = "500px"),
                                      )
                             ),
                             fluidRow(style='height:400px',
                                      column(4,
                                             plotlyOutput("Skew_Freedom_All", width = "400px", height = "500px")
                                      ),
                                      column(4,
                                             plotlyOutput("Skew_Govt_All", width = "400px", height = "500px"),
                                      ),
                                      column(4,
                                             plotlyOutput("Skew_Generosity_All", width = "400px", height = "500px"),
                                      )
                             ),
                             
                             
                             
                           ),
                           
                           
                    ), 
                  ), # Navbar 1, tabPanel
                  
                  # Map Tab
                  tabPanel("Map",
                           mainPanel(
                             leafletOutput("map", width = "150%", height="91vh"),
                             absolutePanel(top = 10, left = 1250,
                                           selectInput("selectedYear", "World Happiness Report Year: ",
                                                       unique(sort(WHR$Year))),
                                           ),
                             
                           ) # mainPanel
                           
                  ),
                  
                )
          )


# Define server function  
server <- function(input, output, session) {
  
  # Data Tab
  # Create a reactive expression for the data
  dataset <- reactive({
    if(input$dataset == "All"){
      data <- WHR
      
    }else{
      data <- filter(WHR, Year == input$dataset)
    }
    
    return(data)
  })
  
  output$tableOutput <- renderDataTable({
    dataset()
    
  },
  options = list(pageLength = 10))
  
  output$structureOutput <- renderPrint({
    str(dataset())
  })
  
  output$dimensionOutput <- renderPrint({
    dim(dataset())
  })
  
  output$summaryOutput <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$datasetOutput <- renderTable({
    dataset()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  # Analysis Tab
  # Data Tab
  # Create a reactive expression for the data
  graph_year <- reactive({
    if(input$graph_year == "2015"){
      year <- filter(WHR$Year == 2015)
      
    }else{
      year <- filter(WHR$Year == input$graph_year)
    }
    
    return(year)
  })
  
# Selected Year Tab Inside Data Analysis
  
# Top 5 Countries based on Happiness Score in the Selected Year
  
  output$Top_5_ANZ <- renderPlotly({
    
    Top_5_ANZ <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Australia and New Zealand") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Australia and New Zealand Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 12, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    
    return(ggplotly(Top_5_ANZ, tooltip="text"))
    
  })
  
  output$Top_5_CEE <- renderPlotly({
    
    Top_5_CEE <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Central and Eastern Europe") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Central and Eastern Europe Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_CEE, tooltip="text"))
    
  })
  
  output$Top_5_EA <- renderPlotly({
  
    Top_5_EA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Eastern Asia") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Eastern Asia Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_EA, tooltip="text"))
  
  })
  
  output$Top_5_LAC <- renderPlotly({
    
    Top_5_LAC <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Latin America and Caribbean") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Latin America and Caribbean Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_LAC, tooltip="text"))
    
  })
  
  output$Top_5_MENA <- renderPlotly({
    
    Top_5_MENA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Middle East and Northern Africa") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Middle East and Northern Africa Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_MENA, tooltip="text"))
    
  })
  
  output$Top_5_NA <- renderPlotly({
    
    Top_5_NA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "North America") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'North America Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_NA, tooltip="text"))
    
  })
  
  output$Top_5_SEA <- renderPlotly({
    
    Top_5_SEA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Southeastern Asia") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Southeastern Asia Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_SEA, tooltip="text"))
    
  })
  
  output$Top_5_SA <- renderPlotly({
    
    Top_5_SA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Southern Asia") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Southern Asia Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_SA, tooltip="text"))
    
  })
  
  output$Top_5_SSA <- renderPlotly({
    
    Top_5_SSA <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Sub-Saharan Africa") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Sub-Saharan Africa Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_SSA, tooltip="text"))
    
  })
  
  output$Top_5_WE <- renderPlotly({
    
    Top_5_WE <- WHR %>%
      filter(WHR$Year == input$graph_year, WHR$Region == "Western Europe") %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Western Europe Region', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(Top_5_WE, tooltip="text"))
    
  })
  
  output$Top_5_Yearly <- renderPlotly({
    
    Top_5_yearly <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      top_n(n=5, wt=Score) %>%
      group_by(Country) %>%
      ggplot(aes(x= reorder(Country, -Score), 
                 y = Score, fill=Country, text=paste("Country:",Country,"<br>Score:",Score))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Top Happiness Score Out of All Countries', 
           x = "Country", y = "Happiness Score") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return (ggplotly(Top_5_yearly, tooltip="text"))
    
  })
  
  # Number of Countries by Region in the Selected Year
  
  output$No_Of_Country <- renderPlotly({
    
    No_Of_Country <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      summarise(num = n_distinct(Country)) %>%
      ggplot(aes(x= Region, 
                 y = num, fill=Region, text=paste("Region:",Region,"<br>Number of Countries:",num))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Number of Countries by Region', 
           x = "Regions", y = "Number of Countries") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    return(ggplotly(No_Of_Country, tooltip="text"))
    
  })
  
  # Scatterplots of Factors vs Happiness Score in the Selected Year
  
  output$Scatter_Economy <- renderPlotly({
    
    scatter_economy <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Economy, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x, method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Economy vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_economy))
    
  })
  
  output$Scatter_Family <- renderPlotly({
    
    scatter_family <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Family, y = Score, color = Region)) +
      geom_point() +  
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Family vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_family))
    
  })
  
  output$Scatter_Health <- renderPlotly({
    
    scatter_health <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Health, y = Score, color = Region)) +
      geom_point() +  
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Health/Life Expectancy vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_health))
    
  })
  
  output$Scatter_Freedom <- renderPlotly({
    
    scatter_freedom <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Freedom, y = Score, color = Region)) +
      geom_point() +  
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Freedom vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_freedom))
    
  })
  
  output$Scatter_Govt <- renderPlotly({
    
    scatter_govt <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Government, y = Score, color = Region)) +
      geom_point() +  
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Trust in Government vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_govt))
    
  })
  
  output$Scatter_Generosity <- renderPlotly({
    
    scatter_generosity <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      group_by(Region) %>%
      ggplot(aes(x = Generosity, y = Score, color = Region)) +
      geom_point() +  
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Generosity vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')
    
    return(ggplotly(scatter_generosity))
    
  })
  
  # Correlation Heatmap of Factors by Region in Selected Year
  
  output$Corr_ANZ <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Australia and New Zealand")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Australia and New Zealand Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_CEE <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Central and Eastern Europe")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Central and Eastern Europe Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_EA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Eastern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Eastern Asia Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_LAC <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Latin America and Caribbean")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Latin America and Caribbean Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_MENA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Middle East and Northern Africa")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Middle East and Northern Africa Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_NA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="North America")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the North America Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_SEA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Southeastern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Southeastern Asia Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_SA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Southern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Southern Asia Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_SSA <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Sub-Saharan Africa")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Sub-Saharan Africa Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_WE <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year, WHR$Region =="Western Europe")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation for the Western Europe Region", mar=c(0,0,1,0)))
    
  })
  
  output$Corr_Year <- renderPlot({
    
    filter_corr <- filter(WHR, WHR$Year == input$graph_year)
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)
    
    return(corrplot(corr1, method = 'number', title = "Correlation on All Factors for the Year", mar=c(0,0,1,0)))
    
  })
  
  # Spread and Skewness on Factors by Region in Selected Year
  
  output$Skew_Score <- renderPlotly({
    
    Skew_Score <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Score)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Happiness Score by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Score))
    
  })
  
  output$Skew_Ecomony <- renderPlotly({
    
    Skew_Ecomony <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Economy)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Economy by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Ecomony))
    
  })
  
  output$Skew_Family <- renderPlotly({
    
    Skew_Family <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Family)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Family by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Family))
    
  })
  
  output$Skew_Health <- renderPlotly({
    
    Skew_Health <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Health)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Health/Life Expectancy by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Health))
    
  })
  
  output$Skew_Freedom <- renderPlotly({
    
    Skew_Freedom <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Freedom)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Freedom by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Freedom))
    
  })
  
  output$Skew_Govt <- renderPlotly({
    
    Skew_Govt <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Government)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Government by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Govt))
    
  })
  
  output$Skew_Generosity <- renderPlotly({
    
    Skew_Generosity <- WHR %>%
      filter(WHR$Year == input$graph_year) %>%
      ggplot(aes(x = Region, y = Generosity)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Generosity by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')
    
    return(ggplotly(Skew_Generosity))
    
  })
  
  # All Years Tab Inside Data Analysis
  # Number of Countries by Region in all of the Year
  
  output$All_No_Of_Country <- renderPlotly({
    
    All_No_Of_Country <- WHR %>%
      group_by(Region) %>%
      summarise(num = n_distinct(Country)) %>%
      ggplot(aes(x= Region, 
                 y = num, fill=Region, text=paste("Region:",Region,"<br>Number of Countries:",num))) +
      geom_bar(stat='identity', alpha = 0.85)+
      labs(title = 'Number of Unique Countries by Region in the All Years', 
           x = "Regions", y = "Number of Countries") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 10), 
            axis.text.x = element_text(angle=70, hjust = 1),
            legend.position = 'none')
    
    ggplotly(All_No_Of_Country, tooltip="text")
    
  })
  
  # Scatterplots of Factors vs Happiness Score in All The Years
  
  output$Scatter_Economy_All <- renderPlotly({

    scatter_economy_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Economy, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x, method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Economy vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_economy_all))

  })

  output$Scatter_Family_All <- renderPlotly({

    scatter_family_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Family, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Family vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_family_all))

  })

  output$Scatter_Health_All <- renderPlotly({

    scatter_health_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Health, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Health/Life Expectancy vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_health_all))

  })

  output$Scatter_Freedom_All <- renderPlotly({

    scatter_freedom_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Freedom, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Freedom vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_freedom_all))

  })

  output$Scatter_Govt_All <- renderPlotly({

    scatter_govt_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Government, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Trust in Government vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_govt_all))

  })

  output$Scatter_Generosity_All <- renderPlotly({

    scatter_generosity_all <- WHR %>%
      group_by(Region) %>%
      ggplot(aes(x = Generosity, y = Score, color = Region)) +
      geom_point() +
      geom_smooth(formula = y ~ x,method = "lm", fullrange = TRUE, se=FALSE) +
      facet_wrap(~Region, nrow = 4, ncol = 3) +
      labs(title = "Scatter plot of Generosity vs Happiness") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),  legend.position = 'none')

    return(ggplotly(scatter_generosity_all))

  })

  # Correlation Heatmap of Factors by Region out of all the years
  
  output$Corr_ANZ_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Australia and New Zealand")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Australia and New Zealand Region", mar=c(0,0,1,0)))

  })

  output$Corr_CEE_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Central and Eastern Europe")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Central and Eastern Europe Region", mar=c(0,0,1,0)))

  })

  output$Corr_EA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Eastern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Eastern Asia Region", mar=c(0,0,1,0)))

  })

  output$Corr_LAC_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Latin America and Caribbean")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Latin America and Caribbean Region", mar=c(0,0,1,0)))

  })

  output$Corr_MENA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Middle East and Northern Africa")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Middle East and Northern Africa Region", mar=c(0,0,1,0)))

  })

  output$Corr_NA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="North America")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the North America Region", mar=c(0,0,1,0)))

  })

  output$Corr_SEA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Southeastern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Southeastern Asia Region", mar=c(0,0,1,0)))

  })

  output$Corr_SA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Southern Asia")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Southern Asia Region", mar=c(0,0,1,0)))

  })

  output$Corr_SSA_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Sub-Saharan Africa")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Sub-Saharan Africa Region", mar=c(0,0,1,0)))

  })

  output$Corr_WE_All <- renderPlot({

    filter_corr <- filter(WHR, WHR$Region =="Western Europe")
    corr_data <- filter_corr[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation for the Western Europe Region", mar=c(0,0,1,0)))

  })

  output$Corr_Year_All <- renderPlot({

    corr_data <- WHR[, c('Score', 'Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
    corr1 <- cor(corr_data)

    return(corrplot(corr1, method = 'number', title = "Correlation on All Factors for All The Year", mar=c(0,0,1,0)))

  })
  
  # Distribution in Happiness Score over the years
  
  output$Dist_Happiness <- renderPlot({
    dist_happiness <- WHR %>%
      ggplot(aes(x=Score, group=Year, fill=factor(Year))) +
      geom_histogram(bins=10, position = 'identity',color =
                       'white', fill = '#0066CC') +
                       labs(title = 'Distribution of Happiness Score',
                            x = 'Happiness Score', y = "Counts") +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            text = element_text(size = 14))+
      facet_wrap(~ Year)

    return(dist_happiness)
  })
  
  # Change in Factors Over Time
  
  output$Over_Time_Score <- renderHighchart({

    Over_time_happiness <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_score = mean(Score), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_score, group = Region)) %>%
      hc_title(
        text = "Happiness Score Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_happiness)

  })

  output$Over_Time_Ecomony <- renderHighchart({

    Over_time_gdp <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_ec = mean(Economy), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_ec, group = Region)) %>%
      hc_title(
        text = "Economy Over Time Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_gdp)

  })

  output$Over_Time_Family <- renderHighchart({

    Over_time_family <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_fm = mean(Family), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_fm, group = Region)) %>%
      hc_title(
        text = "Family and Social Support Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_family)

  })

  output$Over_Time_Health <- renderHighchart({

    Over_time_health <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_h = mean(Health), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_h, group = Region)) %>%
      hc_title(
        text = "Health and Life Expectancy Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_health)

  })

  output$Over_Time_Freedom <- renderHighchart({

    Over_time_freedom <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_fr = mean(Freedom), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_fr, group = Region)) %>%
      hc_title(
        text = "Freedom Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_freedom)

  })

  output$Over_Time_Govt <- renderHighchart({

    Over_time_government <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_cr = mean(Government), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_cr, group = Region)) %>%
      hc_title(
        text = "Trust in Government Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_government)

  })

  output$Over_Time_Generosity <- renderHighchart({

    Over_time_generosity <- WHR %>%
      group_by(Region, Year) %>%
      summarise(avg_cr = mean(Generosity), .groups = "keep") %>%
      hchart('line', hcaes(x = Year, y = avg_cr, group = Region)) %>%
      hc_title(
        text = "Generosity Over Time",
        style = list(fontSize = "16px"),
        align = "center"
      )

    return(Over_time_generosity)

  })
  
  # Spread and Skewness on Factors by Region over all the years
  
  output$Skew_Score_All <- renderPlotly({

    Skew_Score_all <- WHR %>%
      ggplot(aes(x = Region, y = Score)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Happiness Score by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Score_all))

  })

  output$Skew_Ecomony_All <- renderPlotly({

    Skew_Ecomony_all <- WHR %>%
      ggplot(aes(x = Region, y = Economy)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Economy by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Ecomony_all))

  })

  output$Skew_Family_All <- renderPlotly({

    Skew_Family_all <- WHR %>%
      ggplot(aes(x = Region, y = Family)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Family by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Family_all))

  })

  output$Skew_Health_All <- renderPlotly({

    Skew_Health_all <- WHR %>%
      ggplot(aes(x = Region, y = Health)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Health/Life Expectancy by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Health_all))

  })

  output$Skew_Freedom_All <- renderPlotly({

    Skew_Freedom_all <- WHR %>%
      ggplot(aes(x = Region, y = Freedom)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Freedom by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Freedom_all))

  })

  output$Skew_Govt_All <- renderPlotly({

    Skew_Govt_all <- WHR %>%
      ggplot(aes(x = Region, y = Government)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Government by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Govt_all))

  })

  output$Skew_Generosity_All <- renderPlotly({

    Skew_Generosity_all <- WHR %>%
      ggplot(aes(x = Region, y = Generosity)) +
      geom_boxplot(aes(fill=Region)) + theme_bw() +
      labs(title = 'Spread of Generosity by Region')+ theme_bw() +
      theme(axis.title = element_text(size = (10)), plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=70, hjust = 1), legend.position = 'none')

    return(ggplotly(Skew_Generosity_all))

  })
  
  # Map Tab
  output$map <- renderLeaflet({
    
    subset_data <- subset(worldCountries, worldCountries@data$Year == 2015)
    
    # Map hover information
    mytext <- paste(
      "Country: ", subset_data@data$NAME,"<br/>", 
      "Region: ", subset_data@data$Region,"<br/>",
      "Year: ", subset_data@data$Year,"<br/>",
      "Happiness: ", subset_data@data$Score, "<br/>", 
      "Economy: ", subset_data@data$Economy,"<br/>",
      "Family: ", subset_data@data$Family,"<br/>",
      "Health: ", subset_data@data$Health,"<br/>",
      "Freedom: ", subset_data@data$Freedom,"<br/>",
      "Government (Trust): ", subset_data@data$Government,"<br/>",
      "Generosity: ", subset_data@data$Generosity,
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet() %>% 
      addTiles() %>%
      setView( lat=10, lng=0 , zoom=2) %>% #sets default map pan
      setMaxBounds( lng1 = -180, lat1 = -87, #stops the map being dragged out of bounds
                    lng2 = 200, lat2 = 97) %>%
    addPolygons( 
      data = subset_data,
      fillColor = ~mypalette(Score), 
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="black", 
      weight=0.7,
      
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      ),
      
      highlight = highlightOptions(
        weight = 2, #line width
        color = "black",
        fillOpacity = 1.0,
        opacity = 1.0,
        bringToFront = TRUE)
      
    ) %>%
      addLegend( 
        values=~Score, #data in legend  
        opacity=0.9, 
        title = "World Happiness", 
        position = "bottomleft",
        colors = c("#e2e2e2", "#e1a692", "#de6e56", "#e14b31", "#c23728"),
        labels = c("Less Happy", "", "", "", "More Happy"),
      )
      
  })
  
  observe({
    
    subset_data <- subset(worldCountries, worldCountries@data$Year == input$selectedYear)
    
    # Map hover information
    mytext <- paste(
      "Country: ", subset_data@data$NAME,"<br/>", 
      "Region: ", subset_data@data$Region,"<br/>",
      "Year: ", subset_data@data$Year,"<br/>",
      "Happiness: ", subset_data@data$Score, "<br/>", 
      "Economy: ", subset_data@data$Economy,"<br/>",
      "Family: ", subset_data@data$Family,"<br/>",
      "Health: ", subset_data@data$Health,"<br/>",
      "Freedom: ", subset_data@data$Freedom,"<br/>",
      "Government (Trust): ", subset_data@data$Government,"<br/>",
      "Generosity: ", subset_data@data$Generosity,
      sep="") %>%
      lapply(htmltools::HTML)
    
    leafletProxy("map", data = subset_data) %>% clearControls() %>%
      addTiles() %>%
      setView( lat=10, lng=0 , zoom=2) %>% #sets default map pan
      setMaxBounds( lng1 = -180, lat1 = -87, #stops the map being dragged out of bounds
                    lng2 = 200, lat2 = 97) %>%
      addPolygons( 
        data = subset_data,
        fillColor = ~mypalette(Score), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="black", 
        weight=0.7,
        
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        ),
        
        highlight = highlightOptions(
          weight = 2, #line width
          color = "black",
          fillOpacity = 1.0,
          opacity = 1.0,
          bringToFront = TRUE)
        
      ) %>%
        addLegend( 
          values=~Score, #data in legend  
          opacity=0.9, 
          title = "World Happiness", 
          position = "bottomleft",
          colors = c("#e2e2e2", "#e1a692", "#de6e56", "#e14b31", "#c23728"),
          labels = c("Less Happy", "", "", "", "More Happy"),
        )
    })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)