#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
library(htmltools)
library(eechidna)
library(ggplot2)
library(ggalluvial)
source("R/plot_preference_flow.R")


divn <- read.csv("data/current-data-first-prefs-03-03.csv")

find_div_pref_flow_file <- function(type = "HouseDopByPP",name){
  if(type == "HouseDopByPP"){
  paste0("data/HouseDopByPPDownload-24310-",
         unique(divn[divn$DivisionName == name,"State"]),
         "-",
         unique(divn[divn$DivisionName == name,"DivisionAb"]),
         ".csv"
         )
    }
}



## import shapefile
AU_bound <- st_read("divisions/2021_ELB_region.shp")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Display 2022 Federal electoral division boundaries"),
  
  navbarPage(
    title = "Tools",
    id = "tabValue",
    tabPanel("Division boundaries", value = "map_page",
             h2("Download division boundaries"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "division",
                   label = "Division name:",
                   choices = AU_bound$Elect_div,
                   selected = "Banks"
                 ),
                 downloadButton("dl_kml",
                                "Download as KML")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(leafletOutput("divmap", height = "100vh"))
             )),
    tabPanel("Div Rep stats", value = "div_stats",
             h2("2019 Summary of division stats for the house of reps"),
             h3("First preferences for the division"),
             tableOutput("house_FP_table"),
             column(width = 5,
                    p(""),
                    h3("Number of voters by polling place"),
                    tableOutput("voters")
             ),
             column(width = 7,
                    uiOutput("booth_dropdown"),
                    plotOutput("Hrep_pf"),
                    )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  output$divmap <- renderLeaflet({
    
    map1 <-
      st_transform(filter(AU_bound, Elect_div == input$division), "+proj=longlat +datum=WGS84")
    map1 <- st_zm(map1, drop = T, what = "ZM")
    cent1 <- st_coordinates(st_centroid(map1))
    
    leaflet() %>%
      addTiles() %>%
      setView(lat = cent1[,"Y"], lng = cent1[,"X"], zoom = 10) %>%
      addPolygons(data = map1, layerId = "A")
  })

  
  div_map <-
    reactive({
      
      # Attempt to allow downloading the divisions as a single layer
      # if(length(input$division) > 1){
      #   
      #   #divmap <- st_combine(filter(AU_bound, Elect_div %in% input$division))
      #   divmap <- 
      #     st_polygonize(
      #     st_line_merge(
      #     st_cast(
      #       filter(AU_bound, Elect_div %in% input$division),
      #       "MULTILINESTRING")
      #     ))
      #   
      #   divmap <-
      #     st_transform(divmap,
      #                  "+proj=longlat +datum=WGS84")
      # }else{
        divmap <-
          st_transform(filter(AU_bound, Elect_div == input$division),
                       "+proj=longlat +datum=WGS84")
        divmap <-
          st_transform(filter(AU_bound, Elect_div == input$division),
                       "+proj=longlat +datum=WGS84")
      #}
      
      st_zm(divmap, drop = T, what = "ZM")
      
    })
  
  centdroid <-
    reactive({
      cent1 <- st_coordinates(st_centroid(div_map(),of_largest_polygon = TRUE))
    })
  
  observeEvent(input$division, {
    
    
    output$divmap <-
      renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lat = centdroid()[,"Y"], lng = centdroid()[,"X"], zoom = 10) %>%
          addPolygons(data = div_map())
      })
    
    output$dl_kml <- downloadHandler(
      filename = paste0(input$division,".kml"),
      content = function(con){
        #dl_div_fn <- tempfile(pattern = "division",fileext = ".kml")
        st_write(div_map(), dsn= con, driver = "KML")
      })
    
  })
  
  
  ### House of reps first preference data
  output$house_FP_table <-
    renderTable({
      # tab1 <- divn[divn$DivisionName == input$division,]
      # vote_sum <- sum(tab1$Votes, na.rm = TRUE)
      # tab1$VotePercent <- paste0(round(tab1$Votes/vote_sum, 4) * 100, " %")
      # tab1[order(tab1$Votes),c("State", "DivisionName","CandidateSurname", "Votes","VotePercent")]
      
      fp19 %>%
        filter(DivisionNm == casefold(input$division, upper = TRUE)) %>%
        arrange(-Percent) %>%
        select("DivisionNm","BallotPosition","PartyNm",
               "Surname","GivenNm", "Elected", "HistoricElected",
               "OrdinaryVotes", "Percent")
      
    })
  
  hrep_pf_dat <- reactive({
    read.csv(find_div_pref_flow_file(name = input$division),
             skip = 1)
  })
  
  
  
  
  ### Voter turnout by polling location
  output$voters <-
    renderTable({
      
      hrep_pf_dat() %>%
        filter(CountNum == 0 &
                 CalculationType == "Preference Count") %>%
        mutate(progresive =
                 case_when(
                   PartyAb == "FACN" ~ 0,
                   PartyAb == "ALP" ~ 65,
                   PartyAb == "ON" ~ 20,
                   PartyAb == "LNP" ~ 45,
                   PartyAb == "LP" ~ 45,
                   PartyAb == "UAPP" ~ 25,
                   PartyAb == "DLP" ~ 50,
                   PartyAb == "GRN" ~ 80,
                   PartyAb == "GVIC" ~ 80,
                   PartyAb == "REAS" ~ 85,
                   PartyAb == "SAL" ~ 85,
                   PartyAb == "KAP" ~ 50,
                   PartyAb == "AJP" ~ 75,
                   PartyAb == "LDP" ~ 23,
                   PartyAb == "SPP" ~ 70,
                   PartyAb == "LAOL" ~ 15,
                   PartyAb == "AUP" ~ 77,
                   PartyAb == "AFN" ~ 35,
                   PartyAb == "SEP" ~ 70,
                   TRUE ~ NA_real_) * 
                 CalculationValue) %>%
        group_by(PPNm) %>%
        summarise(progresive_score = sum(progresive,
                                         na.rm = TRUE),
                  voter_turnout = as.integer(sum(CalculationValue))) %>%
        mutate(progresive_score = progresive_score/voter_turnout) %>%
        arrange(-voter_turnout)
      
    })
  
  output$booth_dropdown <-
    renderUI({
      booth_choice <- unique(hrep_pf_dat()$PPNm)
      selectInput("booth",
                  "Polling booth", choices = booth_choice)
    })

  output$Hrep_pf <- renderPlot({
    plot_preference_flow(hrep_pf_dat(),
                         division = input$division,
                         polling_booth = input$booth)
  })
  
  

 }

# Run the application 
shinyApp(ui = ui, server = server)
