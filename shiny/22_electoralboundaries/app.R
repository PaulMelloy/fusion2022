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
library(DT)
library(data.table)
source("R/plot_preference_flow.R")


divn <- read.csv("data/current-data-first-prefs-03-03.csv")
partydeets <- read.csv("data/PartyDetailsDownload.csv", skip = 1)
partycolours <- partydeets$colour
names(partycolours) <- partydeets$RegisteredPartyAb
senate_div <- read.csv("data/SenateFirstPrefsByDivisionByVoteTypeDownload-24310.csv", skip = 1)
senate_state <- read.csv("data/SenateFirstPrefsByStateByVoteTypeDownload-24310.csv", skip = 1)

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
  titlePanel("2022 Federal electoral divisions and 2019 election data"),
  p("This app was designed and coded by Dr Paul Melloy for the Fusion party 2022 electoral campaign"),
  a(href="https://github.com/PaulMelloy/fusion2022/tree/main/shiny/22_electoralboundaries",
    "Find the source code, make a pull request, or lodge an issue on GitHub PaulMelloy/Fusion2022"),
  # Add Nav bar and panels
  navbarPage(
    title = "Tools",
    id = "tabValue",
    tabPanel("Division boundaries", value = "map_page",
             h2("Download division boundaries"),
             p("1. Select the federal electoral division of interest"),
             p("2. Click on the other tabs to view more detail about the selected division"),
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
    tabPanel("Divison lower house stats", value = "div_stats",
             h2("2019 Summary of division stats for the house of reps"),
             h3("First preferences for the division"),
             tableOutput("house_FP_table"),
             p("Use the drop down menu to select a 2019 polling booth for more specific information"),
             column(width = 5,
                    p(""),
                    h3("Number of voters by polling place"),
                    checkboxInput("ppvc1", "Include PPVC", value = FALSE),
                    tableOutput("voters")
             ),
             column(width = 7,
                    uiOutput("booth_dropdown"),
                    plotOutput("Hrep_pf"),
                    ),
             p("Note: polling booths include prepolling (PPVC) and mail votes")
    ),
    tabPanel("State wide stats", value = "state_stats",
             column(width = 4,
                    h2("Obtain polling location data by state"),
                    selectInput(inputId = "state",
                                label = "State",
                                choices = unique(divn$State)), 
                    checkboxInput("ppvc", "Include PPVC", value = FALSE),
                    tableOutput("state_voters")),
             column(width = 6,
                    h3(textOutput(outputId = "senate_state_head")),
                    p("Sort table by clicking the arrows at the top of the table"),
                    DTOutput("state_senate"))
             ),
    tabPanel("Senate/Division", value = "div_senate",
             h2(textOutput("senate_div_head")),
             p("Change the division in the 'Dividion boundaries' tab"),
             DTOutput("div_senate"))
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Render a leaflet map at page loading
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
      # Project and select selected division
      divmap <-
        st_transform(filter(AU_bound, Elect_div == input$division),
                     "+proj=longlat +datum=WGS84")
      
      # Drop z dimension to make XY
      st_zm(divmap, drop = T, what = "ZM")
      
    })
  
  # Get the centroid of the division
  centdroid <-
    reactive({
      cent1 <- st_coordinates(st_centroid(div_map(),of_largest_polygon = TRUE))
    })
  
  # when division is changed re render the map with the new division
  observeEvent(input$division, {
    output$divmap <-
      renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lat = centdroid()[, "Y"],
                  lng = centdroid()[, "X"],
                  zoom = 10) %>%
          addPolygons(data = div_map())
      })
    
    # prep the download button
    output$dl_kml <- downloadHandler(
      filename = paste0(input$division, ".kml"),
      content = function(con) {
        # write division map as a KML
        st_write(div_map(), dsn = con, driver = "KML")
      }
    )
    
  })
  
  
  ### House of reps first preference data
  output$house_FP_table <-
    renderTable({
      fp19 %>%
        filter(DivisionNm == casefold(input$division, upper = TRUE)) %>%
        arrange(-Percent) %>%
        select(
          "DivisionNm",
          "BallotPosition",
          "PartyNm",
          "Surname",
          "GivenNm",
          "Elected",
          "HistoricElected",
          "OrdinaryVotes",
          "Percent"
        )
    })
  
  # pull in polling booth data from selected division
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
        filter(if (isFALSE(input$ppvc1)) {
          !(grepl("PPVC", PPNm))}else{
            rep(TRUE,n())
          })%>%
        mutate(
          progresive =
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
              TRUE ~ NA_real_
            ) *
            CalculationValue
        ) %>%
        group_by(PPNm) %>%
        summarise(
          progresive_score = sum(progresive,
                                 na.rm = TRUE),
          voter_turnout = as.integer(sum(CalculationValue))
        ) %>%
        mutate(progresive_score = progresive_score / voter_turnout) %>%
        arrange(-voter_turnout)
      
    })
  
  # Render a updated dropdown menu of booth choices
  output$booth_dropdown <-
    renderUI({
      booth_choice <- unique(hrep_pf_dat()$PPNm)
      selectInput("booth",
                  "Polling booth", choices = booth_choice)
    })
  
  # plot booth preference flows
  output$Hrep_pf <- renderPlot({
    plot_preference_flow(hrep_pf_dat(),
                         division = input$division,
                         polling_booth = input$booth,
                         PartyColour = partydeets)
  })
  
  # Tab 3
  
  state_polls <- reactive({
    
    
    state_divs_list <- 
      lapply(list.files("data/", pattern = input$state),
             function(x){
               read.csv(paste0("data/",x),skip = 1)
             })
    do.call("rbind", state_divs_list)
    
  })
  
  ### Voter turnout by polling location in state
  output$state_voters <-
    renderTable({
      state_polls() %>%
        filter(CountNum == 0 &
                 CalculationType == "Preference Count") %>%
        filter(if (isFALSE(input$ppvc)) {
          !(grepl("PPVC", PPNm))
        } else{
          rep(TRUE, n())
        }) %>% 
        mutate(
          progresive =
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
              PartyAb == "REAS" ~ 75,
              PartyAb == "SAL" ~ 85,
              PartyAb == "KAP" ~ 50,
              PartyAb == "AJP" ~ 75,
              PartyAb == "LDP" ~ 23,
              PartyAb == "SPP" ~ 70,
              PartyAb == "LAOL" ~ 15,
              PartyAb == "AUP" ~ 77,
              PartyAb == "AFN" ~ 35,
              PartyAb == "SEP" ~ 70,
              TRUE ~ NA_real_
            ) *
            CalculationValue
        ) %>%
        group_by(PPNm) %>%
        summarise(
          progresive_score = sum(progresive,
                                 na.rm = TRUE),
          voter_turnout = as.integer(sum(CalculationValue))
        ) %>%
        mutate(progresive_score = progresive_score / voter_turnout) %>%
        arrange(-voter_turnout)
      
    })
  
  output$state_senate<- renderDT({
    datatable(data.table(senate_state)[StateAb == input$state,
                             list(state = unique(StateAb),
                                   OrdinaryVotes = sum(OrdinaryVotes),
                                   AbsentVotes = sum(AbsentVotes),
                                   ProvisionalVotes = sum(ProvisionalVotes),
                                   PrePollVotes = sum(PrePollVotes),
                                   PostalVotes = sum(PostalVotes),
                                   TotalVotes = sum(TotalVotes)),
                                   by = PartyName],
              rownames = FALSE)
  })
  
  output$senate_state_head <- renderText({
    paste0("First preference votes for Senators by party in the state of ",input$state)
    })
  
  
  output$div_senate <- renderDT({
    datatable(data.table(senate_div)[DivisionNm == input$division,
                                       list(Senators_elected = sum(Elected == "Y"),
                                            Senators_HistoricElected = sum(HistoricElected == "Y"),
                                            OrdinaryVotes = sum(OrdinaryVotes),
                                            AbsentVotes = sum(AbsentVotes),
                                            ProvisionalVotes = sum(ProvisionalVotes),
                                            PrePollVotes = sum(PrePollVotes),
                                            PostalVotes = sum(PostalVotes),
                                            TotalVotes = sum(TotalVotes)),
                                       by = PartyName],
              rownames = FALSE)
    
  })
  
  output$senate_div_head <- renderText({
    paste0("First preference votes for Senators by party in the division of ",input$division)
  })
  

 }

# Run the application 
shinyApp(ui = ui, server = server)
