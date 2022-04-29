#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(rgdal)
library(sf)
library(leaflet)
library(htmltools)


## import shapefile
AU_bound <- st_read("divisions/2021_ELB_region.shp")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Display 2022 Federal electoral division boundaries"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "division",
                        label = "Division name:",
                        choices = AU_bound$Elect_div,
                        selected = "Banks")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("divmap",height = "100vh") ,
          downloadButton("dl_kml",
                         "Download as KML")
          
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
  
  
  
  

    
  
  

 }

# Run the application 
shinyApp(ui = ui, server = server)
