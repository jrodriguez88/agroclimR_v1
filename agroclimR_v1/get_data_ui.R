library(shiny)
library(leaflet)
require(geonames)
library(tidyverse)
library(curl)
library(lubridate)
library(jsonlite)
library(naniar)
library(tictoc)


source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_nasapower.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_soilgrids.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/utils/utils_crop_model.R", encoding = "UTF-8")



ui <- fluidPage(
  titlePanel("Select or enter your location"),
  shinythemes::shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Ingrese un nombre: (Sin separadores)", "LOCATION"),
#      numericInput("lon", label = h3("Longitude:"), value = -74),
#      numericInput("lat", label = h3("Latitude:"), value = 3.7),
#      numericInput("alt", label = h3("Elevacion:"), value = 1000),
      tableOutput('ubicacion'),
#      actionButton("recalc", "Seleccionar ubicacion"),
      dateInput('ini_date',
          label = 'Fecha Inicial: yyyy-mm-dd',
          value = Sys.Date() - months(12)
),
      dateInput('end_date',
          label = 'Fecha Final: yyyy-mm-dd',
          value = Sys.Date() - months(6)
),
##cuadro dialogo 
      actionButton("dlnasa", "Descarga NASAPOWER"),   
      actionButton("dlsoilgrids", "Descarga SOILGRIDS"),

#      actionButton("recalc", "Show point")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Mapa', leafletOutput('map')),
        tabPanel('Datos Clima', DT::DTOutput('nasa_df')),
        tabPanel('Datos Suelo', tableOutput('soil_list')),
        tabPanel('Grafico Clima', plotly::plotlyOutput('plot_clima'))
  ))
)
  
)

server <- function(input, output, session) {
  
  
  #### Reactive process
  
  click <- reactive({
    input$map_click 
  })
  
  elev <- reactive({
    click <- click()
    get_elevation(click$lat, click$lng)
    
  })
  
  dl_nasa <- eventReactive(input$dlnasa, {
    click = click()
    get_data_nasapower(click$lat, click$lng, input$ini_date, input$end_date) %>%
      from_nasa_to_model()
  }, ignoreNULL = FALSE)
  
  dl_soilgrids <- eventReactive(input$dlsoilgrids, {
    click = click()
    get_data_soilgrids(click$lat, click$lng) %>%
      from_soilgrids_to_aquacrop(id_name = input$name, soilgrids_data = .)
  }, ignoreNULL = FALSE)
  
  
  output$map <- renderLeaflet({leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>% 
      setView(lng = -74, lat = 3.7, zoom = 2) #%>%
#      addDrawToolbar(editOptions = editToolbarOptions(edit=TRUE)) %>%
#      addMarkers(data = points())
      })
  

  observeEvent(input$map_click, {
    click = click()
    leafletProxy('map') %>% 
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
    
  })
  
  
############## Salidas  
  
  
  get_geo <- function(){
    click = click()
    alt = elev()
    
    tibble::tibble(Latitud = click$lat, Longitud = click$lng, Elevacion = alt) 
  }
  
  output$ubicacion <- renderTable(
    get_geo()
    )
  
  
  output$nasa_df <- DT::renderDT({
    dl_nasa() %>% dplyr::select(-c(day, month, year))
  })
  
  
  output$soil_list <- renderTable(
    dl_soilgrids() %>% . [["data"]]
  )

  output$plot_clima <- plotly::renderPlotly(
    plot_weather_series(dl_nasa() %>% rename(prec = rain), input$name) + theme(legend.position = "none")
  )

}

shinyApp(ui, server)

