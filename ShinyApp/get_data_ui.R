# Script app - AgroclimR - Download Test Data.
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/agroclimR
# 2021



### 1. Cargar requerimientos
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/utils/utils_crop_model.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/agroclim_forecaster.R", encoding = "UTF-8")
load_agroclimr_requeriments()
inpack(c("tidyverse", "data.table", "lubridate", "sirad", "naniar", "jsonlite" ,"soiltexture", "Hmisc", "parallel", 
         "shiny", "leaflet"))



ui <- fluidPage(
  titlePanel("AgroclimR - Download Test Data"),
#  shinythemes::shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Ingrese un nombre: (Sin separadores)", "LOCATION"),
#      numericInput("lon", label = h3("Longitude:"), value = -74),
#      numericInput("lat", label = h3("Latitude:"), value = 3.7),
#      numericInput("alt", label = h3("Elevacion:"), value = 1000),
#      checkboxInput("elevation_box", "Estimar Elevacion?"),
      tableOutput('ubicacion'),
#      actionButton("recalc", "Seleccionar ubicacion"),
      dateInput('ini_date',
          label = 'Fecha Inicial: yyyy-mm-dd',
          value = Sys.Date() - months(12)
),
      dateInput('end_date',
          label = 'Fecha Final: yyyy-mm-dd',
          value = Sys.Date() - months(1)
),
##cuadro dialogo 
      actionButton("dlnasa", "Descarga NASAPOWER"),   
      actionButton("dlsoilgrids", "Descarga SOILGRIDS"),
      selectInput('op_format', 'Elija un formato de descarga', 
                  choices = c('csv', 'AquaCrop', 'ORYZA_V3', 'DSSAT')),
      downloadButton("dldata", "Download")

#      actionButton("recalc", "Show point")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Mapa', leafletOutput('map')),
        tabPanel('Datos Clima', DT::DTOutput('nasa_df')),
        tabPanel('Datos Suelo', tableOutput('soil_list')),
#        tabPanel('', sliderInput()),
        tabPanel('Climatologia', plotOutput('plot_clima'))
  ))
)
  
)

server <- function(input, output, session) {
  

  #### Reactive process
  
  click <- reactive({
    input$map_click 
  })
  
  elev <- reactive({
    req(input$map_click)
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
    get_data_soilgrids(click$lat, click$lng) #%>%
#      from_soilgrids_to_aquacrop(id_name = input$name, soilgrids_data = .)
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
  
#  output$clim_summary <- renderTable(
#    dl_nasa()
#  )
  
  output$soil_list <- renderTable(
    dl_soilgrids() %>% unnest(data) %>% 
      select(var, range, label, values) %>% flatten() %>%
      #    set_names(c("var", "tdepth","bdepth", "unit", "label", "value")) %>%
      pivot_wider(names_from = var, values_from = values.mean) %>% 
      mutate_at(.vars = vars(bdod, cfvo, clay, sand, silt), ~.x/10) %>%
      mutate(bdod = bdod/10) %>%
      select(label, bdod:soc) %>%
      select(label, bdod:soc) %>% 
      set_names(c('Layer', 'Densidad Aparente (g/cm3)', 'Grava (%)', 'Arcilla (%)', 'Nitrogeno (g/kg)', 'Arena (%)', 'Limo (%)', 'Carbono Organico (g/kg)'))
      # %>%. [["data"]]
  )

  output$plot_clima <- renderPlot(
    plot_weather_series(dl_nasa() %>% rename(prec = rain), input$name) 
  )
  
  ###### Save info
  
#  output$dldata <- downloadHandler(
#    filename = function() {
#      paste0(input$name, "_agroclimr_input", ".zip")
#    }
#    
#    content = function()
#    
#  )
  
 
    
    
     
  
  

}

shinyApp(ui, server)

