library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE),
                verbatimTextOutput("clickInfo")
  )
)

server <- function(input, output, session) {
  
  #########################################################
  
  data <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,
               {data$clickedMarker <- input$map_marker_click})
  
  observeEvent(input$map_click,
               {data$clickedMarker <- NULL})
  
  output$clickInfo <- renderPrint({data$clickedMarker})
  
  ##########################################################
  
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircleMarkers(radius = ~mag^2/3, weight = 1, color = "#777777",
                       fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  observe({
    proxy <- leafletProxy("map", data = quakes)
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)




library(shiny)
library(leaflet)
library(leaflet.extras)
library(rvest)

fa_list <- read_html("http://astronautweb.co/snippet/font-awesome/") %>% 
  html_nodes("span.icon-name") %>% 
  html_text()
fa_pretty <- gsub("^fa-", "", fa_list)
# Awesome-icon markers only support the colors below...
fa_cols <- c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", 
             "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", 
             "pink", "cadetblue", "white", "gray", "lightgray", "black")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.1/css/font-awesome.min.css")
  ),
  tags$style(type = "text/css", "#map {height: calc(100vh - 20px)!important;}"),
  fluidRow(
    splitLayout(cellArgs = list(style = "overflow: visible;"),
                shinyWidgets::pickerInput("defaultIcon", "Default Marker", choices = fa_pretty, 
                                          options = shinyWidgets::pickerOptions(liveSearch = TRUE),
                                          choicesOpt = list(icon = paste("fa", fa_list), 
                                                            iconBase = "fontawesome")),
                colourpicker::colourInput("defaultColor", "Default icon color"),
                colourpicker::colourInput("defaultBg", "Default marker color", palette = "limited", 
                                          allowedCols = fa_cols, returnName = TRUE, value = "red")
    ),
    tags$div( tags$b("Place Marker"), 
              shinyWidgets::switchInput("edit_mode", "Edit Mode", 
                                        onLabel = "Click on the map to add a marker"))
  ),
  leafletOutput("map")
)

server <- function(input,output,session){
  react_list <- reactiveValues()
  # While the user has toggled the edit-mode input, register any future map-clicks
  # as reactive values.
  observe({
    if (input$edit_mode & !isTRUE(input$map_click$.nonce == react_list$nonce)) {
      react_list$mapEditClick <- input$map_click
    }
    react_list$nonce <- input$map_click$.nonce
  })
  
  output$map <- renderLeaflet(
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMeasure(
        primaryLengthUnit = "kilometers",
        secondaryAreaUnit = FALSE) %>%
      setView(lat = 45, lng = 9, zoom = 3)
  )
  # When a user clicks on the map while being in edit-mode, place a marker with
  # the chosen icon, color and marker-color at the click coordinates.
  observeEvent(react_list$mapEditClick, {
    leafletProxy("map") %>% 
      addAwesomeMarkers(
        lng     = react_list$mapEditClick$lng, 
        lat     = react_list$mapEditClick$lat,
        layerId = as.character(react_list$mapEditClick$.nonce),
        icon    = makeAwesomeIcon(icon     = input$defaultIcon, 
                                  library     = "fa", 
                                  iconColor   = input$defaultColor, 
                                  markerColor = input$defaultBg),
        label = "Click to delete", 
        labelOptions = labelOptions(TRUE))
  })
  # Delete the marker when it has been clicked.
  observeEvent(input$map_marker_click, {
    leafletProxy("map") %>%
      removeMarker(as.character(input$map_marker_click$id))
  })
}

shinyApp(ui,server)


mt_wash <- data.frame(x = -105, y = 45)

a <- get_elev_point(locations = mt_wash, prj = "EPSG:4326")
a$elevation


text=c("Line1", "Line2","Line3")

ui <- fluidPage(
  
  sidebarPanel(
    h4("Title"),
    p("Subtitle",
      br(),text[1],
      br(),text[2],
      br(),text[3]),
    textInput("filename", "Input a name for the file", value = paste0("data-", Sys.Date(),".txt")),
    downloadButton("download_button", label = "Download")
  )
)

server <- function(input, output, session){
  
  output$download_button <- downloadHandler(
    filename = function(){
      input$filename
    },
    content = function(file) {
      writeLines(paste(text, collapse = ", "), file)
    }
  )
}

shinyApp(ui,server)
