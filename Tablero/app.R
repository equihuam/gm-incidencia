# install latest r-plotly dev version:
# devtools::install_github("ropensci/plotly")

library(plotly)
library(shinyjs)
library(readxl)
library(shinythemes)
library(sf)
library(leaflet)
library(leaflet.extras)
library(stringr)
#setwd("oh_my_gits/sunburst/")
edos = st_read("00ent(1).json") 

pronas = read_xlsx("Proyectos-Pronacess-SSyS.xlsx") |>
  janitor::clean_names() |> 
  mutate(colapser = gsub(", ", "|", estado)) |>
  mutate(nombrecorto = str_wrap(nombrecorto, 8))



ui <- fluidPage(
  tags$img(src = "background.jpeg", style = "position: absolute"),
  theme = shinytheme("darkly"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(tags$style(HTML("hr {border-top: 1px solid #000000;}
                .well {height: 1200px;}")),
    fluidRow(column(12,
      HTML("<div style='height: 90px; text-align: center; font-size: 20px;'>"),
      tags$a("Proyecto: "),   tags$b(htmlOutput("clickDataOut")), tags$br() ),
      HTML("</div>")
    ),
   fluidRow(   
       column(12,
              HTML("<div style='height: 220px; text-align: center;'>"),
             imageOutput("fotito"),
              HTML("</div>")
      )
    ),
   fluidRow(align = "center",
            HTML("<div style='height: 50px; text-align: center; font-size: 20px;'>"),
                  tags$b("Institución:"),
   HTML("</div>"),   
     column(12,
            HTML("<div style='height: 50px; width: 100px; text-align: center; background-color:orange;'>"),
            imageOutput("loguito"),
            HTML("</div>")
     )
   ),
    div(tags$br()),
    fluidRow(   
      column(12,
             HTML("<div style='text-align: center;'>"),
             textOutput("textito"),
             HTML("</div>")
      )
    ),
    div(tags$br()),
    fluidRow(align = "center",
      column(12,
             HTML("<div style='height: 100px;'>"),
             leafletOutput("map", width = "80%", height = "250px"),
             HTML("</div>")
      )
    )
  ),
  mainPanel(tags$style(HTML(".plot-container.plotly {background:#00000000}")),
  plotlyOutput("sunburst", height = "800px"),
  htmlOutput("hoverDataOut")
  )
  )
)

server <- function(input, output, session) {
  temp <- reactive({
    edos %>%
      dplyr::filter(grepl(ifelse(length(clickData()[3]) == 0,
                                        "Jalisco", 
                                        pronas$colapser[pronas$ids ==  clickData()[3]]), NOMGEO))
      })  
  output$map = renderLeaflet( { 
    leaflet() |> 
      setView(lng = -98.884, lat = 20.87757, zoom = 6) |>
      addTiles() |>
      #addProviderTiles(provider = providers$Stamen.Watercolor) |>
      addPolygons(data = temp(),
                  popup =  ~paste0("<b>Proyecto: </b>", NOMGEO,"<br>"), 
                  fillColor = "magenta", color = "magenta",
                  group = "Proyectos",
                  highlightOptions = highlightOptions(color = "gold", fillColor = "gold")) 
  })
  observe({
    proyis = temp()
    proyisbb = st_bbox(proyis)
    leafletProxy("map", data = proyis  ) |>
      flyToBounds(proyisbb[[1]], proyisbb[[2]], proyisbb[[3]], proyisbb[[4]]) |>
#     flyTo(st_coordinates(st_centroid(proyis))[1], st_coordinates(st_centroid(proyis))[2], 8) |>
      addPolygons(stroke = FALSE ) #|>
      #leaflet::addLayersControl(overlayGroups = c("Municipios", "Proyectos"),
      #                          options = layersControlOptions(collapsed = FALSE) )
  })
  
  
  output$sunburst <- renderPlotly({
    plot_ly(data = pronas, source = "sunSource", customdata = ~ids, 
            ids = ~ids, labels= ~nombrecorto, parents = ~parents, 
            #values= ~values, 
            type='sunburst', branchvalues = 'total') %>%
      layout(
             paper_bgcolor='rgba(0,0,0,0',
             plot_bgcolor='rgba(0,0,0,0)')
    
    
  })
  
#  hoverData <- reactive({
#    currentEventData <- unlist(event_data(event = "plotly_hover", source = "sunSource", priority = "event"))
#  })
  
  clickData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_sunburstclick", source = "sunSource", priority = "event"))
  })
  
#  output$hoverDataOut <- renderText({
#    paste("Hover data:", paste(names(hoverData()), unlist(hoverData()), sep = ": ", collapse = " | "))
#  })
  
  output$clickDataOut <- renderText({
    gsub(".*- ", "", paste(clickData()[3]))
  })
  
  output$textito <- renderText({
    ifelse(length(clickData()) == 0, 
           "Explorador de los proyectos PRONAI del Consejo Nacional de Humanidades Ciencia y Tecnología",
                  pronas$texto[pronas$ids == clickData()[3] ] )
  }) 
  output$fotito = renderImage({
    # en el servidor sí pide www si los lee así (no como las otras apps que he hecho)
    fileimg = pronas$nombrecorto[pronas$ids == clickData()[3]] |> iconv(to  = "ASCII//TRANSLIT")
    filename = sprintf("www/foto_%s.jpeg", gsub(" |\n", "_", tolower(fileimg)))
    list(src = ifelse(length(filename) == 0, "www/inecol.jpg", filename ),
         width = "80%", height = "220px" )
  }, deleteFile = FALSE)
  output$loguito = renderImage({
    # en el servidor sí pide www si los lee así (no como las otras apps que he hecho)
    filelogo =  pronas$logo[pronas$ids == clickData()[3]]
    filename = paste0("www/logo_", filelogo, ".png")
    list(src = ifelse(length(filelogo) == 0, "www/logo_inecol.png", filename ),
         width = "100%", height = "50px" )
  }, deleteFile = FALSE)
  
  }


shinyApp(ui, server)
