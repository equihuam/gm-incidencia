library(plotly)
library(shinyjs)
library(shiny)
library(stringi)
library(tidyverse)

DF <- structure(list(labels = c("total", "A", "C", "B", "F", "E", "F", "E", "D", "E", "D", 
                                "D", "F", "G", "H", "H", "G", "H", "G", "H", "H", "G", "I", 
                                "G", "I", "H", "G", "I", "I", "G", "G", "I", "H", "H", "I",
                                "I", "I", "H", "I", "G"),
                     values = c(100L, 36L, 29L, 35L, 12L, 14L, 10L, 14L, 8L, 18L, 5L, 10L, 
                                9L, 6L, 5L, 4L, 3L, 7L, 4L, 2L, 6L, 3L, 8L, 3L, 2L, 4L, 4L,
                                4L, 4L, 4L, 3L, 2L, 5L, 5L, 2L, 2L, 1L, 1L, 1L, 5L),
                     parents = c(NA, "total", "total", "total", "total - A", "total - C", 
                                 "total - C", "total - A",
                                 "total - B", "total - B", "total - C", "total - A", 
                                 "total - B", "total - A - F", "total - C - E", "total - C - F", 
                                 "total - A - E", "total - A - E", "total - B - D", 
                                 "total - B - D", "total - B - E", "total - C - D", 
                                 "total - B - E", "total - A - D", "total - B - D", 
                                 "total - B - F", "total - C - F", "total - A - E", 
                                 "total - C - E", "total - B - E", "total - B - F", 
                                 "total - A - D", "total - A - D", "total - A - F", 
                                 "total - B - F", "total - C - F", "total - C - D",
                                 "total - C - D", "total - A - F", "total - C - E"),
                     ids = c("total", "total - A", "total - C", "total - B", "total - A - F", 
                             "total - C - E", "total - C - F", "total - A - E", "total - B - D", 
                             "total - B - E", "total - C - D", "total - A - D", "total - B - F", 
                             "total - A - F - G", "total - C - E - H", "total - C - F - H", 
                             "total - A - E - G", "total - A - E - H", "total - B - D - G", 
                             "total - B - D - H", "total - B - E - H", "total - C - D - G",
                             "total - B - E - I","total - A - D - G", "total - B - D - I", 
                             "total - B - F - H", "total - C - F - G", "total - A - E - I", 
                             "total - C - E - I", "total - B - E - G", "total - B - F - G",
                             "total - A - D - I", "total - A - D - H", "total - A - F - H", 
                             "total - B - F - I", "total - C - F - I", "total - C - D - I", 
                             "total - C - D - H", "total - A - F - I", "total - C - E - G" )), 
                row.names = c(NA,-40L), class = "data.frame")

link_txt <- "<a href='https://gm-incidencia.netlify.app'>text - Visita nuestro blog!</a>"

DF$ids[grepl("total( - [A-Z]){3}", DF$ids)] <- stri_replace_all_regex(link_txt, "text",
                                               DF$ids[grepl("total( - [A-Z]){3}", DF$ids)])

ui <- fluidPage(useShinyjs(),
                plotlyOutput("sunburst"),
                htmlOutput("hoverDataOut"),
                htmlOutput("clickDataOut"))

server <- function(input, output, session) {
  output$sunburst <- renderPlotly({
    plot_ly(data = DF, source = "sunSource", customdata = ~ids, ids = ~ids, labels= ~labels, 
            parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')})
  
  hoverData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_hover", source = "sunSource", 
                                          priority = "event"))})

  
  clickData <- reactiveVal()
  
  observe({clickData(unlist(event_data(event = "plotly_click", source = "sunSource",
                                       priority = "event")))})
  
  # workaround:
  # onclick(id = "sunburst", expr = {clickData(hoverData())})
  
  output$hoverDataOut <- renderText({paste("Hover data:",
                                           paste(names(hoverData()), 
                                                 unlist(hoverData()), sep = ": ",
                                                 collapse = " | "))})
  
  output$clickDataOut <- renderText({
    paste("Click data:", paste(names(clickData()), unlist(clickData()), sep = ": ", 
                               collapse = " | "))
  })
  
}

shinyApp(ui, server)


library(tidyr)


# Jerarquía de conceptos como Json 

myfile <- 
  conceptos %>%
  select(tema, name = concepto, proyecto = id_p, value = id) %>% 
  group_by(tema) %>% 
  nest() %>% 
  rename(children = data, name = tema) %>%
  mutate(children = map(children, transpose))

mydata <- jsonlite::toJSON(myfile, pretty = TRUE)
write(mydata, "./posts/gs-sqlite-contenido/data/flare-3.json")


nest_by(myfile$children, .key = "proyecto")


test <- myfile %>% as_tibble %>% transpose()
test_js <- jsonlite::toJSON(test, pretty = TRUE)

