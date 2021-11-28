#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)

library(shiny)

library(ggplot2)

library(babynames)

#install.packages("DT")
library(DT)

library(plotly)

#install.packages("shinythemes")
library(shinythemes)

usa_ufo_sightings <- read.csv("usa_ufo_sightings.csv")

server <- function(input, output) {
  
  ufo_help_text <- paste("Esta aplicação analisa os avistamentos de OVNI's ocorridos nos EUA. ",
                         "Selecione um estado americano, um intervalo de datas e clique em <Aplicar>.",
                         "Os dados serão exibidos nas abas <Grafico> e <Tabela>")
  
  observeEvent(input$show_help, {
    showModal(modalDialog(ufo_help_text))
  })
  
  plot_sightings_by_shape <- eventReactive(input$aplicar, {
    usa_ufo_sightings %>% 
      filter(state == input$state) %>%
      filter(date_sighted >= as.Date(input$dates[1], origin = "1970-01-01") 
             & date_sighted <= as.Date(input$dates[2], origin = "1970-01-01")) %>% 
      ggplot(aes(x = shape, fill = shape)) +
      geom_bar() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 30))
  })
  
  table_sightings_by_shape <- eventReactive(input$aplicar, {
    usa_ufo_sightings %>% 
      filter(state == input$state) %>%
      filter(date_sighted >= as.Date(input$dates[1], origin = "1970-01-01") 
             & date_sighted <= as.Date(input$dates[2], origin = "1970-01-01")) %>% 
      group_by(shape) %>%
      summarize(nr_avistamentos = n(), media_duracao_min = mean(duration_sec)/60,
                mediana_duracao_min = median(duration_sec)/60, min_duracao_min = min(duration_sec)/60, 
                max_duracao_min = max(duration_sec)/60)
  })
  
  output$shapes <- plotly::renderPlotly({
    plot_sightings_by_shape()
  })
  
  output$duration_table <- DT::renderDT({
    table_sightings_by_shape()
  })
  
}

ui <- fluidPage(
  titlePanel("Avistamentos de OVNIS nos EUA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Estado americano:", choices = unique(usa_ufo_sightings$state)),
      
      # dateRangeInput("dates", "Choose a date range:",
      #                start = "1920-01-01",
      #                end = "1950-01-01")
      
      dateRangeInput("dates", "Intervalo de datas:", 
                     start = "2001-01-01", 
                     end = format(Sys.time(), "%Y-%m-%d"), 
                     format = "dd-mm-yyyy"),
      
      actionButton("aplicar", "Aplicar"),
      
      actionButton("show_help", " ? ")
      
    ),
    mainPanel(
      tabsetPanel(
        # Add plot output named 'shapes'
        tabPanel("Grafico",
                 plotly::plotlyOutput('shapes')
        ),
        # Add table output named 'duration_table'
        tabPanel("Tabela",
                 DT::DTOutput('duration_table')
        )
      )
    )
  ),
  hr(),
  print("Ricardo Ortegal"),
  br(),
  print("ricardo.ortegal@gmail.com"),
  br()
  #,tags$img(src='D:/Ortegal/R/UFO_Sights/whatsapp.png'),
  #print('+5561999034633')
)

shinyApp(ui, server)