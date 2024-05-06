library(shiny)
library(crosstalk)
library(d3scatter)
library(DT)
library(leaflet)
library(ggplot2)
library(esquisse)

quakes$depth2 <- c("shallow", "mid", "deep")[cut(quakes$depth, 3, labels = F)]

ui <- fluidPage(
  tabsetPanel(   
    tabPanel(title = "Load data", 
    ),
    tabPanel(title = "Crosstalk",
             fluidRow(
               column(6, d3scatterOutput("scatter1")),
               column(6, plotOutput("plot1"))
             ),
             fluidRow(
               column(6, leafletOutput("map1")),
               column(6, DTOutput("table1"))
             )
    ),
    tabPanel(title = "Esquisser", 
             fluidRow(
               column(8, esquisse_ui(id = "esquisse", header = FALSE)),
               column(4, htmlOutput("code"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  output$scatter1 <- renderD3scatter({
    d3scatter(sd, ~mag, ~stations, width = "100%")
  })
  
  sd <- SharedData$new(quakes[1:50,])
  
  df <- debounce(reactive(sd$data(withSelection = TRUE)), 200)
  
  output$plot1 <- renderPlot({
    ggplot(df(), aes(depth2, fill = crosstalk::selection_factor(selected_))) +
      geom_bar(stat = "count") +
      crosstalk::scale_fill_selection("#555555", "#ff0000") +
      theme_classic()
  })
  
  output$map1 <- renderLeaflet({
    leaflet(sd) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      addCircleMarkers(~long, ~lat, weight = 1, radius = 5, color = "black", fillColor = "red", fillOpacity = 1)
  })
  
  output$table1 <- renderDT({
    datatable(sd)
  }, 
  server=FALSE)
  
  observeEvent(input$hej, {
    browser()
  })
  
  data_r <- reactiveValues(data = iris, name = "iris")
  
  results <- esquisse_server(
    id = "esquisse",
    data_rv = data_r
  )
}

shinyApp(ui, server)