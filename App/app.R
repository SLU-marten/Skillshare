library(shiny)
library(crosstalk)
library(d3scatter)
library(DT)
library(leaflet)
library(esquisse)
library(shinythemes)

sd <- SharedData$new(quakes)

ui <- navbarPage(
  title = "Skillshare",
  theme = shinytheme("cerulean"),
  tabPanel(title = "Demo data", 
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot")
             )
           )
  ),
  tabPanel(title = "Crosstalk",
           column(3,
                  filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1),
                  filter_slider("depth", "Depth", sd, column=~depth, step=10),
                  filter_slider("stations", "Stations", sd, column=~stations, step=10)
           ),
           column(9,
                  fluidRow(
                    column(6, d3scatterOutput("scatter1")), 
                    column(6, leafletOutput("map1")),
                  ),
                  fluidRow(
                    column(12, DTOutput("table1"))
                  )
           )
  ),
  tabPanel(title = "Esquisser", 
           fluidRow(
             column(8, esquisse_ui(id = "esquisse", header = FALSE)),
             column(4, htmlOutput("code"))
           )
  )
)

server <- function(input, output, session) {
  
  #### DEMO ####
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  
  #### CROSSTALK ####
  output$scatter1 <- renderD3scatter({
    d3scatter(sd, ~mag, ~stations, width = "100%")
  })

  output$map1 <- renderLeaflet({
    leaflet(sd) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      addCircleMarkers(~long, ~lat,
                       weight = 1, radius = 6, 
                       color = "black", fillColor = "red",
                       fillOpacity = 1)
  })
  
  output$table1 <- renderDT({
    datatable(sd)
  }, server = FALSE)

  
  #### ESQUISSER ####
  data_r <- reactiveValues(data = iris, name = "iris")
  
  results <- esquisse_server(
    id = "esquisse",
    data_rv = data_r
  )
}

shinyApp(ui, server)