library(shiny)
setwd("C:/Users/Jake Scott/Desktop/R Projects/Election_Night_Economist_Model")
source("My_Functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #dec4de;
        }'))),
  
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(width = 4,
                 tags$style(".well {background-color:[white];}"),
                 gt_output("table")
    ),
    
    mainPanel(width=8,
              plotlyOutput("map"),
              fluidRow(
                column(6,
                       plotOutput("gauge")
                )
              ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Reative Values
  reactives <- reactiveValues()
  reactives$results <- update_prob_for_viz()


# Generating Table --------------------------------------------------------
  output$table <- render_gt(
    table_function(results)
  )
  

# Generating Map ----------------------------------------------------------
  output$map <- renderPlotly(
    map_function(results)
  )

# Generating Gauge --------------------------------------------------------
output$gauge <- renderPlot(
  Biden_Gauge_Function(results)
)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

