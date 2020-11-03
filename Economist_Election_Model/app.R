library(shiny)
library(plotly)
library(gsheet)
library(googlesheets4)

#setwd("C:/Users/Jake Scott/Desktop/R Projects/Election_Night_Economist_Model")
source("My_Functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # avoid greying out plot while recalculating
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;};
               .row {font-family: 'Roboto';}
               .container-fluid {font-family: 'Roboto';}"
  ),
  
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(width = 4,
                 tags$style(".well {background-color:[white];}"),
                 gt_output("table")
    ),
    
    mainPanel(width=8,
              plotOutput("lineplot"),
              fluidRow(
                column(6,
                       plotOutput("gauge")
                ),
                column(6,
                       plotlyOutput("map"))
              )
    )
  )
)


# Server Side -------------------------------------------------------------
server <- function(input, output) {
  #Reative Values
  Data <- reactive({
    invalidateLater(10000)
    sheet <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZPvodyP0oWZe6PqKs2UfjuXIj1c8OXZqI6FyeH-fOl4/edit?usp=sharing")
    Biden_Wins <- sheet %>% filter(State_Winner=="Biden") %>% pull(State)
    Trump_Wins <- sheet %>% filter(State_Winner=="Trump") %>% pull(State)
    results <- update_prob_for_viz(biden_states = Biden_Wins,trump_states = Trump_Wins)
    results
  })
  
  Line_Chart <- reactive({
    invalidateLater(10000)
    gsheet2tbl("https://docs.google.com/spreadsheets/d/1yQo7vkCQ7F6aMyelJvkKS32RfTGYtA8X1z8s9OKX4sU/edit#gid=1482552590") %>% 
      mutate(timestamp=as.POSIXct(timestamp))
  })
  


# Generating Table --------------------------------------------------------
  output$table <- render_gt(
    table_function(Data())
  )
  

# Generating Map ----------------------------------------------------------
  output$map <- renderPlotly(
    map_function(Data())
  )

# Generating Gauge --------------------------------------------------------
  output$gauge <- renderPlot(
    Biden_Gauge_Function(Data())
  )

# Generating Line Plot ----------------------------------------------------
  output$lineplot <- renderPlot({
    #Getting image table
    icons <- tibble(Candidate=c("biden_win_prob","trump_win_prob"),
                    icon=c("pngs/bidentransparent.png","pngs/trumptransparent.png"))


    Line_Chart() %>% pivot_longer(cols=biden_win_prob:trump_win_prob,
                                    names_to="Candidate",
                                    values_to="Win_Prob") %>%
      group_by(Candidate) %>%
      mutate(current_prob=tail(Win_Prob,1)) %>%
      left_join(icons) %>%
      ggplot() +
      geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
      geom_image(aes(x=max(timestamp),y=current_prob,image=icon), asp = 2, size = .04) +
      scale_color_manual(values = c("#2E74C0","#CB454A")) +
      scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%a %I:%M",
                                                                            tz = "EST")) +
      # coord_cartesian(xlim = c(as.POSIXct("2020-11-03 18:00:00 EST"),
      #                          as.POSIXct("2020-11-04 4:00:00 EST"))) +
      labs(title="How win probability has changed over time") +
      theme_minimal(base_size = 12, base_family = "Roboto Condensed") +
      theme(panel.grid = element_blank(),
            plot.title = element_markdown(face = "bold", size = rel(3)),
            plot.subtitle = element_text(face = "plain", size = rel(1.5), color = "grey70"),
            axis.text.y = element_text(size=rel(1)),
            axis.text.x = element_text(size=rel(.75)),
            legend.position = "none",
            axis.title = element_blank(),
            plot.title.position = "plot")
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

