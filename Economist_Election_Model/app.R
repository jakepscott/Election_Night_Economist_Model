library(shiny)
library(plotly)
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Reative Values
  reactives <- reactiveValues()
  reactives$results <- update_prob_for_viz()


# Generating Table --------------------------------------------------------
  output$table <- render_gt(
    table_function(reactives$results)
  )
  

# Generating Map ----------------------------------------------------------
  output$map <- renderPlotly(
    map_function(reactives$results)
  )

# Generating Gauge --------------------------------------------------------
  output$gauge <- renderPlot(
    Biden_Gauge_Function(reactives$results)
  )

# Generating Line Plot ----------------------------------------------------
  output$lineplot <- renderPlot({
    Biden_win_prob <- reactives$results$nation$biden_win_prob
    trump_win_prob <- 100-reactives$results$nation$biden_win_prob

    prop_over_time <- tibble(biden_win_prob=reactives$results$nation$biden_win_prob,
                             trump_win_prob=100-biden_win_prob,
                             timestamp=Sys.time())

    prop_over_time <- prop_over_time %>% rbind(tibble(biden_win_prob=reactives$results$nation$biden_win_prob,
                                                      trump_win_prob=100-biden_win_prob,
                                                      timestamp=Sys.time()))

    #Getting image table
    icons <- tibble(Candidate=c("biden_win_prob","trump_win_prob"),
                    icon=c("pngs/bidentransparent.png","pngs/trumptransparent.png"))


    prop_over_time %>% pivot_longer(cols=biden_win_prob:trump_win_prob,
                                    names_to="Candidate",
                                    values_to="Win_Prob") %>%
      group_by(Candidate) %>%
      mutate(current_prob=tail(Win_Prob,1)) %>%
      left_join(icons) %>%
      ggplot() +
      geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
      geom_image(aes(x=max(timestamp),y=current_prob,image=icon), asp = 2, size = .04) +
      scale_color_manual(values = c("#2E74C0","#CB454A")) +
      scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%a %I:%M",
                                                                            tz = "EST")) +
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

