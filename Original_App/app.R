# Loading Libs ------------------------------------------------------------
library(shiny)
library(plotly)
library(gsheet)
library(googlesheets4)

# Loading Functions -------------------------------------------------------
source("My_Functions.R")


ui <- fluidPage(
  # avoid greying out plot while recalculating
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;};
               .row {font-family: 'Roboto';}
               .container-fluid {font-family: 'Roboto';}"
  ),
  
  sidebarLayout(
    #Adding sidebar with table in it, cheeky graphical trick that did the job
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
  #Reative Values; reading in the data
  Data <- reactive({
    #This tells it to rerun every 10 seconds
    invalidateLater(10000)
    #This automatically reads in the data on which states have been called, every 10 seconds. I update the sheet manually
    sheet <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZPvodyP0oWZe6PqKs2UfjuXIj1c8OXZqI6FyeH-fOl4/edit?usp=sharing")
    #Getting which states were won by who
    Biden_Wins <- sheet %>% filter(State_Winner=="Biden") %>% pull(State)
    Trump_Wins <- sheet %>% filter(State_Winner=="Trump") %>% pull(State)
    #Plugging in which states were won by who into a function that recalculates win prob with the new info using the  
    #Economist model
    results <- update_prob_for_viz(biden_states = Biden_Wins,trump_states = Trump_Wins)
    results
  })
  
  Called <-  reactive({
    invalidateLater(10000)
    #Just read in the called states data every 10 seconds
    gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZPvodyP0oWZe6PqKs2UfjuXIj1c8OXZqI6FyeH-fOl4/edit?usp=sharing")
  })
  
  Line_Chart <- reactive({
    invalidateLater(10000)
    #Read in the line chart data every 10 seconds
    gsheet2tbl("https://docs.google.com/spreadsheets/d/1yQo7vkCQ7F6aMyelJvkKS32RfTGYtA8X1z8s9OKX4sU/edit#gid=1482552590") %>% 
      mutate(timestamp=as.POSIXct(timestamp))
  })
  


# Generating Table --------------------------------------------------------
  output$table <- render_gt(
    table_function(Data(), Called())
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
    #Getting properly formatted timestamp
    t <- Sys.time() %>% as.POSIXct(format(),tz="EST")
    t_string <- strftime(t,"%I:%M %p",tz = "EST")
    
    #Getting image table, with icons for Biden and Trump
    icons <- tibble(Candidate=c("biden_win_prob","trump_win_prob"),
                    icon=c("pngs/bidentransparent.png","pngs/trumptransparent.png"))

    #Getting in tidy form for graphing
    Line_Chart() %>% pivot_longer(cols=biden_win_prob:trump_win_prob,
                                    names_to="Candidate",
                                    values_to="Win_Prob") %>%
      group_by(Candidate) %>%
      mutate(current_prob=tail(Win_Prob,1)) %>%
      #joining with icon table
      left_join(icons) %>%
      ggplot() +
      geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
      geom_image(aes(x=max(timestamp),y=current_prob,image=icon), asp =3, size = .03) +
      scale_color_manual(values = c("#2E74C0","#CB454A")) +
      #Getting proper datetime axis and dodging the tick labels
      scale_x_datetime(breaks = date_breaks("3 hour"), labels = date_format("%a %I:%M",
                                                                            tz = "EST"),
                       guide = guide_axis(n.dodge=2)) +
      scale_y_continuous(breaks = seq(0,100,by = 10), labels = seq(0,100,by=10)) +
      coord_cartesian(ylim=c(0,100)) +
      labs(title="How win probability has changed over time",
           subtitle = paste0("Updated every 2 minutes, may differ temporarily from needle. Last updated: ", t_string)) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.title = element_markdown(face = "bold", size = rel(3),hjust=.5),
            plot.subtitle = element_text(face = "plain", size = rel(1.5), color = "grey70", hjust =.5),
            axis.text.y = element_text(size=rel(1.2)),
            axis.text.x = element_text(size=rel(1.2)),
            legend.position = "none",
            axis.title = element_blank(),
            plot.title.position = "plot")
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

