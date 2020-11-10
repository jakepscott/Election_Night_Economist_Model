
# Loading Libs ------------------------------------------------------------
library(shiny)
library(plotly)
library(gsheet)
library(googlesheets4)
library(shinyalert)


# Loading Functions -------------------------------------------------------
source("My_Functions.R")


ui <- fluidPage(
  #Enabling shiny alerts
  useShinyalert(),
  # avoid greying out plot while recalculating
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;};
               .row {font-family: 'Roboto';}
               .container-fluid {font-family: 'Roboto';}"
  ),
  #Adding sidebar with table in it, cheeky graphical trick that did the job
  sidebarLayout(
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
  #Alert that indicates the app no longer auto updates
  shinyalert(
    title = "Warning!",
    text = "This app no longer automatically updates. It is essentially a screenshot of what the app looked like on Saturday November 7th when the race was called.",
    #size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Got it!",
    confirmButtonCol = "#2E74C0",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  #Reative Values; reading in now-static data
  Data <- reactive({
    read_rds("results_by_state.rds")
  })
  
  Called <-  reactive({
    read_rds("Called_States.rds")
  })
  
  Line_Chart <- reactive({
    read_rds("Line_Chart_Data.rds")
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


    Line_Chart() %>% 
      #Getting in tidy form for graphing
      pivot_longer(cols=biden_win_prob:trump_win_prob,
                                    names_to="Candidate",
                                    values_to="Win_Prob") %>%
      group_by(Candidate) %>%
      #Getting current probability
      mutate(current_prob=tail(Win_Prob,1)) %>%
      #joining with icon table
      left_join(icons) %>%
      ggplot() +
      geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
      geom_image(aes(x=max(timestamp),y=current_prob,image=icon), asp =3, size = .03) + #head icons
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

