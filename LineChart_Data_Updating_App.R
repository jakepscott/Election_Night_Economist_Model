library(shiny)
library(googlesheets4)
library(gsheet)
setwd("C:/Users/Jake Scott/Desktop/R Projects/Election_Night_Economist_Model")
source("My_Functions.R")

ui <- fluidPage(
  print("Hi!")
)

server <- function(input, output, session) {
  observe({
    invalidateLater(10000)
    sheet <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZPvodyP0oWZe6PqKs2UfjuXIj1c8OXZqI6FyeH-fOl4/edit?usp=sharing")
    
    if(nrow(sheet)>0) {
      Biden_Wins <- sheet %>% filter(State_Winner=="Biden") %>% pull(State)
      Trump_Wins <- sheet %>% filter(State_Winner=="Trump") %>% pull(State)
      results <- update_prob_for_viz(biden_states = Biden_Wins,trump_states = Trump_Wins)
    } else {
      results <- update_prob_for_viz()
    }
    
    prop_over_time <- tibble(biden_win_prob=results$nation$biden_win_prob,
                             trump_win_prob=100-biden_win_prob,
                             timestamp=Sys.time())
    "https://docs.google.com/spreadsheets/d/1yQo7vkCQ7F6aMyelJvkKS32RfTGYtA8X1z8s9OKX4sU/edit?usp=sharing" %>% 
      sheet_append(prop_over_time)
  })
}

shinyApp(ui, server)