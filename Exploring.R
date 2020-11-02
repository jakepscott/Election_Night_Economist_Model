
# libraries ---------------------------------------------------------------
library(tidyverse)
library(gt)
library(usmap)
library(albersusa)
library(qpcR)
library(scales)

# Get functions -----------------------------------------------------------
source("setting_up_functions.R")


# Gauge Function ----------------------------------------------------------
Biden_Gauge_Function <- function(d_states=NULL,r_states=NULL){
  results <- update_prob_for_viz(biden_states = d_states, trump_states = r_states)
  biden_win_prob <- results$nation$biden_win_prob
  
  gg.gauge(pos=round(biden_win_prob,1),determinent = round(biden_win_prob,1))
}

Biden_Gauge_Function()  


# Table Function ----------------------------------------------------------
table_function <- function(d_states=NULL,r_states=NULL) {
  results <- update_prob_for_viz(biden_states = d_states,
                                 trump_states = r_states)
  
  states <- cbind(state.name,state.abb) %>% as_tibble()
  
  table_data <- results$states %>% 
    as_tibble() %>% 
    left_join(states) %>%
    mutate(state.name=ifelse(is.na(state.name),
                             state.abb,
                             state.name)) %>% 
    arrange(desc(value)) %>% 
    mutate(value=round(value,1)) %>% 
    select(state.name,value)
  
  top <- table_data %>% head(nrow(table_data)/2)
  bottom<- table_data %>% tail(nrow(table_data)/2) %>% rename("state"=state.name,"percent"=value)
  table_data <- cbind(top,bottom)
  
  table_data %>% 
    #Initializing table
    gt() %>% 
    #Setting title
    tab_header(title = "Biden Win Probability by State") %>% 
    tab_options(
      table.border.top.color = "white",
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      heading.align = "center",
      table.font.names = "Roboto Condensed"
    ) %>% 
    cols_label(state.name="",value="Win Probability",state="","percent"="Win Probability") %>% 
    cols_align(align = "left",
               columns = c(1,2)) %>%
    cols_align(align = "right",
               columns = c(3,4)) %>% 
    data_color(
      columns = vars(value,percent),
      colors = scales::col_numeric(
        palette = c("#CB454A", "#2E74C0"),
        domain = c(0,100)
      ))
}

table_function()

# Map Function ------------------------------------------------------------
map_function <- function(d_states=NULL,r_states=NULL) {
  results <- update_prob_for_viz(biden_states = d_states,
                                 trump_states = r_states)
  
  states <- cbind(state.name,state.abb) %>% as_tibble()
  
  map_data <- results$states %>% 
    as_tibble() %>% 
    left_join(states) %>%
    mutate(state.name=ifelse(is.na(state.name),
                             state.abb,
                             state.name)) %>% 
    mutate(value=round(value,1),
           state=tolower(state.name)) %>% 
    mutate(label=paste0("Biden Win Probability: ", value, "%")) %>% 
    mutate(state.name=ifelse(state.name=="DC","District of Columbia",state.name)) %>% 
    dplyr::select(name=state.name,value) %>% 
    mutate(label=paste0("Biden Win Probability: ", value, "%"))
  
  plot <- left_join(usa_sf(),map_data) %>% 
    ggplot() +
    geom_sf(aes(fill=value,text=label)) +
    scale_fill_gradient(low = "#CB454A",
                        high = "#2E74C0")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
  
  
  ggplotly(plot,tooltip = "text") %>% 
    style(hoveron="fills") 
}

map_function(d_states = "TX")


# Line Graph Function -----------------------------------------------------

time <- seq(from = as.POSIXct("2020-11-01 06:00:00", format="%Y-%m-%d %H:%M:%S"),
                 to = Sys.time(),
                 by = "min") %>% as_tibble()

biden_win <- c(rnorm(100,0,10),rep(NA,nrow(seq(from = as.POSIXct("2020-11-01 06:00:00", format="%Y-%m-%d %H:%M:%S"),
                                             to = Sys.time(),
                                             by = "min") %>% as_tibble())-100))
test <- cbind(time,biden_win) %>% as_tibble() %>% rename("biden"=biden_win,
                                                         "time_axis"=value)

ggplot(test) + 
  geom_line(aes(x=time_axis,y=biden)) +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H"),
                   date_breaks = "1 hour") +
  theme(axis.text.x = element_text(angle = 90))
                  
                   