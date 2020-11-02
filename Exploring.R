
# libraries ---------------------------------------------------------------
library(tidyverse)
library(gt)
library(usmap)
library(albersusa)
library(qpcR)
library(scales)
library(ggtext)

# Get functions -----------------------------------------------------------
source("setting_up_functions.R")

# Get Model Results Once --------------------------------------------------
results <- update_prob_for_viz(biden_states = "FL")


# Gauge Function ----------------------------------------------------------
Biden_Gauge_Function <- function(results){
  biden_win_prob <- results$nation$biden_win_prob
  gg.gauge(pos=round(biden_win_prob,1),determinent = round(biden_win_prob,1))
}

Biden_Gauge_Function(results)  
#ggsave("Figures/gauge.png",dpi=600)


# Table Function ----------------------------------------------------------
table_function <- function(results) {
  states <- cbind(state.name,state.abb) %>% as_tibble()
  
  table_data <- results$states %>% 
    as_tibble() %>% 
    left_join(states) %>%
    mutate(state.name=ifelse(is.na(state.name),
                             state.abb,
                             state.name)) %>% 
    arrange(desc(value)) %>% 
    mutate(value=round(value,1)) %>% 
    dplyr::select(state.name,value)
  
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

(table <- table_function(results))
#gtsave(table,"Figures/table.png")

# Map Function ------------------------------------------------------------
map_function <- function(results) {
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
  
  plot <- left_join(usa_sf(proj = "laea"),map_data) %>% 
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

map_function(results)

# Line Graph Function -----------------------------------------------------
results <- update_prob_for_viz(trump_states = "FL")
Biden_win_prob <- results$nation$biden_win_prob
trump_win_prob <- 100-results$nation$biden_win_prob

# prop_over_time <- tibble(biden_win_prob=results$nation$biden_win_prob,
#                          trump_win_prob=100-biden_win_prob,
#                          timestamp=Sys.time())

prop_over_time <- prop_over_time %>% rbind(tibble(biden_win_prob=results$nation$biden_win_prob,
                                                  trump_win_prob=100-biden_win_prob,
                                                  timestamp=Sys.time()))


prop_over_time %>% pivot_longer(cols=biden_win_prob:trump_win_prob,
                                names_to="Candidate", 
                                values_to="Win_Prob") %>% 
  ggplot() +
  geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
  scale_color_manual(values = c("#2E74C0","#CB454A")) +
  scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%a %I:%M",
                                                                        tz = "EST")) +
  labs(title=case_when(Biden_win_prob>=55~paste0("<span style='color: #2E74C0'>**Biden**</span> has a ", round(Biden_win_prob,1),"% chance to win the election"),
                       
                       Biden_win_prob<55 & Biden_win_prob>=45~paste0("It's a Tossup, <span style='color: #2E74C0'>**Biden**</span> has a ", round(Biden_win_prob,1), "% chance to win"),
                       
                       Biden_win_prob<45~paste0("<span style='color: #CB454A'>**Trump**</span> has a ", round(trump_win_prob,1),"% chance to win the election")),
       subtitle = "Win Probability") +
  theme_minimal(base_size = 12, base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(),
        plot.title = element_markdown(face = "bold", size = rel(3)),
        plot.subtitle = element_text(face = "plain", size = rel(1.5), color = "grey70"),
        axis.text = element_text(size=rel(1)),
        legend.position = "none",
        axis.title = element_blank(),
        plot.title.position = "plot")


                  
                   