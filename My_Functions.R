# libraries ---------------------------------------------------------------
library(tidyverse)
library(gt)
library(usmap)
library(albersusa)
library(qpcR)
library(scales)
library(ggtext)
library(ggimage)

# Get economist functions -----------------------------------------------------------
source("Economist_Functions.R")



# General Gauge Function --------------------------------------------------

gg.gauge <- function(pos, breaks = c(0, 33, 66, 100), determinent, Biden_win_prob, trump_win_prob) {
  # get time
  t <- Sys.time()
  t_string <- strftime(t,"%I:%M %p")
  
  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    th.start <- pi * (1 - a / 100)
    th.end   <- pi * (1 - b / 100)
    th       <- seq(th.start, th.end, length = 500)
    x        <- r1 * cos(th)
    xend     <- r2 * cos(th)
    y        <- r1 * sin(th)
    yend     <- r2 * sin(th)
    data.frame(x, y, xend, yend)
  }
  
  #caption for gauge plot
  cap <- paste("Biden win probability, last updated at", t_string)
  
  # colors from https://flatuicolors.com/palette/defo
  ggplot() + 
    geom_segment(data = get.poly(breaks[1],breaks[4]), 
                 aes(x = x, y = y, xend = xend, yend = yend, color = xend),size=2) +
    scale_color_gradientn(colors = c("#CB454A","#2E74C0")) +
    geom_segment(data = get.poly(pos - 1, pos + 1, 0.1), aes(x = x, y  =y, xend = xend, yend = yend)) +
    geom_text(data=as.data.frame(breaks), size = 5, vjust = 0,
              aes(x = 0.8 * cos(pi * (1 - breaks / 100)),  y = -0.1), label = c('Trump Wins!', '', '', "Biden Wins!"), fontface="bold") +
    annotate("text", x  = 0, y = -.1,label=paste(determinent,"%"),vjust=0,size=8,  fontface="bold")+
    coord_fixed()+
    theme_bw()+
    labs(
      title = case_when(Biden_win_prob>=90~"Overall, <span style='color: #2E74C0'>**Biden**</span> is clearly favored to win",
                        Biden_win_prob<90 & Biden_win_prob>=75~"Overall, <span style='color: #2E74C0'>**Biden**</span> is favored to win",
                        Biden_win_prob<75 & Biden_win_prob>=55~"Overall, <span style='color: #2E74C0'>**Biden**</span> is somewhat favored to win",
                        
                        Biden_win_prob<55 & Biden_win_prob>=45~"Overall, it's a tossup",
                        
                        Biden_win_prob<45 & Biden_win_prob>=25~"Overall, <span style='color: #CB454A'>**Trump**</span> is somewhat favored to win",
                        Biden_win_prob<25 & Biden_win_prob>=10~paste0("Overall, <span style='color: #CB454A'>**Trump**</span> is favored to win"),
                        Biden_win_prob<10~"Overall, <span style='color: #CB454A'>**Trump**</span> is clearly favored to win"),
      caption= cap
    ) + 
    theme(plot.title = element_markdown(face = "bold", size = rel(2), hjust = .5),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          legend.position = "none",
          axis.title = element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank(),
          plot.caption = element_text(hjust=0.5, size=rel(1.2)))
}



# Biden Gauge Function ----------------------------------------------------------
Biden_Gauge_Function <- function(results){
  biden_win_prob <- results$nation$biden_win_prob
  trump_win_prob <- 100-results$nation$biden_win_prob
  gg.gauge(pos=round(biden_win_prob,1),determinent = round(biden_win_prob,1), Biden_win_prob=biden_win_prob, trump_win_prob=trump_win_prob)
}

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
  
  
  #browser()
  table_data %>% 
    mutate(value = value / 100,
           percent = percent / 100) %>%
    #Initializing table
    gt() %>% 
    #Setting title
    tab_header(title = "Biden Win Probability by State") %>% 
    fmt_percent(columns = c(2,4), decimals = 1) %>%
    cols_align(align = "center", columns = ) %>%
    tab_options(
      data_row.padding = px(5),
      table.border.top.color = "white",
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      heading.align = "center",
      table.font.names = "Roboto Condensed",
      table.font.size = 13
    ) %>% 
    cols_label(state.name="",value="Win Probability",state="","percent"="Win Probability") %>% 
    cols_align(align = "left",
               columns = c(1,3)) %>%
    cols_align(align = "center",
               columns = c(2,4)) %>% 
    data_color(
      columns = vars(value,percent),
      colors = scales::col_numeric(
        palette = c("#CB454A", "#2E74C0"),
        domain = c(0,1)
      ))
}

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
  labs(title="Where Biden is favored to <span style='color: #2E74C0'>win</span> versus <span style='color: #CB454A'>lose</span> ") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 12) +
  theme(title=element_markdown(size=rel(1.25),face="bold", hjust = .5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
  
  
  ggplotly(plot,tooltip = "text") %>% 
    style(hoveron="fills") 
}

# # Line Graph Function -----------------------------------------------------
# results <- update_prob_for_viz(trump_states = "FL",
#                                biden_states = c("PA","MI","WI"))
# Biden_win_prob <- results$nation$biden_win_prob
# trump_win_prob <- 100-results$nation$biden_win_prob
# 
# # prop_over_time <- tibble(biden_win_prob=results$nation$biden_win_prob,
# #                          trump_win_prob=100-biden_win_prob,
# #                          timestamp=Sys.time())
# 
# prop_over_time <- prop_over_time %>% rbind(tibble(biden_win_prob=results$nation$biden_win_prob,
#                                                   trump_win_prob=100-biden_win_prob,
#                                                   timestamp=Sys.time()))
# 
# #Getting image table
# icons <- tibble(Candidate=c("biden_win_prob","trump_win_prob"),
#                 icon=c("pngs/bidentransparent.png","pngs/trumptransparent.png"))
# 
# 
# prop_over_time %>% pivot_longer(cols=biden_win_prob:trump_win_prob,
#                                 names_to="Candidate",
#                                 values_to="Win_Prob") %>%
#   group_by(Candidate) %>%
#   mutate(current_prob=tail(Win_Prob,1)) %>%
#   left_join(icons) %>%
#   ggplot() +
#   geom_line(aes(x=timestamp,y=Win_Prob,color=Candidate),size=2) +
#   geom_image(aes(x=max(timestamp),y=current_prob,image=icon), asp = 2, size = .04) +
#   scale_color_manual(values = c("#2E74C0","#CB454A")) +
#   scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%a %I:%M",
#                                                                         tz = "EST")) +
#   coord_cartesian(xlim = c(as.POSIXct("2020-11-02 18:00:00 EST"),
#                            as.POSIXct("2020-11-03 4:00:00 EST"))) +
#   labs(title=case_when(Biden_win_prob>=55~paste0("<span style='color: #2E74C0'>**Biden**</span> has a ", round(Biden_win_prob,1),"% chance to win the election"),
# 
#                        Biden_win_prob<55 & Biden_win_prob>=45~paste0("It's a Tossup, <span style='color: #2E74C0'>**Biden**</span> has a ", round(Biden_win_prob,1), "% chance to win"),
# 
#                        Biden_win_prob<45~paste0("<span style='color: #CB454A'>**Trump**</span> has a ", round(trump_win_prob,1),"% chance to win the election"))) +
#   theme_minimal(base_size = 12, base_family = "Roboto Condensed") +
#   theme(panel.grid = element_blank(),
#         plot.title = element_markdown(face = "bold", size = rel(3)),
#         plot.subtitle = element_text(face = "plain", size = rel(1.5), color = "grey70"),
#         axis.text.y = element_text(size=rel(1)),
#         axis.text.x = element_text(size=rel(.75)),
#         legend.position = "none",
#         axis.title = element_blank(),
#         plot.title.position = "plot")



