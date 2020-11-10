# libraries ---------------------------------------------------------------
library(tidyverse)
library(gt)
library(usmap)
#library(albersusa) technically used this to get the map data, but Shiny hates this package:(
library(qpcR)
library(scales)
library(ggtext)
library(ggimage)
library(readr)
library(sf)

# Get economist functions -----------------------------------------------------------
source("Economist_Functions.R")



# General Gauge Function --------------------------------------------------
#All this code comes from this thread: https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r

gg.gauge <- function(pos, breaks = c(0, 33, 66, 100), determinent, Biden_win_prob, trump_win_prob) {
  # get time
  t <- Sys.time() %>% as.POSIXct(format(),tz="EST")
  t_string <- strftime(t,"%I:%M:%S %p",tz = "EST")
  
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
  cap <- paste("Biden win probability, last updated at", t_string, "EST")
  
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
      #Making title dependent on win probability
      title = case_when(Biden_win_prob==100~"<span style='color: #2E74C0'>**Biden**</span> has won the 2020 election",
                        Biden_win_prob<100 & Biden_win_prob>=90~"Overall, <span style='color: #2E74C0'>**Biden**</span> is clearly favored to win",
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
#Basically a wrapper for the generage gauge function
Biden_Gauge_Function <- function(results){
  biden_win_prob <- results$nation$biden_win_prob
  trump_win_prob <- 100-results$nation$biden_win_prob
  gg.gauge(pos=round(biden_win_prob,1),determinent = round(biden_win_prob,1), Biden_win_prob=biden_win_prob, trump_win_prob=trump_win_prob)
}

# Table Function ----------------------------------------------------------
table_function <- function(results, called) {
  #Getting state name to state abbreviation key table
  states <- cbind(state.name,state.abb) %>% as_tibble() 
  #Making the called data have a common column with the state name state abb key
  called <- called %>% rename("state.abb"=State) %>% rename("Called"=State_Winner)
  
  #Making empty string for non-called states, so there can be a blank entry in the table for them
  table_data <- results$states %>% 
    as_tibble() %>% 
    left_join(called) %>% 
    mutate(Called=ifelse(is.na(Called),"",Called)) 

  #Making "called" an ordered factor so I can arrange the table using i
  table_data$Called <- factor(table_data$Called,
                              levels=c("Biden",
                                       "",
                                       "Trump"),
                              labels=c("Biden",
                                       "",
                                       "Trump"))
  #Making a full table of state names, win probs, and called statuses
  table_data <- table_data %>% 
    left_join(states) %>%
    mutate(state.name=ifelse(is.na(state.name),
                             state.abb,
                             state.name)) %>% 
    arrange(Called,desc(value)) %>% 
    mutate(value=round(value,1)) %>% 
    dplyr::select(state.name,value, Called)
  
  #Splitting the above full table in half, so the table can fit on the screen. Need to do some temp renaming to make 
  #it play nice with tidyverse functions
  top <- table_data %>% head(nrow(table_data)/2)
  bottom<- table_data %>% tail(nrow(table_data)/2) %>% rename("state"=state.name,"percent"=value,"Called2"=Called)
  table_data <- cbind(top,bottom)
  
  #Actually making the table itself
  table_data %>% 
    mutate(value = value / 100,
           percent = percent / 100) %>%
    #Initializing table
    gt() %>% 
    #Setting title
    tab_header(title = "Biden Win Probability by State") %>% 
    #Formatting relevant columns as percents
    fmt_percent(columns = c(2,5), decimals = 1) %>%
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
      table.font.size = 11
    ) %>% 
    #Manually labeling columns
    cols_label(state.name="",Called="Winner",value="Win Probability",
               state="","percent"="Win Probability", Called2="Winner") %>% 
    cols_align(align = "left",
               columns = c(1,3,4,6)) %>%
    cols_align(align = "center",
               columns = c(2,5)) %>%
    #Setting color for win prob columns
    data_color(
      columns = vars(value,percent),
      colors = scales::col_numeric(
        palette = c("#CB454A", "#2E74C0"),
        domain = c(0,1)
      )) %>% 
    #Setting color for called status columns
    data_color(
      columns = vars(Called,Called2),
      colors = c("#2E74C0","White","#CB454A")
    )
}

# Map Function ------------------------------------------------------------
map_function <- function(results) {
  #Getting state name state abb key table
  states <- cbind(state.name,state.abb) %>% as_tibble()
  #Because Shiny hates albersusa package, I downloaded the shapefile as an rds, and here I am manually loading it back in
  #And bringing it back to an sf object
  USA_Map <- read_rds("USA_Map.rds") %>% sf::st_as_sf()
  #Manually resetting projection to LAEA, otherwise Shiny yells at me. Not sure why
  st_crs(USA_Map) <- "laea"
  
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
    #This will be the label that shows up when you hover over a state on the app
    mutate(label=paste0("Biden Win Probability: ", value, "%"))
  
  plot <- left_join(USA_Map,map_data) %>% 
    ggplot() +
    #The text aesthetic doesn't exist here in reality, but is a placeholder used later by ggplotly to generate the 
    #label that appears when a user hovers over a state
    geom_sf(aes(fill=value,text=label)) +
    scale_fill_gradient(low = "#CB454A",
                        high = "#2E74C0")+
    labs(title="Where Biden is favored to <span style='color: #2E74C0'>win</span> versus <span style='color: #CB454A'>lose</span> ") +
    theme_minimal(base_size = 12) +
    theme(title=element_markdown(size=rel(1),face="bold", hjust = .5),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
    
    
    ggplotly(plot,tooltip = "text") %>% 
      style(hoveron="fills") #Makes it hover on the fill rather than the lines
}
