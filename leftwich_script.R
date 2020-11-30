library(tidyverse)
library(rvest)
library(here)
library(ggplot2)
library(plotly)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))




scrape_trails <- function(start_int){
  page_url <- paste0(
    "https://www.wta.org/go-outside/hikes?b_start:int=",
    start_int
  )
  
  page_html <- read_html(page_url)
  
  page_html %>% 
    
    html_nodes(".search-result-item") %>% 
    
    map(
      function(hike){
        
        hike_name <- hike %>% html_nodes(".listitem-title") %>% html_nodes("span") %>%  html_text()
        hike_location <- hike %>% html_node("h3") %>% html_text()
        
        hike_stats <- hike %>% html_node(".hike-stats")
        
        hike_length <- hike_stats %>% html_nodes(".hike-length") %>%html_nodes("span") %>%  html_text()
        hike_gain <- hike_stats %>% html_nodes(".hike-gain") %>%html_nodes("span") %>%  html_text()
        hike_highpoint <- hike_stats %>% html_nodes(".hike-highpoint") %>%html_nodes("span") %>%  html_text()
        hike_rating <- hike_stats %>% html_nodes(".hike-rating") %>%html_nodes(".current-rating") %>%  html_text()
        
        hike_desc <- hike %>% html_nodes(".listing-summary") %>% html_text()
        
        hike_features <- hike %>% html_nodes(".trip-features") %>% html_nodes("img") %>% html_attr("title") %>% list()
        
        tibble(
          name = hike_name,
          location = hike_location,
          length = hike_length,
          gain = hike_gain,
          highpoint = hike_highpoint,
          rating = hike_rating,
          features = hike_features,
          description = hike_desc
        )
      }) %>% 
    bind_rows() %>% 
    mutate(description = str_remove(description, "\n") %>% str_squish())
}

start_int <- c(1, seq(30, 3840, by = 30))

hike_data <- start_int %>% 
  map_dfr(scrape_trails)

saveRDS(hike_data,file = "2020/2020-11-24/hike_data.rds")

clean_hike_data <- hike_data %>% 
  mutate(
    trip = case_when(
      grepl("roundtrip",length) ~ "roundtrip",
      grepl("one-way",length) ~ "one-way",
      grepl("of trails",length) ~ "trails"),
    
    length_total = as.numeric(gsub("(\\d+[.]\\d+).*","\\1", length)) * ((trip == "one-way") + 1),
    
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    
    location_general = gsub("(.*)\\s[-][-].*","\\1",location)
  )




hike_plot <- ggplot(clean_hike_data) + 
  geom_rect(aes(
    xmin = 0,
    xmax = length_total,
    ymin = 0,
    ymax = gain,
    label = name
  ),
  alpha = .4,
  fill = "#228B22",
  color = "#765C48"
  ) + 
  facet_wrap(
    ~ location_general,
    scales = "free_x"
  ) +
  labs(
    title = "Washington State Hikes",
    x = "Hike Length (miles)",
    y = "Hike Elevation Gain (ft)",
    caption = "Data from Washingon Trails Association (wta.org) | Viz by @ellis_hughes"
  )


ggplotly(hike_plot)
