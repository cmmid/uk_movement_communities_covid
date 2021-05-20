#Visualise changes in network structure after the first national intervention

# check that these quadkeys are in UK
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
  require(here)
  require(igraph)
})

if(interactive()){
  .args <-  c(here("data/processed/infomap/infomap_daily.csv"),
              here("data/processed/infomap/infomap_weekly.csv"),
              here("data/processed/infomap/infomap_monthly.csv"),
              here("data/processed/geo/tiles.shp"),
              here("data/processed/pop/tile_12_oa_pop.csv"),
              here("output/figs/tile_oa_pop_comparison.png"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

#check if missing data in late September
read_communities <- function(fn){
  data <- read_csv(fn) %>% 
    mutate(quadkey = stringr::str_pad(quadkey, 12, "left", "0"),
           date = as.Date(date)) %>% 
    filter(date <= as.Date("2020-11-01"))
  
  return(data)
}

im_day <- read_communities(.args[1])
im_week <- read_communities(.args[2])
im_month <- read_communities(.args[3])

tiles <- st_read(.args[2]) %>% 
  st_set_crs(4326)


day <- im_day %>% 
  filter(date == min(date))

week <- im_week %>% 
  filter(date == min(date),
         quadkey %in% day$quadkey)

compare(day$cluster, week$cluster, method = "nmi")


# tree diagram from each day to higher week & from each week to higher month

days <- im_day %>% pull(date) %>% unique()
weeks <- im_week %>% pull(date) %>% unique()
months <- im_month %>% pull(date) %>% unique()

# day to week - specify which days are within the higher time period
day_week_pairs <- tibble(expand.grid(days, weeks)) %>% 
  rename(day = Var1, week = Var2) %>% 
  mutate(day_week = lubridate::floor_date(day, "week"),
         within = week == day_week) %>% 
  filter(within) %>% 
  select(day, week) %>% 
  group_by(day) %>% 
  group_split()

# week to month
week_month_pairs <- tibble(expand.grid(weeks, months)) %>% 
  rename(week = Var1, month = Var2) %>% 
  mutate(week_month = lubridate::floor_date(week, "month"),
         within = month == week_month) %>% 
  filter(within) %>% 
  select(week, month) %>% 
  group_by(week) %>% 
  group_split()


compare_community_labelling <- function(small_date, small_data, big_date, big_data){
  
  small_data <- small_data %>% 
    filter(date == small_date)
  
  big_data <- big_data %>% 
    filter(date == big_date,
           quadkey %in% small_data$quadkey)
  
  small_data <- small_data %>% 
    filter(quadkey %in% big_data$quadkey)
  
  return (
    compare(small_data$cluster, big_data$cluster, method = "nmi")
  )

}

compare_time_intervals <- function(pairs, small_name, big_name, small_data, big_data){
  
  res <- c()

  for (pair in pairs){
    small_date <- pair %>% pull(!! sym(small_name))
    big_date <- pair %>% pull(!! sym(big_name))
    
    res <- append(res, compare_community_labelling(small_date, small_data, big_date, big_data))
  }
  
  return (
    do.call(rbind, pairs) %>% 
      mutate(nmi = res)
  )
}

day_week_nmi <- compare_time_intervals(day_week_pairs, "day", "week", im_day, im_week)
week_month_nmi <- compare_time_intervals(week_month_pairs, "week", "month", im_week, im_month)

day_week_nmi <- day_week_nmi %>% 
  mutate(y = 1, 
         yend = 2, 
         week = week + 3) %>% 
  rename(x = day, 
         xend = week)

week_month_nmi <- week_month_nmi %>% 
  mutate(y = 2, 
         yend = 3,
         week = week + 3,
         month = month + 17) %>% 
  rename(x = week, 
         xend = month)


levels <- c("Monthly", "Weekly", "Daily")

p_nmi <- ggplot() + 
  geom_segment(data = rbind(day_week_nmi, week_month_nmi), 
               aes(x = x, xend = xend, y = y, yend = yend, color = nmi), size = 0.5) + 
  geom_hline(aes(yintercept=c(3, 2, 1), linetype = factor(levels, levels = levels)), size = 0.1) + 
  theme_classic() + 
  colorspace::scale_color_continuous_sequential("Blues", limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                labels = c("0 - Complete\ndifference", "0.25", "0.5", "0.75", "1 - Complete\noverlap")) + 
  labs(color = "NMI", linetype=NULL) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank()) + 
  ylim(0.9, 3.1)

p_nmi

ggutils::ggsave_png_pdf(p_nmi, here("output/figs/temporal_nmi.png"),
                        12, 5)

plotly::ggplotly(p_nmi)
  
do.call(rbind, day_week_pairs) %>% 
  mutate(nmi = res, 
         y_day = 1, 
         y_week = 2,
         week = week + 3) %>% 
  ggplot() + 
  geom_segment(aes(x = day, xend = week, y = y_day, yend = y_week), size = 0.1)

do.call(rbind, day_week_pairs) %>% 
  mutate(nmi = res, 
         id = row_number()) %>% 
  rename(x_day = day, 
         x_week = week) %>% 
  mutate(x_week = x_week + 3) %>% 
  pivot_longer(cols = starts_with("x")) %>% 
  rename(x = value) %>% 
  mutate(y = ifelse(name == "x_day", 1, 2)) %>% 
  ggplot() + 
  geom_curve(aes(x = x, y = y, group = id, color = nmi), size = 0.2)
