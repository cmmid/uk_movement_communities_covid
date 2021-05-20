# -- Template by bubble with <3. --


# *** bubble make ***

# Script to ...

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(sf)
  require(RColorBrewer)
  require(cowplot)
})

# Source modules
# source("file")

# *** bubble input start ***
# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here("data/processed/infomap/infomap_labelled_daily.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/geometry/tiles/tiles.shp",
              here("data/processed/mob/movement_daily.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp",
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}
# *** bubble input end ***

tiles <- st_read(.args[2]) %>% 
  st_transform(27700)

mob <- read_csv(.args[3]) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(start_quadkey = stringr::str_pad(start_quadkey, 12, side='left', '0'),
         end_quadkey = stringr::str_pad(end_quadkey, 12, side='left', '0'))

pop <- read_csv(.args[4])

a3 <- st_read(.args[5]) %>% 
  st_simplify(preserveTopology = T, dTolerance = 75) 

comm_im <- read_csv(.args[1]) %>% 
  filter(quadkey %in% unique(pop$quadkey_12)) %>% 
  filter(date < as.Date('2020-11-01'))

comm_pal <- ggutils::qualitative_pal(unique(comm_im$cluster))

# Get degree of network vertices

mob_days <- mob %>% 
  filter(start_quadkey %in% unique(pop$quadkey_12) & end_quadkey %in% unique(pop$quadkey_12)) %>% 
  group_by(date) %>% 
  group_split()

dates <- sort(unique(mob$date))

od_igraph <- function(mob_date){
  
  graph_df <- mob_date %>% 
    select(start_quadkey, end_quadkey, n_crisis) %>% 
    rename(from = start_quadkey, 
           to = end_quadkey,
           weight = n_crisis)
  
  g <- igraph::graph_from_data_frame(graph_df)  
  
  return(g)
  
}

g <- lapply(mob_days, od_igraph)

get_degree <- function(i, g, dates){
  
  g <- g[[i]]
  
  df <- tibble(quadkey = V(g)$name, 
               degree = as.numeric(degree(g)),
               date = dates[i])
  
  return(df)
  
}

dummy_pal <-  c('Intervention\nArea' = 'red', 
                'Community' = 'darkblue', 
                'Degree' = 'black')
arr <- factor(names(dummy_pal), levels = names(dummy_pal))

dummy_p <- ggplot() + 
  geom_bar(aes(x = c(1, 2, 3), fill = arr), size = 0) + 
  scale_fill_manual(values = dummy_pal) + 
  theme(legend.title = element_blank())

legend <- cowplot::get_legend(dummy_p)

deg <- lapply(1:length(g), get_degree, g = g, dates = dates)

# define local params

la_name_ne <- c('Newcastle upon Tyne', 'Gateshead', 'Northumberland', 'North Tyneside', 'South Tyneside', 'Sunderland')
i_date_ne <- as.Date('2020-09-18')
zoom_ne <- c(10000, 20000)
coeff_ne <- 90

ne <- list(name = la_name_ne,
           date = i_date_ne,
           zoom = zoom_ne,
           coeff = coeff_ne)
#goog_code_ne <- 'GB-NET'

la_name_mn <- c('Manchester', 'Trafford', 'Bury', 'Tameside', 'Rochdale', 'Salford', 'Oldham', 'Stockport', 'Wigan', 'Bolton')
i_date_mn <- as.Date('2020-07-30')
zoom_mn <- c(15000, 10000)
coeff_mn <- 90

mn <- list(name = la_name_mn,
           date = i_date_mn,
           zoom = zoom_mn,
           coeff = coeff_mn)
#goog_code_mn <- 'GB-MAN'

la_name_ln <- c('Blackburn with Darwen', 'Blackpool', 'Burnley', 'Chorley', 'Fylde', 'Hyndburn', 'Lancaster', 'Pendle', 'Preston', 'Ribble Valley', 'South Ribble', 'Rossendale', 'West Lancashire', 'Wyre')
i_date_ln <- as.Date('2020-08-22')
zoom_ln <- c(15000, 10000)
coeff_ln <- 90

ln <- list(name = la_name_ln,
           date = i_date_ln,
           zoom = zoom_ln,
           coeff = coeff_ln)
#goog_code_ln <- 'GB-BPL'

la_name_lei <- c('Leicester')
i_date_lei <- as.Date('2020-06-29')
zoom_lei <- c(15000, 10000)
coeff_lei <- 7

lei <- list(name = la_name_lei,
           date = i_date_lei,
           zoom = zoom_lei,
           coeff = coeff_lei)
#goog_code_lei <- 'GB-LCE'

qks <- unique(c(mob$start_quadkey, mob$end_quadkey))

a3_int <- st_intersection(tiles %>% filter(quadkey %in% qks), a3)

a3_int_top <- a3_int %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>% 
  group_by(quadkey) %>% 
  top_n(1, area)

#uk %>% st_drop_geometry() %>% view

area <- ln

i_names <- area$name
i_date <- area$date
i_zoom <- area$zoom
i_coeff <- area$coeff

area_tiles <- a3_int_top %>% 
  filter(lad17nm %in% i_names) %>% 
  pull(quadkey) %>% as.character()

date_range <- c(i_date - 14, i_date + 14)

area_mob <- mob %>% 
  filter(start_quadkey != end_quadkey) %>% 
  mutate(date = as.Date(date)) %>% 
  #mutate(start = ifelse(start_quadkey %in%  area_tiles, 'lockdown', 'other')) %>% 
  #mutate(end = ifelse(end_quadkey %in%  area_tiles, 'lockdown', 'other')) %>% 
  mutate(period = ifelse(date < i_date & date >= date_range[1], 'Pre', NA),
         period = ifelse(date > i_date & date <= date_range[2], 'Post', period)) %>% 
  filter(!is.na(period)) %>% 
  #mutate(start = ifelse(start == 'lockdown', start_quadkey, start)) %>% 
  #mutate(end = ifelse(end == 'lockdown', end_quadkey, end)) %>% 
  group_by(period, start_quadkey, end_quadkey) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T) / length(unique(date)), .groups = 'drop') %>% 
  mutate(start = ifelse(start_quadkey %in%  area_tiles, 'Intervention', 'Other')) %>% 
  mutate(end = ifelse(end_quadkey %in%  area_tiles, 'Intervention', 'Other')) %>% 
  filter(!c(start == 'Other' & end == 'Other'))

am <- list()

am[['Post']] <- area_mob %>% 
  filter(period == 'Post')

am[['Pre']] <- area_mob %>% 
  filter(period == 'Pre')

# communities

overlap_comm <- comm_im %>% 
  mutate(period = ifelse(date < i_date & date >= date_range[1], 'Pre', NA),
         period = ifelse(date > i_date & date <= date_range[2], 'Post', period)) %>% 
  filter(!is.na(period)) %>% 
  filter(quadkey %in% area_tiles) %>% 
  group_by(period) %>% 
  summarise(cluster = unique(cluster), .groups = 'drop')

oc <- list()

oc[['Pre']] <- overlap_comm %>% filter(period == 'Pre')
oc[['Post']] <- overlap_comm %>% filter(period == 'Post') 

comm_plot_data <- function(p){
  
  if (p == 'Pre'){
    pre_comm <- comm_im %>% 
      mutate(period = ifelse(date < i_date & date >= date_range[1], 'Pre', NA)) %>% 
      drop_na(period) 
  }
  
  if (p == 'Post'){
    pre_comm <- comm_im %>% 
      mutate(period = ifelse(date >= i_date & date <= date_range[2], 'Post', NA)) %>% 
      drop_na(period)
  }
  
  print(pre_comm$date %>% unique)
  
  pre_comm <- pre_comm %>% 
    filter(cluster %in% oc[[p]]$cluster) %>% 
    group_by(period, quadkey) %>% 
    summarise(cluster = unique(cluster), .groups = 'drop')
  
  pre_comm <- tiles %>% 
    left_join(pre_comm, by = 'quadkey') %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(n = 1, .groups = 'drop')
  
  return(pre_comm)
  
}

plot_comm <- function(d, zoom_x=0, zoom_y=0){
  
  p <- d %>% 
    ggplot() + 
    ggutils::plot_basemap('United Kingdom', country_size = 0.1) + 
    geom_sf(aes(fill = cluster), size = 0) + 
    ggutils::geo_lims(d, zoom_x = zoom_x, zoom_y = zoom_y) + 
    geom_sf(data = a3 %>% filter(lad17nm %in% i_names), size = 0.3, color = 'red', fill = 'transparent') + 
    scale_fill_manual(values = comm_pal) + 
    theme_void() + 
    theme(legend.position = 'none')
  
  return(p)
  
}

pre_p <- plot_comm(comm_plot_data('Pre'), zoom_x = i_zoom[1], zoom_y = i_zoom[2])
post_p <- plot_comm(comm_plot_data('Post'), zoom_x = i_zoom[1], zoom_y = i_zoom[2])

title <- ggdraw() + 
  draw_label("c", x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 0, 7))

p_comm <- cowplot::plot_grid(pre_p, post_p, ncol = 1)

p_comm <- cowplot::plot_grid(title, p_comm, ncol = 1, rel_heights = c(0.1, 1))

# between tile journeys

area_btw <- mob %>% 
  #filter(start_quadkey != end_quadkey) %>% 
  filter(start_quadkey %in% area_tiles) %>% 
  mutate(type = ifelse(start_quadkey == end_quadkey, 'within', 'between')) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date, type) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = type, values_from = n_crisis) %>% 
  #drop_na(between) %>% 
  mutate(perc_leaving = (between / (within + between)) * 100)

#%>% 
  #mutate(start = ifelse(start_quadkey %in%  area_tiles, 'lockdown', 'other')) %>% 
  #mutate(end = ifelse(end_quadkey %in%  area_tiles, 'lockdown', 'other')) %>% 
  #filter(!c(start == 'other' & end == 'other')) %>% 
  #group_by(period) %>% 
  #group_split()

connected_communities <- comm_im %>% 
  filter(quadkey %in% area_tiles)

connected_communities <- comm_im %>% 
  filter(cluster %in% connected_communities$cluster)

comm_btw <- mob %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(type = ifelse(start_quadkey == end_quadkey, 'within', 'between')) %>% 
  left_join(comm_im %>% select(date, quadkey, cluster), by = c('date', 'start_quadkey' = 'quadkey')) %>% 
  drop_na(cluster) %>% 
  mutate(type = ifelse(start_quadkey == end_quadkey, 'within', 'between')) %>% 
  group_by(date, type) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = type, values_from = n_crisis) %>% 
  mutate(perc_leaving = (between / (within + between)) * 100) %>% 
  mutate(date = as.Date(date))
  #group_by(date) %>% 
  #summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop')
  

p_mob <- area_btw %>% 
  ggplot() + 
  geom_path(data = comm_btw, aes(x = date, y = perc_leaving), size = 0.3, color = 'grey') + 
  geom_path(aes(x = date, y = perc_leaving), size = 0.3) + 
  geom_vline(aes(xintercept=i_date), size = 0.2, linetype = 'dashed') + 
  theme_classic() + 
  theme(axis.title.x = element_blank()) + 
  ylab('% Leaving Cell') + 
  ggtitle('b')

deg_data <- do.call(rbind, deg) %>% 
  mutate(area = quadkey %in% area_tiles) %>% 
  filter(area) %>% 
  group_by(date) %>% 
  summarise(degree = sum(degree, na.rm = T), .groups = 'drop') %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(week) %>% 
  summarise(degree = mean(degree, na.rm = T), .groups = 'drop')

coeff <- i_coeff

p_mob <- area_btw %>% 
  ggplot() + 
  geom_path(data = comm_btw, aes(x = date, y = perc_leaving), size = 0.3, color = 'darkblue') + 
  geom_path(aes(x = date, y = perc_leaving), size = 0.3, color = 'red') + 
  geom_vline(aes(xintercept=i_date), size = 0.2, linetype = 'dashed') + 
  geom_path(data = deg_data, aes(x = as.Date(paste(2020, week, 1, sep="-"), "%Y-%U-%u"), 
                                 y = degree / coeff),
            color = 'black', size = 0.4) + 
  scale_y_continuous(
    
    name = "% Leaving Cell",
    
    sec.axis = sec_axis(~.*coeff, name="Weekly Average Degree")
  ) + 
  theme_classic() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.15, b = 0, l = 0, unit = 'cm'))) + 
  ggtitle('a') + 
  xlim(c(i_date - 30, i_date + 30))



# cases

cases <- readr::read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv') %>% 
  rename(code = `areaCode`) %>% 
  rename(name = `areaName`) %>% 
  rename(count = `newCasesBySpecimenDate`) %>% 
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>% 
  filter(area_type == 'E0')

p_cases <- cases %>% 
  filter(name %in% i_names) %>% 
  filter(date > as.Date(mob$date) %>% min,
         date < as.Date(mob$date) %>% max) %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = count), stat = 'identity') + 
  geom_vline(aes(xintercept=i_date), size = 0.2, linetype = 'dashed') + 
  theme_classic() + 
  ylab('Confirmed Cases') + 
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,4.5,0,0),"cm")) + 
  ggtitle('b') + 
  xlim(c(i_date - 30, i_date + 30))

#p_map <- cowplot::plot_grid(p_graph, p_comm, nrow = 1)

p_mob_leg <- cowplot::plot_grid(p_mob, legend, nrow = 1, rel_widths = c(0.8, 0.2))

p_ts <-  cowplot::plot_grid(p_mob_leg, p_cases, nrow = 2)

p <- cowplot::plot_grid(p_ts, p_comm, nrow = 1, rel_widths = c(0.7, 0.3))


if ('Leicester' %in% i_names){
  ggutils::ggsave_png_pdf(p, 
                          here("output/figs/local_lei.png"),
                          9, 4)  
}



if ('Newcastle upon Tyne' %in% i_names){
  ggutils::ggsave_png_pdf(p, 
                          here("output/figs/local_ne.png"),
                          9, 4)  
}

if ('Manchester' %in% i_names){
  ggutils::ggsave_png_pdf(p, 
                          here("output/figs/local_mn.png"),
                          9, 4)  
}

if ('Blackpool' %in% i_names){
  ggutils::ggsave_png_pdf(p, 
                          here("output/figs/local_ln.png"),
                          9, 4)  
}


