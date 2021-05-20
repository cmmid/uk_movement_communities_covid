# -- Template by bubble with <3. --

# *** bubble make ***

# Script to plot an overview of community structure


# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
  require(ggraph)
  require(igraph)
  require(RColorBrewer)
})

# Source modules
# source("file")

# *** bubble input start ***
# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here("data/processed/infomap/infomap_labelled_daily.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/geometry/tiles/tiles.shp",
              "/Users/hamishgibbs/Downloads/NHS_England_Regions_(April_2020)_Boundaries_EN_BFC/NHS_England_Regions_(April_2020)_Boundaries_EN_BFC.shp",
              here("data/processed/mob/movement_daily.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
      "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}
# *** bubble input end ***

comm_im <- read_csv(.args[1]) %>% 
  filter(date < as.Date('2020-11-01'))

tiles <- st_read(.args[2]) %>% 
  st_transform(27700)

nhs <- st_read(.args[3]) %>% 
  st_simplify(dTolerance = 150, preserveTopology = T)

mob <- read_csv(.args[4]) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(start_quadkey = stringr::str_pad(start_quadkey, 12, side='left', '0'),
         end_quadkey = stringr::str_pad(end_quadkey, 12, side='left', '0'))

pop <- read_csv(.args[5])

mob <- mob %>% 
  filter(start_quadkey %in% unique(pop$quadkey_12) & end_quadkey %in% unique(pop$quadkey_12)) %>% 
  group_by(date, start_quadkey, end_quadkey) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
  filter(date < as.Date('2020-11-01'))

comm_date <- mob %>% 
  left_join(comm_im %>% select(-flow), by = c('date', 'start_quadkey' = 'quadkey')) %>% 
  rename(start_comm = cluster) %>% 
  left_join(comm_im %>% select(-flow), by = c('date', 'end_quadkey' = 'quadkey')) %>% 
  rename(end_comm = cluster) %>% 
  group_by(date, start_comm, end_comm) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>% 
  filter(start_comm != end_comm) %>% 
  group_by(date) %>% 
  group_split()

uk <- rnaturalearth::ne_states('United Kingdom', returnclass = 'sf') %>% 
  st_transform(27700)

tile_uk_int <- st_intersection(tiles, uk) %>% 
  filter(geonunit %in% c('Scotland', 'Northern Ireland', 'Wales'))

tile_uk_int <- tile_uk_int %>% 
  select(quadkey, geonunit) %>% 
  rename(region = geonunit)

tile_nhs_int <- st_intersection(tiles, nhs)

tile_nhs_int <- tile_nhs_int %>% 
  select(quadkey, nhser20nm) %>% 
  rename(region = nhser20nm)

tile_int <- rbind(tile_nhs_int, tile_uk_int)

tile_regions <- tile_int %>% 
  mutate(area = units::set_units(st_area(geometry), 'km^2')) %>% 
  group_by(quadkey) %>% 
  top_n(as.numeric(area), n = 1) %>% 
  st_drop_geometry() %>% 
  select(quadkey, region, area) %>% 
  mutate(quadkey = as.character(quadkey),
         region = as.character(region),
         area = as.numeric(area))


unique(tile_regions$region)

region_pal <- c(
  'London' = '#ffff33',
  'South East' = '#b03060',
  'South West' = '#9880C2',
  'East of England' = '#006400',
  'Midlands' = '#88582C',
  'North East and Yorkshire' = '#ff0000',
  'North West' = '#6495ed',
  'Northern Ireland' = '#ff00ff', 
  'Wales' = '#ff4500',
  'Scotland' = '#00008b'
)
region_pal <- rev(c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999', '#8dd3c7'))
names(region_pal) <- unique(tile_regions$region)

#tile_regions %>% 
#  write_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/quick/regions/tile_regions.csv')

date_seq <- sort(comm_im$date %>% unique())

date1 <- which(date_seq == as.Date('2020-03-10'))
date2 <- which(date_seq == as.Date('2020-04-13'))
date3 <- which(date_seq == as.Date('2020-07-13'))
date4 <- which(date_seq == as.Date('2020-10-06'))

od_igraph_comm <- function(mob_date){
  
  graph_df <- mob_date %>% 
    select(start_comm, end_comm, n_crisis) %>% 
    rename(from = start_comm, 
           to = end_comm,
           weight = n_crisis)
  
  g <- igraph::graph_from_data_frame(graph_df)  
  
  return(g)
  
}

get_community_graph <- function(comm_date){
  
  focus_date <- unique(comm_date$date)
  
  node_size <- mob %>% group_by(start_quadkey) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop')
  
  comm_size <- comm_im %>% 
    filter(date == focus_date) %>% 
    left_join(node_size, by = c('quadkey' = 'start_quadkey')) %>% 
    group_by(cluster) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T), .groups='drop')  
  
  comm_region <- comm_im %>% 
    filter(date == focus_date) %>% 
    left_join(tile_regions, by = c('quadkey')) %>% 
    group_by(cluster, region) %>%
    summarise(area = sum(area, na.rm = T), .groups='drop') %>% 
    group_by(cluster) %>% 
    top_n(area, n = 1) %>% 
    select(-area) %>% 
    ungroup()
  
  g <- od_igraph_comm(comm_date)
  
  node_names <- names(V(g))
  
  node_order <- tibble(cluster = node_names) %>% 
    left_join(comm_size, by = c('cluster')) %>% 
    left_join(comm_region, by = c('cluster')) %>% 
    arrange(region, -n_crisis) %>% 
    mutate(rank = row_number()) %>% 
    group_by(region) %>% 
    mutate(n_comms = length(unique(cluster)),
           region_rank = row_number(),
           region_label = ceiling(unique(n_comms) / 2),
           region_label = region_label == region_rank)
  
  node_order <- tibble(cluster = node_names) %>% 
    left_join(node_order, by = 'cluster')
  
  g <- set.vertex.attribute(g, 'rank', value = node_order$rank)
  g <- set.vertex.attribute(g, 'region', value = node_order$region)
  g <- set.vertex.attribute(g, 'region_label', value = node_order$region_label)
  
  return(g)  
  
}

plot_community_graph <- function(g, scale_width, legend_position = 'none', alpha = 0.2, r = 0.02){
  
  p <- ggraph(g, layout = 'linear', circular = T, sort.by=rank) + 
    geom_edge_arc(aes(width = weight), alpha=alpha) + 
    scale_width + 
    geom_node_circle(aes(fill = region, r = r), size = 0, alpha = 0.8) +
    scale_fill_manual(values = region_pal) + 
    theme_void() + 
    theme(legend.position = legend_position,
          plot.margin=unit(c(0,0.55,0,0.55),"cm")) + 
    labs(fill = '') + 
    #scale_edge_width_continuous(guide=F) + 
    guides(guide_legend(nrow = 1))

  return(p)
  
}

g1 <- get_community_graph(comm_date = comm_date[[date1]])
g2 <- get_community_graph(comm_date = comm_date[[date2]])
g3 <- get_community_graph(comm_date = comm_date[[date3]])
g4 <- get_community_graph(comm_date = comm_date[[date4]])

weights <- c(get.edge.attribute(g1, 'weight'), 
             get.edge.attribute(g2, 'weight'), 
             get.edge.attribute(g3, 'weight'),
             get.edge.attribute(g4, 'weight'))

scale_width <- scale_edge_width(range = c(0.1, 2), limits = c(min(weights), max(weights)))

pg1 <- plot_community_graph(g1, scale_width = scale_width, alpha = 0.5)
pg2 <- plot_community_graph(g2, scale_width = scale_width, alpha = 0.5)
pg3 <- plot_community_graph(g3, scale_width = scale_width, alpha = 0.5)
pg4 <- plot_community_graph(g4, scale_width = scale_width, alpha = 0.5)


# plot the community maps here

comm_pal <- ggutils::qualitative_pal(unique(comm_im$cluster))

comm_date_labels <- comm_im %>% 
  group_by(date) %>% 
  group_split()

tiles <- tiles %>% st_transform(4326)

plot_cluster_map <- function(comm_date_labels){
  
  focus_date <- comm_date_labels$date %>% unique()
  
  p <- tiles %>% 
    left_join(comm_date_labels, by = 'quadkey') %>% 
    left_join(tile_regions, by = 'quadkey') %>% 
    drop_na(cluster) %>% 
    group_by(cluster, region) %>% 
    summarise(area = sum(area, na.rm = T), .groups = 'drop') %>% 
    mutate(cluster_num = factor(cluster),
           cluster_num = as.numeric(cluster_num)) %>% 
    ggplot() + 
    ggutils::plot_basemap('United Kingdom', country_size = 0) + 
    geom_sf(aes(alpha = cluster_num, fill = region), size = 0.09, color = '#414141') + 
    scale_fill_manual(values = region_pal) + 
    theme_void() + 
    ylim(49.9, 59) + 
    xlim(-7.7, 1.6) + 
    theme(legend.position = 'none',
          plot.margin=unit(c(0,0,0,0),"cm")) + 
    ggtitle(strftime(focus_date, format = '%B %d, %Y'))
  
  return(p)
}

pm1 <- plot_cluster_map(comm_date_labels[[date1]])
pm2 <- plot_cluster_map(comm_date_labels[[date2]])
pm3 <- plot_cluster_map(comm_date_labels[[date3]])
pm4 <- plot_cluster_map(comm_date_labels[[date4]])

pm <- cowplot::plot_grid(pm1, pm2, pm3, pm4, nrow = 1)

pg <- cowplot::plot_grid(pg1, pg2, pg3, pg4, nrow = 1)

p <- cowplot::plot_grid(pm, pg, nrow = 2, rel_heights = c(0.7, 0.3))

#graph_legend <- cowplot::get_legend(plot_community_graph(g4, scale_width = scale_width, legend_position = 'bottom'))

#p_leg <- cowplot::plot_grid(p, graph_legend, nrow = 2, rel_heights = c(0.9, 0.1))

ggutils::ggsave_png_pdf(p,
                        '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/figures/community_overview.png',
                        width = 12, height = 7.8)


# Number of communities on specific days
comm_im %>% filter(date == date_seq[date1]) %>% pull(cluster) %>% unique() %>% length()
comm_im %>% filter(date == date_seq[date2]) %>% pull(cluster) %>% unique() %>% length()
comm_im %>% filter(date == date_seq[date3]) %>% pull(cluster) %>% unique() %>% length()
comm_im %>% filter(date == date_seq[date4]) %>% pull(cluster) %>% unique() %>% length()


# number and area of communities

qks <- unique(comm_im$quadkey)

tile_area <- tiles %>% 
  filter(quadkey %in% qks) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>% 
  st_drop_geometry()

n_comms <- comm_im %>% 
  left_join(tile_area, by = 'quadkey') %>% 
  group_by(date, cluster) %>% 
  summarise(area = sum(area, na.rm = T), .groups = 'drop') %>% 
  group_by(date) %>% 
  summarise(area = mean(area, na.rm = T),
            n_comm = length(unique(cluster)), .groups = 'drop')

coeff <- 0.3

p_n <- n_comms %>% 
  ggplot() + 
  geom_path(aes(x = date, y = area)) + 
  geom_path(aes(x = date, y = n_comm / coeff), color = 'red') + 
  theme_classic() + 
  scale_y_continuous(
    name = bquote('Average Area'~(km^2)),
    sec.axis = sec_axis(~.*coeff, name="Number of Communities")
  ) + 
  theme(axis.title.x = element_blank())

ggutils::ggsave_png_pdf(p_n,
                        '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/figures/n_communities.png',
                        8, 4)


# community labels vs pop_density

n_lab <- comm_im %>% 
  group_by(quadkey) %>% 
  summarise(n_labels = length(unique(cluster)),
            n_days = length(unique(date))) %>% 
  mutate(n_labels_norm = n_days / n_labels) %>% 
  arrange(-n_labels)

p_lab <- n_lab %>% 
  left_join(pop, by = c('quadkey' = 'quadkey_12')) %>% 
  left_join(tile_area, by = 'quadkey') %>% 
  ggplot() + 
  geom_point(aes(x = log(pop / area, 10), y = n_labels, group = quadkey), size = 0.1) + 
  theme_classic() + 
  ylab('Number of Unique Labels') + 
  xlab(bquote('Population Density '~(log[10])))


ggutils::ggsave_png_pdf(p_lab,
                        '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/figures/n_labs_pop.png',
                        8, 4)


# degree through time
g <- lapply(comm_date, od_igraph_comm)

deg <- lapply(g, degree)

deg <- lapply(deg, as.data.frame)

dates <- unique(comm_im$date)

for (i in 1:length(deg)){
  deg[[i]]$date <- dates[i]
  deg[[i]]$cluster <- rownames(deg[[i]])
}

deg <- do.call(rbind, deg) %>% 
  as_tibble() %>% 
  rename(degree = `X[[i]]`)


p_deg <- comm_im %>% 
  left_join(deg, by = c('cluster', 'date')) %>% 
  mutate(period = ifelse(date <= as.Date('2020-03-23'), 'Pre', NA),
         period = ifelse(is.na(period) & date > as.Date('2020-03-23'), 'Post', period)) %>% 
  filter(date <= as.Date('2020-05-10')) %>% 
  group_by(quadkey, period) %>% 
  summarise(degree = sum(degree, na.rm = T) / length(unique(date)), .groups = 'drop') %>% 
  pivot_wider(names_from = period, values_from = degree) %>% 
  replace_na(list(Pre = 0, Post = 0)) %>% 
  mutate(diff = Post - Pre) %>% 
  filter(Post != 0) %>% 
  ggplot() + 
  geom_point(aes(x = Pre, y = Post), size = 0.2) + 
  geom_abline(linetype = 'dashed') + 
  theme_classic() + 
  ylab('Post-lockdown degree') + 
  xlab('Pre-lockdown degree') + 
  ylim(c(0, 65)) + 
  xlim(c(0, 65))
  
ggutils::ggsave_png_pdf(p_deg,
                        here("output/figs/comm_degreee.png"),
                        8, 4)

# needs a, b, c titles
n_comm <- comm_im %>% 
  filter(quadkey %in% c(pop$quadkey_12)) %>% 
  group_by(quadkey) %>% 
  summarise(n_comm = length(unique(cluster)), 
            n_days = length(quadkey),
            .groups = "drop")
  
tiles %>% 
  left_join(n_comm, by = c("quadkey")) %>% 
  drop_na(n_comm) %>% 
  ggplot() + 
  geom_sf(aes(fill = n_comm), size = 0)

tiles %>% 
  left_join(n_comm %>% filter(n_comm > 30)) %>% 
  drop_na(n_comm) %>% 
  ggplot() + 
  geom_sf(fill = "green", size = 0)

