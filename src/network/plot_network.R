#Visualise changes in network structure after the first national intervention
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
  require(here)
  require(igraph)
})

if(interactive()){
  .args <-  c(here("data/processed/mob/movement_daily.csv"),
              here("data/processed/geo/tiles.shp"),
              here("data/processed/geo/journey_lines.shp"),
              here("output/figs/tile_oa_pop_comparison.png"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

#check if missing data in late September
mob <- read_csv(.args[1]) %>% 
  mutate(journey = paste0(start_quadkey, "_", end_quadkey)) %>% 
  filter(date <= as.Date("2020-11-01"))

tiles <- st_read(.args[2]) %>% 
  st_set_crs(4326)

journey_lines <- st_read(.args[3]) %>% 
  st_set_crs(4326)

get_network_date_journey_lines <- function(network_data){
  
  mob_date <- network_data %>% 
    filter(start_quadkey != end_quadkey) %>% 
    select(start_quadkey, end_quadkey, n_crisis) %>% 
    rename(weight = n_crisis)
  
  g <- igraph::graph_from_data_frame(mob_date)
  
  btw <- edge.betweenness(g)
  
  mob_date <- mob_date %>% 
    mutate(betweenness = btw,
           journey = paste0(start_quadkey, "_", end_quadkey))
  
  journey_lines_date <- journey_lines %>% 
    left_join(mob_date, by = c("journey")) %>% 
    #drop_na(betweenness) %>% 
    #filter(betweenness > 0) %>% 
    drop_na(betweenness)
  
  
  return(journey_lines_date)
  
}

plot_journey_lines <- function(plot_data, title, subtitle){
  
  p <- plot_data %>% 
    ggplot() + 
    ggutils::plot_basemap() + 
    geom_sf(aes(size = betweenness)) + 
    size_scale + 
    ylim(c(50, 58.5)) + 
    xlim(c(-9, 2)) + 
    theme_void() + 
    labs(title = title, 
         subtitle=subtitle, 
         size="Edge\nBetweenness")
  
  return(p)
  
}

period_dates <- list(
  "Pre" = list(
    "start_date" = as.Date("2020-03-10"),
    "end_date" = as.Date("2020-03-22")), 
  "Post" = list(
    "start_date" = as.Date("2020-03-23"),
    "end_date" = as.Date("2020-04-04"))
)

get_period_data <- function(period){
  
  network <- mob %>% 
    filter(date >= period$start_date & date <= period$end_date) %>% 
    group_by(start_quadkey, end_quadkey) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T), 
              .groups = "drop")
  
  return(network)  
  
}

pre_network <- get_period_data(period_dates$Pre)
post_network <- get_period_data(period_dates$Post)

pre_network <- get_network_date_journey_lines(pre_network)
post_network <- get_network_date_journey_lines(post_network)

betweeness_tot <- c(pre_network$betweenness, post_network$betweenness)
size_scale <- scale_size(range = c(0.005, 1), limits = c(min(betweeness_tot), max(betweeness_tot)), labels = scales::comma)

# Plot spatial networks with edges weighted by betweenness centrality
p_network_l <- list()
p_network_l[[1]] <- plot_journey_lines(pre_network, "a", "Pre-intervention") + theme(legend.position = "none")
p_network_l[[2]] <- plot_journey_lines(post_network, "b", "Intervention") + theme(legend.position = c(0.8, 0.8))

p_network <- cowplot::plot_grid(
  p_network_l[[1]],
  p_network_l[[2]],
  nrow = 1
)

ggutils::ggsave_png_pdf(p_network, here("output/figs/network_maps.png"), 8, 5)


# Plot reduction in edges by centrality
pre_btw_q <- pre_network %>% select(journey, betweenness) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(btw_pre = betweenness)

post_btw_q <- post_network %>% select(journey, betweenness) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(btw_post = betweenness)

js <- unique(c(pre_btw_q$journey, post_btw_q$journey))

p_data <- tibble(journey = js) %>% 
  left_join(pre_btw_q, by = 'journey') %>%
  mutate(btw_q_pre = ntile(btw_pre, 10)) %>% 
  drop_na(btw_q_pre) %>% 
  left_join(post_btw_q, by = 'journey') %>% 
  mutate(btw_q_post = ntile(btw_post, 10)) %>% 
  replace_na(list(btw_q_pre = 'missing_pre')) %>% 
  replace_na(list(btw_q_post = 'missing_post')) %>% 
  mutate(type = NA,
         type = ifelse(btw_q_pre == btw_q_post, 'Same', 'Other'),
         type = ifelse(btw_q_post == 'missing_post', 'Missing', type)) %>% 
  group_by(btw_q_pre, type) %>% 
  summarise(n = n(), .groups = "drop")

p_data$type <- factor(p_data$type, levels = c("Other", "Missing", "Same"))
p_data$btw_q_pre <- factor(as.character(p_data$btw_q_pre), levels = as.character(10:1),
                           labels = c("10 (high)", 9:2, "1 (low)"))

p_btw_q <- p_data %>% 
  ggplot() + 
  geom_bar(aes(x = btw_q_pre, y = n, fill = type), stat="identity") + 
  scale_fill_manual(values = c('Other' = 'grey', 'Same' = 'black', 'Missing' = '#C80000')) + 
  theme_classic() + 
  labs(y = "Number of edges", x = "Pre-intervention centrality quantile", fill = NULL,
      title="d")

# Plot percent difference in cumulative journey distance
pre_dist <- pre_network %>% 
  mutate(distance = as.numeric(units::set_units(st_length(geometry), "km")),
         period = "Pre")

post_dist <- post_network %>% 
  mutate(distance = as.numeric(units::set_units(st_length(geometry), "km")),
         period = "Post")

network_distance <- rbind(pre_dist, post_dist) %>% 
  st_drop_geometry() %>% 
  as_tibble()

p_dist <- network_distance %>% 
  mutate(bin = as.integer(cut(network_distance$distance, seq(min(network_distance$distance), max(network_distance$distance) + 20, 20), right = FALSE)),
         bin = bin * 20) %>%
  mutate(total_distance = distance * weight) %>% 
  group_by(period, bin) %>% 
  summarise(total_distance = sum(total_distance, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = period, values_from = total_distance) %>% 
  replace_na(list(Pre = 0, Post = 0)) %>% 
  mutate(diff = ((Post - Pre) / Pre) * 100) %>% 
  arrange(bin) %>% 
  ggplot() + 
  geom_point(aes(x = bin, y = diff), size = 0.5) + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.1) + 
  geom_hline(aes(yintercept = -100), linetype = 'dashed', size = 0.1) +
  geom_hline(aes(yintercept = 100), linetype = 'dashed', size = 0.1) + 
  theme_classic() + 
  theme(plot.margin = margin(t = 0, r = 2.8, b = 0, l = 0, unit = 'cm')) + 
  ggtitle('f') + 
  ylab('Difference (%)') + 
  xlab('Journey Distance (km)')

# Plot betweenness vs distance
network_distance$period <- factor(network_distance$period, levels = c("Pre", "Post"), 
                                  labels = c("Pre-intervention", "Intervention"))
p_dist_btw <- network_distance %>% 
  ggplot()  + 
  geom_point(aes(x = distance, y = betweenness), size = 0.1) + 
  scale_x_continuous(trans="log10") + 
  scale_y_continuous(trans="log10", labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 1)) + 
  annotate("rect", xmin = 90, xmax = 850, ymin = 10000, ymax = 1000000,
           alpha = .2, fill = "red") + 
  facet_wrap(~period) + 
  theme_classic() + 
  ggtitle('e') + 
  labs(x = "Distance (km)", y = "Edge Betweenness")


# Plot volume of travel outside of tile 
p <- c(0.05, 0.95, 0.25, 0.75, 0.4, 0.6, 0.5)

p_names <- c('lower_90', 'upper_90', 'lower_50', 'upper_50', 'lower_20', 'upper_20', 'median')

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

dens_data <- mob %>% 
  mutate(within = start_quadkey == end_quadkey) %>% 
  group_by(date, start_quadkey, within) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), 
            .groups = "drop") %>% 
  mutate(within = ifelse(within, "Within", "Between")) %>% 
  pivot_wider(names_from = within, values_from = n_crisis) %>% 
  drop_na(Between) %>% 
  mutate(perc_leaving = (Between / (Between + Within))) %>% 
  group_by(date) %>% 
  summarize_at(vars(perc_leaving), p_funs) %>% 
  ungroup()

color <- '#173B72'

p_dens <- dens_data %>% 
  ggplot() + 
  geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90), fill = color, alpha = 0.3) + 
  geom_ribbon(aes(x = date, ymin = lower_50, ymax = upper_50), fill = color, alpha = 0.3) + 
  geom_ribbon(aes(x = date, ymin = lower_20, ymax = upper_20), fill = color, alpha = 0.3) + 
  theme_classic() + 
  scale_y_continuous(labels = scales::percent) + 
  ylab('Leaving Cell (%)') + 
  ggtitle('c') + 
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(t = 0.1, r = 2.8, b = 0, l = 0, unit = 'cm'))

p_row <- cowplot::plot_grid(p_dens, p_btw_q, p_dist, ncol = 1)

p_comp <- cowplot::plot_grid(p_network, p_dist_btw, nrow = 2, rel_heights = c(0.65, 0.35))

p <- cowplot::plot_grid(p_comp, p_row, nrow = 1, rel_widths = c(0.6, 0.4))

ggutils::ggsave_png_pdf(p, 
                        here("output/figs/network_overview.png"),
                        14, 9.5)

# Summary metrics comparing networks

pre_g <- igraph::graph_from_data_frame(
  pre_network %>% select(start_quadkey, end_quadkey, weight) %>% 
    st_drop_geometry() %>% as_tibble()
  )

post_g <- igraph::graph_from_data_frame(
  post_network %>% select(start_quadkey, end_quadkey, weight) %>% 
    st_drop_geometry() %>% as_tibble()
)

pre_joureys <- pre_network$journey %>% unique()
post_journeys <- post_network$journey %>% unique()

setdiff(pre_joureys, post_journeys)

post_network %>% 
  filter(journey %in% setdiff(post_journeys, pre_joureys)) %>% 
  mutate(distance = as.numeric(units::set_units(st_length(geometry), "km"))) %>% pull(distance) %>% max


average.path.length(pre_g)
average.path.length(post_g)

round(mean(degree(pre_g)), 2) - round(mean(degree(post_g)), 2)
components(pre_g)$csize %>% length()
components(post_g)$csize %>% length()
