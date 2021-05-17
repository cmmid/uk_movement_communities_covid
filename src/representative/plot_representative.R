#Visualise OA population vs mean FB pop (internal movement) per tile over time periods 
suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(colorspace)
  require(here)
})



if(interactive()){
  .args <-  c(here("data/processed/mob/movement_daily.csv"),
              here("data/processed/pop/tile_12_oa_pop.csv"),
              here("data/processed/geo/tiles.shp"),,
              
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/tile_oa_pop_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

#check if missing data in late September
mob <- read_csv(.args[1], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character())) %>% 
  mutate(date_time = as.Date(date_time)) %>% 
  mutate(journey = paste0(start_quadkey, end_quadkey)) %>% 
  select(journey, date_time, start_quadkey, end_quadkey, n_baseline, n_crisis)

#change because old data is questionable
#mob_old <- read_csv(.args[2], col_types = cols(start_quadkey = col_character(), end_quadkey = col_character())) %>% 
#  mutate(date_time = as.Date(date_time)) %>% 
#  filter(date_time < min(mob$date_time))
#
#mob <- rbind(mob, mob_old)

oa_pop <- read_csv(.args[2], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

tiles <- st_read(.args[3]) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

cases <- readr::read_csv('/Users/hamishgibbs/Downloads/download (15)') %>% 
  filter(countriesAndTerritories == 'United_Kingdom') %>% 
  mutate(dateRep = lubridate::dmy(dateRep))

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')

ylab <- c(13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5)

p_tot <- mob %>% 
  mutate(date_time = as.Date(date_time)) %>% 
  group_by(date_time) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = date_time, y = n_crisis)) + 
  geom_vline(aes(xintercept = as.Date('2020-03-23'))) +
  annotate("text", x = as.Date('2020-03-23') + 2, y = 14e6, label = "National stay\nat home\norder", hjust = 0) + 
  geom_vline(aes(xintercept = as.Date('2020-05-10'))) +
  annotate("text", x = as.Date('2020-05-10') + 2, y = 15.8e6, label = '"Stay Aware"\nguidance', hjust = 0) + 
  geom_vline(aes(xintercept = as.Date('2020-06-29'))) + 
  annotate("text", x = as.Date('2020-06-29') - 2, y = 13.7e6, label = "Leicester local\nrestrictions", hjust = 1) + 
  geom_vline(aes(xintercept = as.Date('2020-07-04'))) + 
  annotate("text", x = as.Date('2020-07-04') + 2, y = 15e6, label = "Relaxation\nof restrictions", hjust = 0) + 
  ylab('Movements') + 
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab,
                     limits = c(13 * 10^6, 16.5 * 10^6)
  ) + 
  theme_bw() + 
  plot_default_theme + 
  theme(plot.margin = unit(c(0,1.2,0,0), "cm"),
        text = element_text(size = 12),
        axis.title.x = element_blank())

inter_mob <- mob %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(start_quadkey) %>% 
  summarise(n_crisis = median(n_crisis, na.rm = T), .groups = 'drop') %>% 
  left_join(oa_pop, by = c('start_quadkey' = 'quadkey_12')) %>% 
  drop_na(pop) %>% 
  mutate(pop_ratio = n_crisis / pop)

p_dens_data <- inter_mob %>% 
  left_join(a3, by = c('start_quadkey' = 'quadkey')) %>% 
  drop_na(NAME_1) 

levs <- p_dens_data %>% group_by(NAME_1) %>% summarise(m = median(pop_ratio * 100)) %>% arrange(-m) %>% pull(NAME_1)

p_dens_data$NAME_1 <- factor(p_dens_data$NAME_1, levels = levs)

#needs arrow pointing to median
p_dens <- p_dens_data %>% 
  ggplot() + 
  geom_vline(aes(xintercept = median(inter_mob %>% pull(pop_ratio))), linetype = 'dashed', size = 0.1) + 
  geom_density(aes(x = pop_ratio * 100, group = NAME_1, color = NAME_1)) + 
  scale_color_manual(values = c('#b15928', '#1f78b4', '#ff7f00', '#33a02c')) + 
  labs(color = 'Country') + 
  ylab('Frequency') + 
  xlab('Percent Population') +
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = c(0.65, 0.7),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0, 'cm'))) + 
  ggtitle('b')

p_pt <- inter_mob %>% 
  ggplot() + 
  geom_point(aes(x = log(pop, 10), y = log(n_crisis, 10)), size = 0.01, alpha = 0.4) + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 'dashed', size = 0.5) + 
  geom_hline(aes(yintercept = log(10, 10)), linetype = 'dashed', size = 0.2, color = 'red') + 
  annotate("text", x = 2, y = 0.6, label = 'Censoring threshold', hjust = 0) + 
  xlim(0, 6) + 
  ylim(0, 6) + 
  ylab(expression(N~Facebook~users~log[10])) + 
  xlab(expression(Population~log[10])) + 
  #xlab('Population (log)') + 
  theme_bw() + 
  plot_default_theme + 
  theme(plot.margin = unit(c(0,0,0, 0), "cm"), 
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 0.8, 0, 0, 'cm'))) + 
  ggtitle('c')

swindon <- tiles %>% 
  filter(quadkey %in% c(a3 %>% filter(NAME_2 %in% c('Wiltshire', 'Gloucestershire', 'Oxfordshire', 'Berkshire')) %>% pull(quadkey))) %>% 
  st_bbox() %>% 
  st_as_sfc()

high <- tiles %>% 
  left_join(inter_mob, by = c('quadkey' = 'start_quadkey')) %>% 
  filter(pop_ratio > 0.31)

p_map <- tiles %>% 
  left_join(inter_mob, by = c('quadkey' = 'start_quadkey')) %>% 
  drop_na(pop) %>% 
  ggplot() + 
  geom_sf(data = world, size = 0.2, colour = 'black', fill = '#E0E0E0') + 
  geom_sf(data = swindon, fill = 'blue') + 
  geom_sf(aes(fill = pop_ratio), size = 0.05) + 
  geom_sf(data = high, fill = 'red', size = 0.05) + 
  viridis::scale_fill_viridis(direction = -1, limits = c(0, 0.31)) + 
  geom_sf(data = world, size = 0.2, colour = 'black', fill = 'transparent') + 
  xlim(-8, 2) + 
  ylim(50.4, 58.4) +
  labs(fill = 'Facebook\nuser ratio') + 
  theme_bw() + 
  plot_default_theme + 
  theme(legend.position = c(0.8, 0.8),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 10)) + 
  ggtitle('d')

#this needs another panel
title <- cowplot::ggdraw() +
  cowplot::draw_label("a", x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 55))

g <- cowplot::plot_grid(p_dens, p_pt, ncol = 1)

p <- cowplot::plot_grid(g, p_map, rel_widths = c(0.4, 0.6))

p <- cowplot::plot_grid(p_tot, p, rel_heights = c(0.2, 0.8), nrow = 2)

p <- cowplot::plot_grid(title, p, rel_heights = c(0.05, 1), nrow = 2)

ggsave(tail(.args, 1), p,
       width = 8, height = 7.2,
       units = 'in')

ggsave(gsub('png', 'pdf', tail(.args, 1)), p,
       width = 8, height = 7.2,
       useDingbats = F,
       units = 'in')
