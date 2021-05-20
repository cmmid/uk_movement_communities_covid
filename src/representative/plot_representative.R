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
              here("data/processed/geo/tiles.shp"),
              here("data/processed/geo_lu/a3_tile_reference.csv"),
              here("output/figs/tile_oa_pop_comparison.png"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

#check if missing data in late September
mob <- read_csv(.args[1]) %>% 
  mutate(journey = paste0(start_quadkey, "_", end_quadkey)) %>% 
  filter(date <= as.Date("2020-11-01"))

oa_pop <- read_csv(.args[2], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

tiles <- st_read(.args[3]) %>% 
  st_set_crs(4326)

a3 <- read_csv(.args[4])

#cases <- readr::read_csv('/Users/hamishgibbs/Downloads/download (15)') %>% 
#  filter(countriesAndTerritories == 'United_Kingdom') %>% 
#  mutate(dateRep = lubridate::dmy(dateRep))

#ylab <- c(13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5)

p_tot <- mob %>% 
  group_by(date) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            .groups = "drop") %>%
  ggplot() + 
  geom_path(aes(x = date, y = n_crisis / 3)) +  
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.1)) + 
  ylab('Average Movements') + 
  theme_classic() + 
  theme(plot.margin = unit(c(0,1.2,0,0), "cm"),
        text = element_text(size = 12),
        axis.title.x = element_blank())

p_tot

inter_mob <- mob %>% 
  filter(start_quadkey == end_quadkey) %>% 
  group_by(start_quadkey) %>% 
  summarise(n_baseline = median(n_baseline, na.rm = T), .groups = 'drop') %>% 
  left_join(oa_pop, by = c('start_quadkey' = 'quadkey_12')) %>% 
  drop_na(pop) %>% 
  mutate(pop_ratio = n_baseline / pop)

p_dens_data <- inter_mob %>% 
  left_join(a3, by = c('start_quadkey' = 'quadkey')) %>% 
  drop_na(NAME_1) 

levs <- p_dens_data %>% group_by(NAME_1) %>% summarise(m = median(pop_ratio * 100)) %>% arrange(-m) %>% pull(NAME_1)

p_dens_data$NAME_1 <- factor(p_dens_data$NAME_1, levels = levs)

p_dens <- p_dens_data %>% 
  ggplot() + 
  geom_density(aes(x = pop_ratio, group = NAME_1, color = NAME_1)) + 
  scale_x_continuous(trans="log10", labels = scales::percent) + 
  scale_color_manual(values = c('#b15928', '#1f78b4', '#ff7f00', '#33a02c')) + 
  labs(color = 'Country') + 
  ylab('Frequency') + 
  xlab(expression(Percent~Population~log[10])) + 
  theme_classic() + 
  theme(legend.position = c(0.8, 0.7),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0, 'cm'))) + 
  ggtitle('b')

p_dens

p_pt <- inter_mob %>% 
  ggplot() + 
  geom_point(aes(x = pop, y = n_baseline), size = 0.01, alpha = 0.8) + 
  ggpubr::stat_cor(aes(x = pop, y = n_baseline), label.x = 3.5,
                   label.y = 1) + 
  geom_smooth(aes(x = pop, y = n_baseline), method = "lm", color = "blue", size = 0.25) + 
  scale_y_continuous(trans = "log10",
                     labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 0.1)) + 
  scale_x_continuous(trans = "log10",
                     labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 0.1)) + 
  ylab(expression(Facebook~users~log[10])) + 
  xlab(expression(Census~Population~log[10])) + 
  theme_classic() + 
  theme(plot.margin = unit(c(0,0,0, 0), "cm"), 
        text = element_text(size = 12)) + 
        #axis.title.y = element_text(margin = margin(0, 0.8, 0, 0, 'cm'))) + 
  ggtitle('c')

p_pt

p_map <- tiles %>% 
  left_join(inter_mob, by = c('quadkey' = 'start_quadkey')) %>% 
  drop_na(pop) %>% 
  ggplot() + 
  ggutils::plot_basemap(country_size = 0.1) + 
  geom_sf(aes(fill = pop_ratio), size = 0.05) + 
  colorspace::scale_fill_continuous_sequential("Teal", trans = "log10", labels = scales::percent) + 
  ylim(c(50, 58.5)) + 
  xlim(c(-9, 2)) + 
  labs(fill = '% Facebook\nUsers') + 
  theme_void() + 
  theme(legend.position = c(0.8, 0.8),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size = 10)) + 
  ggtitle('d')

p_map

#this needs another panel
title <- cowplot::ggdraw() +
  cowplot::draw_label("a", x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 55))

g <- cowplot::plot_grid(p_dens, p_pt, ncol = 1)

p <- cowplot::plot_grid(g, p_map, rel_widths = c(0.4, 0.6))

p <- cowplot::plot_grid(p_tot, p, rel_heights = c(0.2, 0.8), nrow = 2)

p <- cowplot::plot_grid(title, p, rel_heights = c(0.05, 1), nrow = 2)

ggutils::ggsave_png_pdf(p,
                        tail(.args, 1),
                        9, 8)

