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
              here("data/processed/geo/tiles.shp"),
              here("data/processed/pop/tile_12_oa_pop.csv"),
              here("data/raw/geo/Travel_to_Work_Areas__December_2011__Boundaries-shp/Travel_to_Work_Areas__December_2011__Boundaries.shp"),
              here("data/raw/geo/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp"),
              here("output/figs/tile_oa_pop_comparison.png"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

#check if missing data in late September
tiles <- st_read(.args[2]) %>% 
  st_set_crs(4326)

oa_pop <- read_csv(.args[3], col_types = cols()) %>% 
  mutate(quadkey_12 = str_pad(quadkey_12, 12, pad = "0"))

im <- read_csv(.args[1]) %>% 
  mutate(quadkey = stringr::str_pad(quadkey, 12, "left", "0"),
         date = as.Date(date)) %>% 
  filter(date <= as.Date("2020-11-01"),
         quadkey %in% c(oa_pop$quadkey_12))

ttwas <- st_read(.args[4]) %>% 
  st_simplify(dTolerance = 150, preserveTopology = T)

las <- st_read(.args[5]) %>% 
  st_simplify(dTolerance = 150, preserveTopology = T)

tile_area <- tiles %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  st_drop_geometry() %>% 
  as_tibble()


tiles %>% 
  left_join(im %>% filter(date == min(date)), by = c("quadkey")) %>% 
  drop_na(cluster) %>% 
  group_by(cluster) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_sf(aes(fill = as.character(cluster))) + 
  theme(legend.position = "none")


## Typical area
im_area <- im %>% 
  left_join(tile_area, by = c("quadkey")) %>% 
  group_by(date, cluster) %>% 
  summarise(area = sum(area, na.rm = T), .groups="drop") %>% 
  group_by(date) %>% 
  summarise(avg = mean(area, na.rm = T),
            med = median(area, na.rm = T), .groups="drop")

im_area <- im_area %>% 
  pivot_longer(!date)

color_scale <- scale_color_manual(values = c("avg" = "black", "med" = "red"), labels = c("avg" = "Average", "med" = "Median"))

p_area <- im_area %>% 
  ggplot() + 
  geom_path(aes(x = date, y = value, color = name), size = 0.3) + 
  labs(y = "Area (km^2)", title = "a", color=NULL) + 
  color_scale + 
  theme_classic() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = 'cm')))

p_area 

## Typical population size
im_pop <- im %>% 
  left_join(oa_pop, by = c("quadkey" = "quadkey_12")) %>% 
  group_by(date, cluster) %>% 
  summarise(pop = sum(pop, na.rm = T), .groups="drop") %>% 
  group_by(date) %>% 
  summarise(avg = mean(pop, na.rm = T),
            med = median(pop, na.rm = T), .groups="drop")

im_pop <- im_pop %>% 
  pivot_longer(!date)

p_pop <- im_pop %>% 
  ggplot() + 
  geom_path(aes(x = date, y = value, color = name), size = 0.3) + 
  scale_y_continuous(labels = scales::comma) + 
  color_scale + 
  labs(y = "Population", title = "b", color = NULL) + 
  theme_classic() + 
  theme(axis.title.x = element_blank())

p_pop

legend <- cowplot::get_legend(p_pop)

p_plots <- cowplot::plot_grid(p_area + theme(legend.position = "none"), 
                        p_pop + theme(legend.position = "none"), ncol = 1)

p <- cowplot::plot_grid(p_plots, legend, rel_widths = c(0.8, 0.2))

ggutils::ggsave_png_pdf(p, here("output/figs/communities_typical_size.png"))

p_n <- im %>% 
  group_by(date) %>% 
  summarise(n = length(unique(cluster))) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n), size = 0.3) + 
  ylim(180, 320) + 
  theme_classic() + 
  labs(y = "Number of communities", x = NULL)

ggutils::ggsave_png_pdf(p_n, here("output/figs/communities_number.png"),
                        8, 3)

## Do these sizes correspond to LAs 
ttwas %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  pull(area) %>% mean()


## Do these sizes correspond to Travel to work areas
las %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  pull(area) %>% mean()

im %>% 
  left_join(tile_area) %>% 
  group_by(date, cluster) %>% 
  summarise(area = sum(area, na.rm = T)) %>% 
  pull(area) %>% mean()
  

plot_style <- list(
  ggutils::plot_basemap(country_size = 0.1),
  ylim(c(50, 58.5)), 
  xlim(c(-9, 2)),
  theme_void()
)

p_ttwa <- ttwas %>% 
  st_transform(4326) %>% 
  ggplot() + 
  geom_sf(size = 0.2) + 
  plot_style + 
  ggtitle("Travel to work areas")

p_las <- las %>% 
  st_transform(4326) %>% 
  ggplot() + 
  geom_sf(size = 0.2) + 
  plot_style + 
  ggtitle("Lower Tier Local Authority")

p_comm <- tiles %>% 
  left_join(im %>% filter(date == min(date))) %>% 
  drop_na(cluster) %>% 
  group_by(cluster) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_sf(size = 0.2) + 
  plot_style + 
  ggtitle("Infomap communities (2020-03-10)")

p <- cowplot::plot_grid(p_ttwa, p_las, p_comm, nrow = 1)

ggutils::ggsave_png_pdf(p, here("output/figs/las_ttwa_comm.png"),
                        10, 5)

im %>% filter(date == min(date)) %>% pull(cluster) %>% unique() %>% length
las %>% pull(1) %>% length()
ttwas %>% pull(1) %>% length()

msoa <- st_read("/Users/hamishgibbs/Documents/Covid-19/archive/uk_demographic_mobility/data/raw/msoa_centroids/Middle_Layer_Super_Output_Areas__December_2011__Population_Weighted_Centroids.shp")

msoa %>% 
  mutate(country = stringr::str_sub(msoa11cd, 1, 1)) %>% 
  filter(country == "E")

tile_area %>% 
  left_join(im %>% filter(date == min(date))) %>% 
  drop_na(cluster) %>% 
  group_by(cluster) %>% 
  summarise(area = sum(area, na.rm = T)) %>% 
  pull(area) %>% mean()
