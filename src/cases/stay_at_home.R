# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(ggpubr)
  require(sf)
  require(here)
})

if(interactive()){
  .args <-  c(here("data/processed/mob/movement_daily.csv"),
              here("data/processed/geo/tiles.shp"),
              here("data/raw/geo/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC"),
              here("data/raw/geo/nhs_region/NHS_England_Regions_(April_2020)_Boundaries_EN_BUC.geojson"),
              here("data/processed/pop/tile_12_oa_pop.csv"),
      "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

mob <- read_csv(.args[1]) %>% 
  mutate(start_quadkey = stringr::str_pad(start_quadkey, 12, side='left', '0'),
         end_quadkey = stringr::str_pad(end_quadkey, 12, side='left', '0'))

tiles <- st_read(.args[2]) %>% 
  st_transform(27700)

a3 <- st_read(.args[3]) %>% 
  st_simplify(preserveTopology = T, dTolerance = 75) %>% 
  mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON"))

nhs <- st_read(.args[4]) %>% 
  st_transform(27700) %>% 
  st_simplify(preserveTopology = T, dTolerance = 150)

tile_pop <- read_csv(.args[5]) %>% 
  mutate(quadkey_12 = stringr::str_pad(quadkey_12, 12, side='left', '0'))

mob <- mob %>% 
  filter(start_quadkey %in% tile_pop$quadkey_12 & end_quadkey %in% tile_pop$quadkey_12) %>% 
  mutate(date = as.Date(date))

qks <- unique(c(mob$start_quadkey, mob$end_quadkey))

a3_int <- st_intersection(tiles %>% filter(quadkey %in% qks), a3)

a3_int_top <- a3_int %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>% 
  group_by(quadkey) %>% 
  top_n(1, area) %>% 
  mutate(lad19nm = as.character(lad19nm),
         quadkey = as.character(quadkey)) %>% 
  select(lad19nm, quadkey) %>% 
  st_drop_geometry() %>% 
  mutate(lad19nm = ifelse(lad19nm == "Cornwall", "Cornwall and Isles of Scilly", lad19nm))

a3_pop <- tile_pop %>% 
  left_join(a3_int_top, by = c("quadkey_12" = "quadkey")) %>% 
  drop_na(lad19nm) %>% 
  group_by(lad19nm) %>% 
  summarise(pop = sum(pop, na.rm = T), .groups = "drop")

tile_nhs_int <- st_intersection(tiles %>% filter(quadkey %in% qks), nhs)

tile_nhs_int <- tile_nhs_int %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>% 
  group_by(quadkey) %>% 
  top_n(1, area) %>% 
  st_drop_geometry() %>% 
  select(quadkey, nhser20nm) %>% 
  rename(region = nhser20nm)

cases <- readr::read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv') %>% 
  rename(code = `areaCode`) %>% 
  rename(name = `areaName`) %>% 
  rename(count = `newCasesBySpecimenDate`) %>% 
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>% 
  filter(area_type == 'E0') %>% 
  filter(date >= as.Date('2020-03-10'),
         date < as.Date('2020-11-01'))

cases_month <- cases %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month, name) %>% 
  summarise(count = sum(count, na.rm = T))

region_pal <- c(
  'London' = 'orange',
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
region_pal <- c(
  'London' = '#ffff33',
  'South East' = '#b03060',
  'South West' = '#9880C2',
  'East of England' = '#006400',
  'Midlands' = '#88582C',
  'North East and Yorkshire' = '#ff0000',
  'North East and\nYorkshire' = '#ff0000',
  'North West' = '#6495ed',
  'Northern Ireland' = '#ff00ff', 
  'Wales' = '#ff4500',
  'Scotland' = '#00008b'
)
region_pal <- c('North West' = '#984ea3',
                'North East and Yorkshire' = '#ff7f00',
                'North East and\nYorkshire' = '#ff7f00',
                'Midlands' = '#ffff33',
                'East of England' = '#a65628',
                'South West' = '#f781bf',
                'South East' = '#999999', 
                'London' = '#8dd3c7')

p_data <- mob %>% 
  filter(date < as.POSIXct('2020-11-01')) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(type = ifelse(start_quadkey == end_quadkey, 'within', 'between')) %>% 
  left_join(a3_int_top, by = c('start_quadkey' = 'quadkey')) %>% 
  left_join(tile_nhs_int, by = c('start_quadkey' = 'quadkey')) %>% 
  group_by(month, type, lad19nm) %>% 
  summarise(n_crisis = sum(n_crisis, na.rm = T), 
            region = unique(region)[1],
            .groups = 'drop') %>% 
  pivot_wider(id_cols = c(month, lad19nm, region), names_from = type, values_from = n_crisis) %>% 
  mutate(perc = (between / (between + within) * 100)) %>% 
  left_join(cases_month, by = c('lad19nm' = 'name', 'month')) %>% 
  mutate(month = paste0(month.abb[month], ', 2020')) %>% 
  drop_na(count) %>% 
  left_join(a3_pop, by = c('lad19nm')) %>% 
  mutate(cases_per = count / pop)

p_data$month <- factor(p_data$month, levels = paste0(month.abb, ', 2020'))

plot_cases_mob <- function(data, var, xlab){
  
  p <- data %>% 
    ggplot(aes(x = log(!! sym(var), 10), y = perc)) + 
    geom_point(aes(color = region), size = 0.8, alpha = 0.7) + 
    scale_color_manual(values = region_pal) + 
    geom_smooth(method = 'lm', size = 0.3) + 
    stat_cor(method = "pearson", label.x = -5, label.y = 30, label.sep='\n', size = 2.9) + 
    facet_wrap(~month, nrow = 2) + 
    theme_classic() + 
    ylab('% Travelling between cells') + 
    xlab(xlab) + 
    labs(color = 'NHS Region') + 
    theme(strip.background = element_blank(),
          strip.text = element_text(angle = 0, hjust = 0, margin = margin(0, 0, 0, 8), face = 'bold'),
          legend.position = 'none',
          axis.title.y = element_text(margin = margin(t = 0, r = 0.6, b = 0, l = 0, unit = 'cm'))) + 
    ggtitle("b")
  
  
  return(p)  
}

p_raw <- plot_cases_mob(p_data, "count", bquote('Cases '~(log[10])))
p_percap <- plot_cases_mob(p_data, "cases_per", bquote('Cases per capita '~(log[10])))

# cases plot

a3_pre <- a3 %>% 
  select(lad19nm) %>% 
  st_make_valid()

nhs_pre <- nhs %>% 
  select(nhser20nm) %>% 
  st_make_valid()

a3_nhs_int <- st_intersection(a3_pre, nhs_pre) %>% 
  st_drop_geometry()

p_cases <- cases %>% 
  left_join(a3_nhs_int, by = c('name' = 'lad19nm')) %>% 
  group_by(date, nhser20nm) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  drop_na(nhser20nm) %>% 
  mutate(nhser20nm = as.character(nhser20nm),
         nhser20nm = ifelse(nhser20nm == 'North East and Yorkshire', 'North East and\nYorkshire', nhser20nm)) %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = count, fill = nhser20nm), stat = 'identity') + 
  scale_fill_manual(values = region_pal) + 
  facet_wrap(~nhser20nm, nrow = 1) + 
  theme_classic() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0, margin = margin(0, 0, 0, 2), face = 'bold')) + 
  ylab('Confirmed Cases') + 
  ggtitle("a")

p <- cowplot::plot_grid(p_cases, p_raw, ncol = 1, rel_heights = c(0.3, 0.7))

ggutils::ggsave_png_pdf(p,
                        here("output/figs/mob_cases.png"),
                        10, 6)


p <- cowplot::plot_grid(p_cases, p_percap, ncol = 1, rel_heights = c(0.3, 0.7))

ggutils::ggsave_png_pdf(p,
                        here("output/figs/mob_cases_percap.png"),
                        10, 6)

cases 
a3_int_top

a3_pop <- tile_pop %>% 
  filter(hour == 8) %>% 
  left_join(a3_int_top, by = c('quadkey_12' = 'quadkey')) %>% 
  group_by(lad19nm) %>% 
  summarise(pop = sum(pop, na.rm = T))

cases_pop <- cases_month %>% 
  left_join(a3_pop, by = c('name' = 'lad19nm')) %>% 
  mutate(month = paste0(month.abb[month], ', 2020'))
  
cases_pop$month <- factor(cases_pop$month, levels = paste0(month.abb, ', 2020'))

cases_pop %>% 
  ggplot() + 
  geom_point(aes(x = log(pop, 10), y = log(count, 10)), size =0.2) + 
  facet_wrap(~month, scales = 'free', nrow = 2) + 
  theme_classic() + 
  ylab('Cases') + 
  xlab('Population')
