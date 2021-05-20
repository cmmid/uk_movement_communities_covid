#scritp to compare overlap of communities from different algorithms
#using node membership - not spatial extent

suppressPackageStartupMessages({
  require(RColorBrewer)
  require(tidyverse)
  require(sf)
})

source('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/visualization/utils/plot_default_theme.R')

if(interactive()){
  .args <-  c('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_leiden_test_norm.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/lei_im_s_to_l.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_descriptive/lei_im_l_to_s.csv',
              '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.png')
} else {
  .args <- commandArgs(trailingOnly = T)
}

im <- read_csv(.args[1]) %>% 
  group_by(date) %>% 
  group_split()

lei <- read_csv(.args[2]) %>% 
  group_by(date) %>% 
  group_split()

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

letter_title <- function(letter){
  return(cowplot::ggdraw() + cowplot::draw_label(letter, x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 7, 21)))
}

results_s_to_l_df <- read_csv(.args[3])

results_l_to_s_df <- read_csv(.args[4])


pl <- list()
pl[[1]] <- results_s_to_l_df %>% group_by(date) %>% 
  summarise(max_overlap = mean(max_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = max_overlap)) + 
  ylim(0, 1) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ylab('Mean % overlap with Leiden modules') + 
  ggtitle('Infomap to Leiden')

pl[[1]] <- cowplot::plot_grid(letter_title('a'), pl[[1]], rel_heights = c(0.05, 1), nrow = 2)

pl[[2]] <- results_s_to_l_df %>% group_by(date) %>% 
  summarise(n_overlap = mean(n_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ylab('Mean overlapping communities') + 
  ggtitle('Infomap to Leiden')

pl[[2]] <- cowplot::plot_grid(letter_title('b'), pl[[2]], rel_heights = c(0.05, 1), nrow = 2)

pl[[3]] <- results_l_to_s_df %>% group_by(date) %>% 
  summarise(n_overlap = mean(n_overlap)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) + 
  ylab('Mean overlapping communities') + 
  ggtitle('Leiden to Infomap') + 
  ylim(1, 3.2)

pl[[3]] <- cowplot::plot_grid(letter_title('c'), pl[[3]], rel_heights = c(0.05, 1), nrow = 2)

p1 <- cowplot::plot_grid(plotlist = pl, nrow = 1)

pl2 <- list()

daily_df <- results_s_to_l_df %>% 
  filter(date == min(date)) %>% 
  arrange(max_overlap)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[1]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = max_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank(),
        text = element_text(size = 12)) + 
  xlab('Cluster ID') + 
  ylab('Largest % overlap with Leiden modules') + 
  ggtitle(paste(unique(daily_df$date), ' - Infomap to Leiden'))

pl2[[1]] <- cowplot::plot_grid(letter_title('d'), pl2[[1]], rel_heights = c(0.05, 1), nrow = 2)

daily_df <- results_s_to_l_df %>% 
  filter(date == min(date)) %>% 
  arrange(-n_overlap)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[2]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank(),
        text = element_text(size = 12)) + 
  xlab('Cluster ID') + 
  ylab('Overlapping communities') + 
  ggtitle(paste(unique(daily_df$date), ' - Infomap to Leiden'))

daily_df <- results_l_to_s_df %>% 
  filter(date == min(date)) %>% 
  arrange(n_overlap)

pl2[[2]] <- cowplot::plot_grid(letter_title('e'), pl2[[2]], rel_heights = c(0.05, 1), nrow = 2)

daily_df$cluster <- factor(daily_df$cluster, levels = daily_df$cluster)
pl2[[3]] <- daily_df %>% 
  ggplot() + 
  geom_point(aes(x = cluster, y = n_overlap)) + 
  theme_bw() + 
  plot_default_theme + 
  theme(axis.text.x = element_blank(),
        text = element_text(size = 12)) + 
  xlab('Cluster ID') + 
  ylab('Overlapping communities') + 
  ggtitle(paste(unique(daily_df$date), ' - Leiden to Infomap'))

pl2[[3]] <- cowplot::plot_grid(letter_title('f'), pl2[[3]], rel_heights = c(0.05, 1), nrow = 2)


p1 <- cowplot::plot_grid(plotlist = pl, nrow = 1)
p2 <- cowplot::plot_grid(plotlist = pl2, nrow = 1)

p <- cowplot::plot_grid(p1, p2, nrow = 2)

ggsave(tail(.args, 1), p,
       width = 14, height = 8,
       units = 'in')

ggsave(gsub('.png', '.pdf', tail(.args, 1)), p,
       width = 14, height = 8,
       units = 'in', useDingbats = F)

get_spatial_clusters <- function(comm){
  
  res <- tiles %>% 
    left_join(comm) %>% 
    drop_na(cluster) %>% 
    group_by(cluster) %>% 
    summarise(n = n())  
  
  return(res)
  
}

plot_im_lei <- function(im, lei, title){
  
  comm_im <- get_spatial_clusters(im) %>% 
    mutate(type = "InfoMap")
  comm_lei <- get_spatial_clusters(lei) %>% 
    mutate(type = "Leiden")
  
  print(unique(im$date))
  print(unique(lei$date))
  testthat::expect_equal(unique(im$date), unique(lei$date))
  
  p <- ggplot() + 
    ggutils::plot_basemap(country_size = 0.1) + 
    geom_sf(data = comm_im, fill = "transparent", size = 0.5, color = "red") + 
    geom_sf(data = comm_lei, fill = "transparent", size = 0.2, color = "black") + 
    ylim(c(50, 58.5)) + 
    xlim(c(-9, 2)) + 
    theme_void() + 
    ggtitle(title, unique(im$date))
  
  return(p)  
  
}


p_1 <- plot_im_lei(im[[1]], lei[[1]], "a")
p_2 <- plot_im_lei(im[[68]], lei[[68]], "b")
p_3 <- plot_im_lei(im[[136]], lei[[length(lei)]], "c")


p <- cowplot::plot_grid(p_1, p_2, p_3, nrow = 1)

ggutils::ggsave_png_pdf(p, 
                        here("output/figs/im_lei_map.png"),
                        10, 4)
p

# p_im <- plot_clusters(im[[1]], uk, title = paste0(unique(im[[1]]$date), ' Infomap'))
# p_lei <- plot_clusters(lei[[1]], uk, title = paste0(unique(lei[[1]]$date), ' Leiden'))
# 
# p_im <- cowplot::plot_grid(letter_title('a'), p_im, rel_heights = c(0.05, 1), nrow = 2)
# 
# p_lei <- cowplot::plot_grid(letter_title('b'), p_lei, rel_heights = c(0.05, 1), nrow = 2)
# 
# p <- cowplot::plot_grid(p_im, p_lei)
# 
# ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison_map.png', p,
#        width = 6, height = 6,
#        units = 'in')
# 

# max_ol_clust <- results_l_to_s_df %>% 
#   filter(n_overlap == max(n_overlap))
# 
# lei_intersect <- do.call(rbind, lei) %>% 
#   filter(date == max_ol_clust$date, cluster == max_ol_clust$cluster)
# 
# im_intersect <- do.call(rbind, im) %>% 
#   filter(date == max_ol_clust$date, quadkey %in% lei_intersect$quadkey)
# 
# lei_intersect <- tiles %>% 
#   left_join(lei_intersect) %>% 
#   drop_na(cluster) %>% 
#   mutate(n = 1) %>% 
#   group_by(n) %>% 
#   summarise()
# 
# im_intersect <- tiles %>% 
#   left_join(im_intersect) %>% 
#   drop_na(cluster)
# 
# m <- ggplot() + 
#   geom_sf(data = lei_intersect, fill = 'transparent', color = 'blue', size = 1.5) + 
#   geom_sf(data = im_intersect, aes(fill = cluster), size = 0) + 
#   geom_sf(data = uk, fill = 'transparent', size = 0.1, color = 'black') + 
#   ylim(51.9, 53.5) + 
#   xlim(-0.7, 1.5) +  
#   theme_void() + 
#   theme(legend.position = 'none') + 
#   ggtitle('Example - Leiden module overlapping most IM modules')
# 
# p2 <- cowplot::plot_grid(p, m, ncol = 2, rel_widths = c(0.7, 0.3))
# 
# ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.png', p2,
#        width = 18, height = 6,
#        units = 'in')
# 
# ggsave('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/reports/figures/spatial_comparison.pdf', p2,
#        width = 14, height = 6,
#        units = 'in', useDingbats = F)
