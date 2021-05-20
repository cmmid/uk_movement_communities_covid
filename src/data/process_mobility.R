require(dplyr)

#Combine raw mobility files into a single dataset
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/raw/Britain_TileMovement",
              here::here("data/processed/mob/movement_hourly.csv"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

files <- list.files(.args[1], full.names = T)

files <- files[stringr::str_detect(files, "2020")]

read_file <- function(fn){
  
  data <- readr::read_csv(fn, col_types = readr::cols()) %>% 
    select(date_time, start_quadkey, end_quadkey, n_crisis, n_baseline) %>% 
    dplyr::mutate(start_quadkey = stringr::str_trim(format(start_quadkey, scientific = F)),
                  start_quadkey = stringr::str_pad(as.character(start_quadkey), 12, "left", pad="0"),
                  end_quadkey = stringr::str_trim(format(end_quadkey, scientific = F)),
                  end_quadkey = stringr::str_pad(as.character(end_quadkey), 12, "left", pad="0"))
  
  return(data)
  
}

mob <- lapply(files, read_file)

mob <- do.call(rbind, mob)

readr::write_csv(mob, .args[2])

aggregate_mobility <- function(data, period){
  
  aggregated <- data %>% 
    dplyr::mutate(date = lubridate::floor_date(date_time, period)) %>% 
    dplyr::group_by(date, start_quadkey, end_quadkey) %>% 
    summarise(n_crisis = sum(n_crisis, na.rm = T), 
              n_baseline = sum(n_baseline, na.rm = T), 
              .groups = "drop")
  
  return(aggregated)
  
}

readr::write_csv(aggregate_mobility(mob, "day"), gsub("hourly", "daily", .args[2]))
readr::write_csv(aggregate_mobility(mob, "week"), gsub("hourly", "weekly", .args[2]))
readr::write_csv(aggregate_mobility(mob, "month"), gsub("hourly", "monthly", .args[2]))
