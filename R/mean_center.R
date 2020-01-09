#### Extract DNB pixels as XY points for each month and year 
dnbstars_to_points <- function(admin_units_outline, starsobj){
  
  mystar_masked <- starsobj[admin_units_outline]
  tbl <- st_coordinates(mystar_masked) %>%
    mutate(dnb = as.vector(mystar_masked[["DNB"]])) %>%
    mutate(year = lubridate::year(date)) %>%
    mutate(minas = lubridate::month(date)) %>%
    dplyr::filter(dnb > 0) %>%
    as_tibble()
  return(tbl)
}


#### Calculate Weighted Mean Center and Standard Distance per Year
mean_center_year <- function(tbl){
  wmean_year <- tbl %>% 
    dplyr::group_by(year) %>%
    dplyr::mutate(wX = sum(x*dnb) / sum(dnb), wY = sum(y * dnb) / sum(dnb)) %>%
    dplyr::mutate(Swd = sqrt((sum(dnb * ((x - wX) ^ 2)) +  
                                sum(dnb * ((y - wY) ^ 2))) / n())) %>%
    dplyr::distinct(year, Swd, wX, wY) %>%
    dplyr::mutate(minas = as.factor("year")) %>%
    st_as_sf(coords = c("wX", "wY"), crs = 2100, remove = F)
  return(wmean_year)
  
}


#### Calculate Weighted Mean Center and Standard Distance per month and year
mean_center_month <- function(tbl){
  wmean_month <-  tbl %>%  
    dplyr::mutate_at(vars(minas), list(~ordered(month.name[minas], levels= c( month.name)) )) %>%
    dplyr::group_by(year, minas) %>%
    dplyr::mutate(wX = sum(x*dnb)/sum(dnb), wY = sum(y*dnb) / sum(dnb)) %>%
    dplyr::mutate(Swd = sqrt((sum(dnb * ((x - wX) ^ 2)) +  sum(dnb * ((y - wY) ^ 2))) / n())) %>%
    dplyr::group_by(year, minas) %>%
    dplyr::distinct(year, minas, wX, wY, Swd) %>%
    st_as_sf(coords = c("wX", "wY"), crs = 2100, remove = F)
  return(wmean_month)
}

#ggplot (map) mean center per month and year
plot_mean_center <- function(admin_units, wmean,wmean_year){
  theme_set(theme_bw())
  offset <- 5000
  ggplot() +
    geom_sf(data = admin_units) +
    geom_point(data = wmean, aes(x = wX, y = wY, fill = as.factor(minas)), pch = 21, size = 2) +
    scale_fill_manual(name = "Month", values = c(
      "#252525", "#636363", "#969696",
      "#74c476", "#31a354", "#006d2c",
      "#fd8d3c", "#e6550d", "#a63603",
      "#fed976", "#ffeda0", "#ffffcc"
    )) +
    geom_point(data = wmean_year, aes(x = wX, y = wY, shape = as.factor(13)), size = 3) +
    scale_shape_manual(name = "", values = c(13), labels = c("Year")) +
    facet_grid(. ~ year) +
    coord_sf(xlim = c(min(wmean$wX - offset), max(wmean$wX + offset)), ylim =  c(min(wmean$wY - offset), max(wmean$wY + offset))) +
    ggtitle("Weighted mean center") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Longitude")  + ylab("Latitude")  
}
