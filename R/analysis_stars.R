library(stars)
library(here)
library(lubridate)
library(ggplot2)
library(dplyr)
library(raster)
library(gglorenz)
library(ineq)
library(viridis)

#### Read settings ####
cfg <- config::get(file = here::here("R", "config.yml"))
options(pillar.sigfig = 7)


source(here::here(".", cfg$R_DIR, "functions.R"))
source(here::here(".", cfg$R_DIR, "gas_flares.R"))
source(here::here(".", cfg$R_DIR, "inequalities.R"))
source(here::here(".", cfg$R_DIR, "mean_center.R"))

#### Stack ####
dnb_files <- list.files(here::here(cfg$DATA_DIR, cfg$TIFFS_DIR),
                        pattern = "^SVDNB_npp_201*.*avg_rade9h_2100.tif$",
                        full.names = T)
mydates   <- prepare_dates(dnb_files)
dnb_stack <- tiffs_to_stack(dnb_files, mydates)

#gas flare coordinates
xlsx_files <- list.files(here::here(cfg$DATA_DIR, cfg$GASFLARE_DIR),
                         pattern = "^VIIRS_Global_flaring_*.*xlsx$",
                         full.names = T)

#get coordinates from gas flares xlsx files
coords <- get_flares_coordinates(xlsx_files)

#set gas flare cells to NA
dnb_stack <- mask_dnb_gas_flares(coords,dnb_stack ) #todo: sometimes! returns incorrect number of dimensions (???)


#### Stars ####
mystar <- st_as_stars(dnb_stack) %>%
  st_set_dimensions("band", values = mydates, names = "date")

names(mystar) <- "DNB"
mystar[mystar <= 2] <- 0 # apply filter  https://www.sciencedirect.com/science/article/pii/S0198971517303113#t0010
# αυτός το έχει 3nanoWatt https://www.tandfonline.com/doi/full/10.1080/2150704X.2014.890758


#### Διοικητικές ενότητες (vector data)  ####
admin_units <- load_admin_units(path=cfg$DHMOI, columns_to_delete=c("PREF_ID", "PREFECTURE", "EDRA"))

AGGREGATOR <- sum  # μπορεί να είναι και mean, max, min, length(αντί για count). Με το sum υπολογίζουμε το SoL
df <- calc_aggregator_per_admin_unit(mystar, admin_units, AGGREGATOR)



# index_max = function(x) ifelse(all(is.na(x)), NA, which.max(x))
# a<-st_apply(dnb_admin_units, "geometry", index_max)


#### Δεικτές Σταθάκη ####
#### σωστό ######
nyears <- length(dplyr::distinct(df, years)$years)
dhmoi <- df %>%
  dplyr::group_by(code, month) %>%
  dplyr::summarise(mean_SoL = mean(SoL, na.rm = T))  %>% # calculate average SoL per month
  dplyr::mutate(median_SoL = median(mean_SoL, na.rm = T)) %>% # median of mean SoL of months
  dplyr::left_join(admin_units, by = c("code" = "FID")) %>% # Join to get names of regions and geometry, Join with *tmp* to avoid geometry
  dplyr::mutate_at(vars(month), list(month = ~ ordered(month, levels = c(month.name)))) %>% #convert month to ordered factor
  dplyr::mutate(mean_SoL = replace(mean_SoL, mean_SoL < median_SoL / nyears, NA)) %>% # 1 rule of filtering
  dplyr::mutate(mean_SoL = replace(mean_SoL, mean_SoL > (2 * median_SoL), NA)) %>%  # 2 rule of filtering
  dplyr::group_by(code) %>% # 3 rule of filtering
  dplyr::mutate(mean_SoL = replace(mean_SoL, month == "March" & is.na(mean_SoL), mean_SoL[month == "February"])) %>%  # 3 rule of filtering
  dplyr::mutate(mean_SoL = replace(mean_SoL, month == "March" & is.na(mean_SoL), mean_SoL[month == "April"])) %>% # 3 rule of filtering
  dplyr::group_by(code) %>% # calculate seasonality_coefficient
  dplyr::mutate(seasonality_coefficient = mean_SoL / mean_SoL[month == "March"]) %>% #calculate seasonality_coefficient
  dplyr::arrange(code, month) # arrange data
# dplyr::group_by(code) %>% # calculate peak value & month
# dplyr::mutate(peak_value_S = max(seasonality_coefficient, na.rm=T),
#               peak_month_S = paste(month[which(seasonality_coefficient == max(seasonality_coefficient,na.rm=T))], collapse = ", ")) %>% # calculate peak value & month
# dplyr::group_by(code) %>%  # calculate season length
# dplyr::mutate(season_length = sum(seasonality_coefficient>(seasonality_coefficient[month=="March"]*1.5),na.rm=T))%>% # calculate season length
# dplyr::arrange(code, month)%>% # arrange data
# sf::st_as_sf()# convert it to simple features (sf)

peak <- dhmoi %>%
  dplyr::group_by(code) %>% # calculate peak value & month
  dplyr::summarise(peak_value_S = max(seasonality_coefficient, na.rm = T),
                   peak_month_S = month[which(seasonality_coefficient == max(seasonality_coefficient, na.rm = T))][1])  # calculate peak value & month. Σε περίπτωση που αντιστοιχούν δύο μήνες στην max value κράτα μονο τον πρώτο


season_length <- dhmoi %>%
  dplyr::group_by(code) %>%  # calculate season length
  dplyr::summarise(season_length = sum(seasonality_coefficient > (seasonality_coefficient[month == "March"] * 1.5))) # calculate season length. set na.rm=T στο sum() αν δεν θελω να έχω NA στα season length

final <- peak %>%
  dplyr::left_join(season_length, by = c("code" = "code")) %>%
  dplyr::left_join(admin_units, by = c("code" = "FID")) %>%
  sf::st_as_sf()

ggplot() +
  geom_sf(data = final, aes(fill = as.factor(season_length))) +
  scale_fill_brewer(palette = "Paired", name = "Season Length")

ggsave("Season Length.png",
       plot = last_plot(),
       device = "png",
       path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1,
       width = 30,
       height = 30,
       units = c("cm"),
       dpi = 300,
       limitsize = TRUE)


ggplot() +
  geom_sf(data = final, aes(fill = as.factor(ceiling(final$peak_value_S)))) + # εδώ έχουμε θέμα πρέπει να κανω groups
  scale_fill_brewer(palette = "Paired", name = "Peak value", labels = c("NA", "1-2", "2-3", "3-4", "4-5", "5-6",  "6-7", "7-8", "10-11"))


ggsave("Peak_value.png",
       plot = last_plot(),
       device = "png",
       path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1,
       width = 30,
       height = 30, 
       units = c("cm"),
       dpi = 300,
       limitsize = TRUE)

#https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/#set-gradient-between-n-colors
library(wesanderson)
final2 <- final %>%
          mutate(peak_value_S = replace(peak_value_S, is.infinite(peak_value_S), 0))  #set inf to 0
pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot() +
  geom_sf(data = final2, aes(fill = peak_value_S)) + # εδώ έχουμε θέμα πρέπει να κανω groups
  scale_fill_gradientn(colours = pal)

ggplot() +
  geom_sf(data = final, aes(fill =  peak_month_S)) +
  #scale_fill_brewer(palette = "Paired", name="Peak month")
  scale_fill_manual(name = "Peak month", values = c(
    "#252525", "#636363", "#969696",
    "#74c476", "#31a354", "#006d2c",
    "#fd8d3c", "#e6550d", "#a63603",
    "#fed976", "#ffeda0", "#ffffcc"
  ))
ggsave("Peak month.png", 
       plot = last_plot(), 
       device = "png", 
       path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1, 
       width = 30, 
       height = 30, 
       units = c("cm"),
       dpi = 300,
       limitsize = TRUE)







### Mean centre ####

#### Extract DNB pixels inside admin units as XY points for each month and year ####
admin_units_outline <- st_union(st_geometry(admin_units))
tbl <- dnbstars_to_points(admin_units_outline, mystar)

#### Calculate Weighted Mean Center and Standard Distance per Year ####
wmean_year <- mean_center_year(tbl)

#### Calculate Weighted Mean Center and Standard Distance per month and year ####
wmean_month <- mean_center_month(tbl)

#admin_units_outline_sf <- admin_units_outline %>% st_as_sf

#ggplot (map) mean center per month and year
(mean_center <- plot_mean_center(admin_units_outline, wmean_month,wmean_year))
ggsave("weighted_mean_center.png", plot = mean_center, device = "png", path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1, width = 40, height = 30, units = c("cm"),
       dpi = 300, limitsize = TRUE)


### Inequalities ####
# ggplot Lorenz Curve ####
(lorenz_curve <- plot_lorenz_curve(df))
ggsave("lorenz_curve.png", plot = lorenz_curve, device = "png", path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1, width = 40, height = 30, units = c("cm"),
       dpi = 300, limitsize = TRUE)

#ggplot Gini coefficient #### 
(gini_coefficient <- plot_gini_coefficient(df))
ggsave("gini_coefficient", plot = gini_coefficient, device = "png", path = cfg$GGPLOT_OUTPUT_DIR,
       scale = 1, width = 40, height = 30, units = c("cm"),
       dpi = 300, limitsize = TRUE)



##### Moran's I test: είναι πειραματικός δεν καταλήγει κάπου  #####
##### https://r-spatial.github.io/spdep/reference/knearneigh.html
##### Applied spatial data analysis with R (cite:Bivand2008)
##### https://journals.sagepub.com/doi/suppl/10.1177/2399808318788567/suppl_file/Supplemental_material_1.pdf #κώδικας dstath
library(spdep)
coords <- sf::st_centroid(sf::st_geometry(admin_units), of_largest_polygon=TRUE)
col.knn <- spdep::knearneigh(coords, k=4)
nb <-  spdep::poly2nb(admin_units, queen = T)
