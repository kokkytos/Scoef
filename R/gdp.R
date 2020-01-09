library(here)
library(raster)
library(tibble)
library(dplyr)
library(tidyr)
library(sf)
library(readr)
  

#### Read settings ####
cfg <- config::get(file = here::here("R", "config.yml"))
options(pillar.sigfig = 7)

source(here::here(".", cfg$R_DIR, "functions.R"))


#### NUTS3 ####
nuts_ntl <- sf::st_read(dsn =here::here(cfg$OUTPUT_DIR,"NUTS_RG_01M_2016_4326_LEVL_3_NTL.gpkg"), stringsAsFactors = F)  %>%
            dplyr::filter(CNTR_CODE == "EL") %>% as_tibble()

#Keep Greece only
nuts <- nuts_ntl #%>% dplyr::filter(CNTR_CODE == "EL") %>% as_tibble()

# new column for each year with mean of all months
for (year in c(2014:2017))
{
  SoL_mean  <- paste("SoL_mean",year, sep = "")
  SoL_regex <- paste("SoL_",year, sep = "")
  nuts <- nuts %>% dplyr::mutate(!!SoL_mean := rowMeans(select(., starts_with(SoL_regex)), na.rm = TRUE)) 
}

# mean SoL per year
# nuts <- nuts %>% dplyr::mutate(SoL_mean2014 = rowMeans(select(., starts_with("SoL_2014")), na.rm = TRUE)) 
# nuts <- nuts %>% dplyr::mutate(SoL_mean2015 = rowMeans(select(., starts_with("SoL_2015")), na.rm = TRUE)) 
# nuts <- nuts %>% dplyr::mutate(SoL_mean2016 = rowMeans(select(., starts_with("SoL_2016")), na.rm = TRUE)) 
# nuts <- nuts %>% dplyr::mutate(SoL_mean2017 = rowMeans(select(., starts_with("SoL_2017")), na.rm = TRUE)) 

# Select rows
nuts <- nuts%>% as_tibble() %>% dplyr::select("LEVL_CODE", "NUTS_ID","CNTR_CODE","NUTS_NAME","FID_CODE","SoL_mean2014", "SoL_mean2015","SoL_mean2016","SoL_mean2017")  

nuts <- nuts %>% tidyr::gather(Year, SoL_mean, SoL_mean2014:SoL_mean2017) 
nuts <- nuts %>% dplyr::mutate(Year= as.double(substr(Year,(nchar(Year)+1)-4,nchar(Year))))

### Read GDP
mycsv <- read_csv(here::here(cfg$DATA_DIR, cfg$GDP_DIR, cfg$GDP_CSV),na = c("", "NA", ":")) %>% filter(!is.na(Value))

# Join with NUTS  
nuts <- dplyr::inner_join(nuts, mycsv, by=c("NUTS_ID" = "GEO","Year"="TIME" ), keep=T)  %>% dplyr::rename(GDP=Value) %>% as.data.frame()


#Run linear regression GDP~SoL_mean
lm_model <- lm(GDP~SoL_mean, data=nuts)  





# vectors filenames
vector_files <- list.files(
  here::here(cfg$OUTPUT_DIR),
  pattern = "^NUTS_RG_01M_2016_4326_LEVL_*.*_NTL.gpkg$",
  recursive = F,
  full.names = T
)

# function to estimate GDP
estim_gdp <-function(infile,lm_model){
  
  OUTFILE <- sprintf("%s_%s%s", tools::file_path_sans_ext(infile[1]), "GDP", ".gpkg")


   if(file.exists(here::here(cfg$OUTPUT_DIR, OUTFILE))){
     message(sprintf("%s already exists. Will be deleted.",here::here(cfg$OUTPUT_DIR, OUTFILE)))
     file.remove(here::here(cfg$OUTPUT_DIR, OUTFILE))
   }
          #### NUTS1 ####
   nuts_ntl1 <- sf::st_read(dsn =infile, stringsAsFactors = F) %>% dplyr::filter(CNTR_CODE == "EL") %>% as_tibble()
        
        
        
        #for each month of year estimate GDP
        nuts_gdp <- nuts_ntl1
        # new column for each year with mean of all months
        for (year in c(2014:2017))
        {
          for (month in c(1:12))
          {
            GDP  <- sprintf("GDP_%s_%s",year,month)
            SoL  <- sprintf("SoL_%s_%s",year,month)
            nuts_gdp <- nuts_gdp %>%  dplyr::mutate(!!GDP:=predict(lm_model, data.frame(SoL_mean=!!as.name(SoL)) )) #predict GDP
          }
        }
        
        #convert to sf object and write to disk
        st_sf(
          nuts_gdp,
          agr = NA_agr_,
          sf_column_name = 'geom',
          check_ring_dir = FALSE,
          sfc_last = TRUE
        ) %>% sf::st_write(OUTFILE, delete_dsn=TRUE, delete_layer=T)

}

# for each vector apply estim_gdp function
vectors <- sapply(vector_files,estim_gdp, lm_model)
      
