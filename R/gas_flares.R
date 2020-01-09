# https://www.ngdc.noaa.gov/eog/viirs/download_global_flare.html
extract_gas_flares <- function(xlsx){
  wb_sheets <-  c("flares_upstream", "flares_downstream_oil", "flares_downstream_gas")
  df_list_read <- wb_sheets %>%
    purrr::map(function(sheet){ # iterate through each sheet name
      readxl::read_excel(xlsx, sheet) %>% 
        dplyr::filter(ISO_Code == "GRC") %>% 
        dplyr::select_if( names(.) %in% c('Latitude', 'Longitude')) 
      
    })
  return(df_list_read)
}


get_flares_coordinates <- function(xlsx_files){
  coords <- sapply(xlsx_files, extract_gas_flares) %>%
    purrr::reduce(rbind) %>% 
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    sf::st_transform( crs = 2100) %>% st_coordinates
  return(coords)
  
}


