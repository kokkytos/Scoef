library(raster)

prepare_dates <- function(dnb_files) {
    as.Date(sub(".*_(\\d{8}).*-.*", "\\1", dnb_files), format = "%Y%m%d")
}

tiffs_to_stack <- function(dnb_files, mydates) {
    dnb_stack <- raster::stack(dnb_files)
    # dnb_stack <-raster::calc(dnb_stack, fun=function(x) {x[x<=2]<-0; return (x)})
    dnb_stack <- raster::setZ(dnb_stack, mydates)
    return(dnb_stack)
}


load_admin_units <- function(path, columns_to_delete) {
    admin_units <- sf::st_read(path, fid_column_name = "FID", stringsAsFactors = F) %>%
        dplyr::mutate(FID = as.integer(FID)) %>%
        dplyr::mutate(area_m2 = st_area(geom)) %>%
        dplyr::select(-one_of(columns_to_delete))
    return(admin_units)
}


calc_aggregator_per_admin_unit <- function(mystar, admin_units, aggregator) {
    # SoL για κάθε διοικητική ενότητα
    dnb_admin_units <- stats::aggregate(mystar,
                                        by = admin_units,
                                        FUN = aggregator)
    # convert it to dataframe
    df_dnb_admin_units <- dnb_admin_units %>% as.tbl_cube() %>% as.data.frame()

    tmp <- admin_units %>%
        as_tibble() %>%
        dplyr::select(-one_of(c("geom")))  # as tibble, exclude geometry column from vector dataser

    df <- left_join(df_dnb_admin_units, tmp, by = c(geometry = "FID")) %>%
        dplyr::rename(code = geometry, SoL = DNB) %>%
        mutate(month = months(date)) %>%
        mutate(years = lubridate::year(date)) %>%
        as_tibble()

    return(df)
}


mask_dnb_gas_flares <- function(coords, dnb_stack){
    
    cells <- raster::cellFromXY(dnb_stack, coords) #get cells from coordinates
    dnb_stack[cells] <- NA #set gas flare sites to NA
    return(dnb_stack)
}
