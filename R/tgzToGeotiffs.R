library(here)
library(raster)

####====== Read settings ========================================== ####

cfg <- config::get(file=here::here("R", "config.yml"))

wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
greekgrid <- "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +ellps=GRS80 +towgs84=-199.87,74.79,246.62,0,0,0,0 +units=m +no_defs"


DATA_DIR  <- cfg$DATA_DIR
TGZ_DIR   <- cfg$TGZ_DIR
TIFFS_DIR <- cfg$TIFFS_DIR
####=============================================================== ####



#list tgz files
tar_files <- list.files(here::here(DATA_DIR, TGZ_DIR), pattern = "*.tgz")



raster_generator <- function(huge_geotiff,geotiff_fn){

      r_template <- raster::raster()
      #attika: c(379354,529354,4132181,4282181) 
      ##greece: c(379354,529354,4132181,4282181) 
      raster::extent(r_template) <- c(cfg$EXTENT$X_MIN, #greece      
                              cfg$EXTENT$X_MAX,
                              cfg$EXTENT$Y_MIN,
                              cfg$EXTENT$Y_MAX)
      
      raster::res(r_template) <- cfg$EXTENT$RESOLUTION
      raster::projection(r_template) <- sp::CRS(greekgrid)
      
      
      
      r <- raster::raster(huge_geotiff)
      raster::projection(r) <- sp::CRS(wgs84)
      
      r_2100 <- raster::projectRaster(from=r, 
                              to=r_template,
                              method="ngb") 
      
      
      
      
      message(sprintf("Writing geotiff %s",geotiff_fn))
      raster::writeRaster(r_2100, 
                          filename=geotiff_fn,
                          datatype="FLT4S", 
                          options="COMPRESS=LZW", 
                          format="GTiff",
                          overwrite=T)
      
      # delete huge geotiff
                
      message(sprintf("Successfully generated file:%s",geotiff_fn))
      
  
}



# function to uncompress files
i <- 1
errors <- vector()
processTarFiles <- function(tarfile){
  fn_without_extension <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tarfile))
  #full raster
  huge_geotiff <- here::here(DATA_DIR,TIFFS_DIR,sprintf("%s.avg_rade9h.tif",fn_without_extension))
  #new
  geotiff_fn <- here::here(DATA_DIR,TIFFS_DIR,sprintf("%s_%s.tif",basename(tools::file_path_sans_ext(huge_geotiff)),"2100"))
  
  #uncompress file
  if(!file.exists(geotiff_fn)){
    message(sprintf("Uncompressing %s",tarfile))
    if (!file.exists(huge_geotiff)){
      #try to extract file
      code <- untar(here::here(DATA_DIR,TGZ_DIR,tarfile),exdir=here::here("data","tiffs"),extras = " --wildcards --no-anchored '*avg_rade9h*' ")
      
      if (code == 0){ # succesfull extraction code=0
        #export to geotiff 
        raster_generator(huge_geotiff,geotiff_fn)
        
        
      }
      else{ #error, corrupted file
        message(sprintf("Error: %s",tarfile))
        errors[i] <<- tarfile
        i <<- i+1
      }
      #Delete extracted file  
      message(sprintf("Delete %s",huge_geotiff))
      unlink(huge_geotiff)
    }
    
  }
  else{ # reprojected/cropped geotiff already exists 
    message(sprintf("%s already exists",geotiff_fn))
  }
  
  
  
}


#uncompress files
invisible(sapply(tar_files,processTarFiles ))

# write errors to file
if (length(errors)>0){
  fileConn< - file(here::here(DATA_DIR,TIFFS_DIR,"errors.txt"))
  writeLines(errors, fileConn)
  close(fileConn)
}
