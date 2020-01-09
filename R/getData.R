library(rvest)
library(dplyr)
library(stringr)
library(here)
library(curl)
library(raster)
library(rgdal)

#### ====== Read settings ========================================== ####

cfg <- config::get(file = here::here("R", "config.yml"))


NOAA_URL <- cfg$NOAA_URL
DATA_DIR <- cfg$DATA_DIR
TGZ_DIR <- cfg$TGZ_DIR
TIFFS_DIR <- cfg$TIFFS_DIR
#### =============================================================== ####

if (!exists("snakemake")) {
    ROOT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
    setwd(ROOT_DIR)  #for Rscript inside RStudio  
}


# ================ functions ========================================== ####

# extract links https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2
scraplinks <- function(url) {
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    # Extract the link text
    link_ <- webpage %>% rvest::html_nodes("a") %>% rvest::html_text()
    return(data.frame(link = link_, url = url_))
}

# Download files
downloadFile <- function(row) {
    url <- row[2]
    fn <- here::here(DATA_DIR, TGZ_DIR, basename(url))
    
    if (!file.exists(fn)) {
        message(sprintf("Download %s", url))
        curl_download(url = url, destfile = here::here(DATA_DIR, TGZ_DIR, basename(fn)), mode = "wb")
    } else {
        message(sprintf("%s already exists", url))
    }
}


# get all links from page
df <- scraplinks(NOAA_URL)

# keep useful tgz files
df <- df %>% filter(str_detect(url, "_75N060W_vcmslcfg_"))

# download files
apply(df, 1, downloadFile)

message("Data download completed")
