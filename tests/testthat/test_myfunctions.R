context("Test")
source("../../R/functions.R")

test_that("Test prepare_dates", {
  tiffs <- c("SVDNB_npp_20190101-20190131_75N060W_vcmslcfg_v10_c201905201300.avg_rade9h_2100.tif",
  "SVDNB_npp_20190201-20190228_75N060W_vcmslcfg_v10_c201903110900.avg_rade9h_2100.tif",
  "SVDNB_npp_20190301-20190331_75N060W_vcmslcfg_v10_c201904071900.avg_rade9h_2100.tif")
  
  expect_type(prepare_dates(tiffs), "double") 
  expect_is(prepare_dates(tiffs), "Date")

  expect_gt(length(prepare_dates(tiffs)), 0)
})


test_that("Test tiffs_to_stack", {
  tiffs <- c("../../data/tiffs/SVDNB_npp_20140101-20140131_75N060W_vcmslcfg_v10_c2015006171539.avg_rade9h_2100.tif"
  ,"../../data/tiffs/SVDNB_npp_20140201-20140228_75N060W_vcmslcfg_v10_c201507201053.avg_rade9h_2100.tif" 
  ,"../../data/tiffs/SVDNB_npp_20140301-20140331_75N060W_vcmslcfg_v10_c201506121552.avg_rade9h_2100.tif")
  dates <- prepare_dates(tiffs)
  
  expect_gt(length(tiffs_to_stack(tiffs,dates)), 0)
  expect_is(tiffs_to_stack(tiffs,dates), "RasterStack")
  expect_type(tiffs_to_stack(tiffs,dates), "S4")
  expect_true(any(!is.na(raster::getZ(tiffs_to_stack(tiffs,dates)))))
 

})
