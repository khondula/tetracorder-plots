# function to crop raster and drop layers with no values

# my_rast <- mineral_agg2
# my_ext <- terra::ext(mineral_agg2)

crop_n_drop <- function(my_rast, my_ext){

  my_rast_crop <- terra::crop(my_rast, my_ext)
  
  chk_vals <- function(rast_lyr_name){
    rast_lyr <- my_rast_crop[[rast_lyr_name]]
    max_val <- terra::values(rast_lyr) %>% unique() %>% max(na.rm = TRUE)
    max_val > 0
  }
  
  lyrs_to_keep <- purrr::map_lgl(names(my_rast_crop), ~chk_vals(.x))
  
  my_rast_crop_sub <- terra::subset(my_rast_crop, which(lyrs_to_keep))
  
  return(my_rast_crop_sub)
}
