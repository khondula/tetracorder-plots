# Figure 4.1 in tetracorder ATBD
# full size minerals and mineral mixtures
library(terra)
library(glue)
library(dplyr)
library(purrr)
library(scico)
library(sf)

base_dir <- '/Volumes/alab/CarbonMapper'
minerals_df <- readr::read_csv('Minerals.csv')
mixtures_df <- readr::read_csv('Mixtures.csv')
ems_df <- readr::read_csv('AllEndmembers.csv')
source('color-pals.R')
source('crop-n-drop.R')
cube_path <- glue('{base_dir}/software/tetracorder-essentials/example/input/ang20200712t201415_corr_v2y1_img')
cube_id <- basename(cube_path)
orig_rast <- terra::rast(cube_path)
tet_out_dir <- glue('{base_dir}/software/tetracorder-essentials/example/output/try1')
orig_ext <- terra::ext(orig_rast)
orig_crs <- terra::crs(orig_rast)
zoom_size <- 750

# Alunite Hill
my_x <- terra::xFromCol(orig_rast, 392)
my_y <- terra::yFromRow(orig_rast, 2700)
my_ext <- terra::ext(c(my_x-zoom_size, my_x+zoom_size, my_y-zoom_size, my_y+zoom_size))

make_allem_rast_zoom <- function(mineral_name, my_group_name, my_ext){
  grp_short <- stringr::str_remove(my_group_name, stringr::fixed("."))
  all_ems <- ems_df %>% dplyr::filter(target == mineral_name, Group == my_group_name) %>% dplyr::pull(tetracorder_file)
  em_fits <- glue('{tet_out_dir}/01_tetfits/{grp_short}/{all_ems}.fit.gz')
  em_rast <- terra::rast(em_fits)
  em_rast_na <- terra::subst(em_rast, 0, NA)
  em_rast_na_crop <- crop_n_drop(em_rast_na, my_ext)
  em_mineral_max <- terra::app(em_rast_na_crop, which.max, na.rm = TRUE)
  out_list <- list('rast' = em_mineral_max, 
                   'lyrs_in_img' = names(em_rast_na_crop),
                   'all_lyrs' = all_ems)
  return(out_list)
}

make_minerals_rast_zoom <- function(mineral_name, my_group_name, my_ext){
  grp_short <- stringr::str_remove(my_group_name, stringr::fixed("."))
  mins_ems <- minerals_df %>% dplyr::filter(target == mineral_name, group_name == my_group_name) %>% dplyr::pull(tetracorder_file)
  em_fits <- glue('{tet_out_dir}/01_tetfits/{grp_short}/{mins_ems}.fit.gz')
  em_rast <- terra::rast(em_fits)
  em_rast_na <- terra::subst(em_rast, 0, NA)
  em_rast_na_crop <- crop_n_drop(em_rast_na, my_ext)
  em_mineral_max <- terra::app(em_rast_na_crop, which.max, na.rm = TRUE)
  out_list <- list('rast' = em_mineral_max, 
                   'lyrs_in_img' = names(em_rast_na_crop),
                   'all_mins_ems' = mins_ems)
  return(out_list)
}

make_mixtures_rast_zoom <- function(mineral_name, my_group_name, my_ext){
  grp_short <- stringr::str_remove(my_group_name, stringr::fixed("."))
  mixture_ems <- mixtures_df %>% dplyr::filter(Dominant_Mineral == mineral_name, group_name == my_group_name) %>% dplyr::pull(tetracorder_file)
  mins_ems <- minerals_df %>% dplyr::filter(target == mineral_name, group_name == my_group_name) %>% dplyr::pull(tetracorder_file)
  mixs_ems <- unique(c(mixture_ems, mins_ems))
  
  em_fits <- glue('{tet_out_dir}/01_tetfits/{grp_short}/{mixs_ems}.fit.gz')
  em_rast <- terra::rast(em_fits)
  em_rast_na <- terra::subst(em_rast, 0, NA)
  em_rast_na_crop <- crop_n_drop(em_rast_na, my_ext)
  em_mineral_max <- terra::app(em_rast_na_crop, which.max, na.rm = TRUE)
  out_list <- list('rast' = em_mineral_max, 
                   'lyrs_in_img' = names(em_rast_na_crop),
                   'all_mixs_ems' = mixs_ems)
  return(out_list)
  }


# hematite

## all endmembers
hematite_rast_list <- make_allem_rast_zoom(mineral_name = 'Hematite', my_group_name = 'group.1um', my_ext)
hematite_rast <- hematite_rast_list$rast
hematite_rast_list$lyrs_in_img
hematite_rast_list$all_lyrs
n_hematite <- max(as.data.frame(freq(hematite_rast))[['value']])
n_hematite
# set coltab 
coltab_all <- data.frame(values = 1:length(hematite_rast_list$lyrs_in_img),
                         names = hematite_rast_list$lyrs_in_img,
                         cols = scico(length(hematite_rast_list$lyrs_in_img), palette = 'batlow'))

coltab(hematite_rast) <- coltab_all[, c('values', 'cols')]

p3a <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(hematite_rast, axes = FALSE, colNA = 'black')
}

# hematite legend

hematite_pal <- coltab_all[,c('names', 'cols')]

make_hematite_legend <- function(){
  par(mar = c(0,3,0,0), oma = c(0,0,0,0))
  plot.new()
  legend('center',
         ncol = 4,
    # x = 0.01, y = 0.85, 
         # cex = 0.5,
         title.adj = 0,
         legend = stringr::str_remove(hematite_pal$names, pattern = stringr::fixed(".fit")), 
         fill = hematite_pal$cols, 
         bty = 'n')
}



## mixtures
hematite_mixs_list <- make_mixtures_rast_zoom(mineral_name = 'Hematite', my_group_name = 'group.1um', my_ext)
hematiteMIX_rast <- hematite_mixs_list$rast
hematite_mixs_list$lyrs_in_img
hematite_mixs_list$all_mixs_ems
n_hematiteMIX <- max(as.data.frame(freq(hematiteMIX_rast))[['value']])

coltab_mix <- coltab_all %>% dplyr::filter(names %in% hematite_mixs_list$lyrs_in_img) %>%
  dplyr::select(-values) %>% dplyr::mutate(values = 1:n_hematiteMIX)
coltab(hematiteMIX_rast) <- coltab_mix[, c('values', 'cols')]

p3b <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(hematiteMIX_rast, axes = FALSE, colNA = 'black')
}

# set coltab with new vals

## just minerals
hematite_min_list <- make_minerals_rast_zoom(mineral_name = 'Hematite', my_group_name = 'group.1um', my_ext)
hematiteMIN_rast <- hematite_min_list$rast
hematite_min_list$lyrs_in_img
n_hematiteMIN <- max(as.data.frame(freq(hematiteMIN_rast))[['value']])

coltab_min <- coltab_all %>% dplyr::filter(names %in% hematite_min_list$lyrs_in_img) %>%
  dplyr::select(-values) %>% dplyr::mutate(values = 1:n_hematiteMIN)
coltab(hematiteMIN_rast) <- coltab_min[, c('values', 'cols')]

# plot(hematiteMIN_rast, axes = FALSE, colNA = 'black')

p3c <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(hematiteMIN_rast, axes = FALSE, colNA = 'black')
}



## ALL TOGETHER

## just hematite
png('Fig4_3_hematite.png', width = 900, height = 400)
aa <- cowplot::plot_grid(NULL, p3c, NULL, p3b, NULL, p3a,
                   labels = c('', 'Minerals', '', 'Minerals + Mixtures', '', 'All Endmembers'),
                   label_x = 0.2,
                   rel_widths = c(-0.1, 1, -0.2, 1, -0.2, 1),
                   nrow = 1)
cowplot::plot_grid(aa, NULL, make_hematite_legend, nrow = 3, rel_heights = c(1, -0.01, 0.2))
dev.off()

