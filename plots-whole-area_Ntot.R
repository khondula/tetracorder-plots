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

make_allem_rast <- function(mineral_name, group_name){
  grp_short <- stringr::str_remove(group_name, stringr::fixed("."))
  all_ems <- ems_df %>% dplyr::filter(target == mineral_name, Group == group_name) %>% dplyr::pull(tetracorder_file)
  em_fits <- glue('{tet_out_dir}/01_tetfits/{grp_short}/{all_ems}.fit.gz')
  em_rast <- terra::rast(em_fits)
  em_rast_na <- terra::subst(em_rast, 0, NA)
  em_rast_na_crop <- crop_n_drop(em_rast_na, terra::ext(em_rast))
  em_mineral_max <- terra::app(em_rast_na_crop, which.max, na.rm = TRUE)
  return(em_mineral_max)
}
# PANEL A - Hematite
hematite_rast <- make_allem_rast(mineral_name = 'Hematite', group_name = 'group.1um')
n_hematite <- max(as.data.frame(freq(hematite_rast))[['value']])
n_hematite

p3a <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(hematite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_hematite, palette = 'romaO', direction = -1))
}


# PANEL B - GOETHITE
goethite_rast <- make_allem_rast(mineral_name = 'Goethite', group_name = 'group.1um')
n_goethite <- max(as.data.frame(freq(goethite_rast))[['value']])
n_goethite

p3b <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(goethite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_goethite, palette = 'romaO', direction = -1))
}

# PANEL C - ALUNITE
alunite_rast <- make_allem_rast(mineral_name = 'Alunite', group_name = 'group.2um')
n_alunite <- max(as.data.frame(freq(alunite_rast))[['value']])
n_alunite

p3c <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(alunite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_alunite, palette = 'romaO', direction = -1))
}

# CALCITE

calcite_rast <- make_allem_rast(mineral_name = 'Calcite', group_name = 'group.2um')
n_calcite <- max(as.data.frame(freq(calcite_rast))[['value']])
n_calcite

p3d <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(calcite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_calcite, palette = 'romaO', direction = -1))
}

# KAOLINITE

kaolinite_rast <- make_allem_rast(mineral_name = 'Kaolinite', group_name = 'group.2um')
n_kaol <- max(as.data.frame(freq(kaolinite_rast))[['value']])
n_kaol

p3e <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(kaolinite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_kaol, palette = 'romaO', direction = -1))
}

# MONTMORILLONITE

mont_rast <- make_allem_rast(mineral_name = 'Montmorillonite', group_name = 'group.2um')
n_mont <- max(as.data.frame(freq(mont_rast))[['value']])
n_mont

p3f <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(mont_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_mont, palette = 'romaO', direction = -1))
}

# MUSCOVITE
muscovite_rast <- make_allem_rast(mineral_name = 'Muscovite', group_name = 'group.2um')
n_musc <- max(as.data.frame(freq(muscovite_rast))[['value']])
n_musc

p3g <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(muscovite_rast,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       col = scico::scico(n_musc, palette = 'romaO', direction = -1))
}

## ALL TOGETHER
png('Fig4_2.png', width = 1200, height = 800)
cowplot::plot_grid(p3a, NULL, p3b, NULL, p3c, NULL, p3d, NULL, p3e, NULL, p3f, NULL, p3g,
                   labels = c('Hematite', '', 
                              'Goethite', '', 
                              'Alunite', '', 
                              'Calcite', '',
                              'Kaolinite', 
                              'Montmorill.', '',
                              'Muscovite'),
                   label_x = 0.35,
                   # hjust = -0.5,
                   rel_widths = c(1, -0.5, 
                                  1, -0.5, 
                                  1, -0.5, 
                                  1, -0.5, 
                                  1, -0.5, 
                                  1, -0.5, 
                                  1), 
                   nrow = 1)
dev.off()
