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

# PANEL A

make_rgb <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  terra::plotRGB(orig_rast, r = 54, g = 36, b = 20, 
                 scale = 10000,
                 stretch = 'lin', 
                 mar = c(3, 2, 2,2), 
                 oma=c(0,0,0,0),
                 # ext = my_ext, 
                 colNA = 'white')
  }

## PANEL B Whole area group 1 Minerals and Mixtures

mineral_agg1 <- glue('{tet_out_dir}/03_mineral_mix/group1um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg1_na <- terra::subst(mineral_agg1, 0, NA)
em_mineral_max1 <- terra::app(mineral_agg1_na, which.max, na.rm = TRUE)

p1 <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max1, 
     colNA = 'white', 
     levels = names(mineral_agg1),
     # ext = my_ext,
     axes = FALSE,
     type = 'class',
     legend = FALSE,
     # main = glue('Group 1 Minerals and mixtures'),
     # col = my_grp2_pal2)
     col = c(scico::scico(n = 5, palette = 'lajolla', direction = -1)))
}

# PANEL C Just minerals group 1 !!

mineral_agg1m <- glue('{tet_out_dir}/02_minerals_only/group1um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg1m <- terra::subst(mineral_agg1m, 0, NA)
em_mineral_max1m <- terra::app(mineral_agg1m, which.max, na.rm = TRUE)

lajolla5 <- scico::scico(n = nlyr(mineral_agg1m), palette = 'lajolla', direction = -1)
# scales::show_col(lajolla5)
# lajolla3 <- lajolla5[names(mineral_agg1) %in% names(mineral_agg1_crop)]

p1b <- function(){
  plot(em_mineral_max1m, 
     colNA = 'white', 
     # ext = my_ext,
     type = 'class',
     axes = FALSE,
     levels = names(mineral_agg1m),
     legend = FALSE,
     # main = glue('Group 1 Minerals Only'),
     col = scico::scico(n = nlyr(mineral_agg1m), palette = 'lajolla', direction = -1))
}

###########################################
# PANEL D Whole area group 2 Minerals and Mixtures
###########################################

mineral_agg2 <- glue('{tet_out_dir}/03_mineral_mix/group2um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg2 <- crop_n_drop(mineral_agg2, orig_ext)
mineral_agg2_na <- terra::subst(mineral_agg2, 0, NA)
em_mineral_max2 <- terra::app(mineral_agg2_na, which.max, na.rm = TRUE)

lapaz16 <- scico::scico(n = nlyr(mineral_agg2_na), palette = 'lapaz', direction = 1)
grp2cols_df <- data.frame(names = names(mineral_agg2_na), values = c(1:nlyr(mineral_agg2_na)), cols=lapaz16)
coltab(em_mineral_max2) <- grp2cols_df[,c('values', 'cols')]

p2 <- function(){
  plot(em_mineral_max2, 
     colNA = 'white', 
     axes = FALSE)
}


###########################
# PANEL E Just minerals group 2 
###########################

mineral_agg2m <- glue('{tet_out_dir}/02_minerals_only/group2um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg2m_crop <- crop_n_drop(mineral_agg2m, orig_ext)
mineral_agg2m_na <- terra::subst(mineral_agg2m_crop, 0, NA)
em_mineral_max2m <- terra::app(mineral_agg2m_na, which.max, na.rm = TRUE)

# names(mineral_agg2m_na)
# use colors from other layer and update to new values for layers used
# keeping names and colors the same
grp2cols_df2 <- grp2cols_df %>% 
  dplyr::filter(names %in% names(mineral_agg2m)) %>%
  dplyr::select(-values) %>% 
  dplyr::mutate(values = 1:nlyr(mineral_agg2m_na))

coltab(em_mineral_max2m) <- grp2cols_df2[,c('values', 'cols')]

p2b <- function() {
  plot(em_mineral_max2m, 
     colNA = 'white', 
     axes = FALSE)
}
# LEGEND

my_grp2_pal2 <- c('alunite' = '#190C65', 
                  'buddingtonite' = '1F2474',
                  'calcite' = '#223984',
                  'chlorite'= '#264C91',
                  'dickite'= '#2C5F9B',
                  'epidote'= '#4483A6',
                  'group2_amix'= '#5992A5',
                  'group2_imix'= '#729DA0',
                  'halloysite'= '#8CA398',
                  'illite'= '#A5A58E',
                  'jarosite' = '#BFAA88',
                  'kaolinite'= '#DFB793',
                  'montmorillonite'= '#F7CDB2',
                  'muscovite'= '#FEE0D3',
                  'nontronite' = 'FFF2F2')

my_grp1_pal <- c('epidote' = '#B2F2FD',
                 'goethite' = '#883E3A',
                 'hematite' = '#E0754F',
                 'nonotronite' = '#FFFFCC',
                 'pyrite' = '#F1C659')

make_g12_legend <- function(){
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  plot.new()
  legend(x = 0.01, y = 0.85, 
         cex = 1.5,
         title = expression(paste('Group 1', mu, 'm')),
         title.adj = 0,
         legend = stringr::str_to_title(names(my_grp1_pal)), 
         fill = my_grp1_pal, 
         bty = 'n')
  legend(x = 0.01, y =0.65,
         title = expression(paste('Group 2', mu, 'm')),
         cex = 1.5,
         title.adj = 0,
         legend = stringr::str_to_title(names(mineral_agg2)), 
         fill = lapaz17, 
         bty = 'n')
}

## ALL TOGETHER

png('Fig4_1.png', width = 1200, height = 800)
cowplot::plot_grid(make_rgb, NULL, p1b, NULL, p1, NULL, p2b, NULL, p2, NULL, make_g12_legend,
                   labels = c('a', '', 'b', '', 'c', '', 'd', '', 'e', '', ''), 
                   label_x = 0.5,
                   rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1, -0.1, 1), nrow = 1)
dev.off()
