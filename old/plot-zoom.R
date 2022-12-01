# plot RGB with inset box
# load first few chunks of 00 Rmd

# Alunite Hill
my_x <- terra::xFromCol(orig_rast, 392)
my_y <- terra::yFromRow(orig_rast, 2700)
my_ext <- terra::ext(c(my_x-zoom_size, my_x+zoom_size, my_y-zoom_size, my_y+zoom_size))

my_aoi_sf <- my_ext %>% st_bbox() %>% st_as_sfc() %>% st_as_sf()

make_rgb_box <- function(){
  terra::plotRGB(orig_rast, r = 54, g = 36, b = 20,
                 scale = 10000,
                 stretch = 'lin',
                 colNA = 'white')
  plot(my_aoi_sf, add = TRUE, border = 'red', lwd = 2)
  
}

# The default is (3.1, 3.1, 2.1, 7.1) 
# for a single plot with a legend and 
# (3.1, 3.1, 2.1, 2.1) otherwise

make_rgb_aoi <- function(){
  par(mar=c(3.1,3.1,2.1,2.1), oma=c(0,0,0,0))
  terra::plotRGB(orig_rast, r = 54, g = 36, b = 20,
                 scale = 10000,
                 stretch = 'lin',
                 mar=c(3.1,3.1,2.1,2.1), oma=c(0,0,0,0),
                 ext = my_ext,
                 colNA = 'white')
}

# Fit 

# mixture_ems2 <- mixtures_df %>% dplyr::filter(group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
# justmineral_ems2 <- minerals_df %>% dplyr::filter(group_name == 'group.2um') %>%  dplyr::pull(tetracorder_file)
# mineral_ems2 <- unique(c(justmineral_ems2, mixture_ems2))
# 
# em_fits <- glue('{tet_out_dir}/01_tetfits/group2um/{mineral_ems2}.fit.gz')
# em_rast <- terra::rast(em_fits)
# 
# em_rast2_crop <- crop_n_drop(em_rast, my_ext)
# em_rast2_crop255 <- em_rast2_crop/255
# mineral_agg2_na <- terra::subst(em_rast2_crop255, 0, NA)
# mineral_agg2_na_all <- terra::app(mineral_agg2_na, sum, na.rm = TRUE)
# 
# fitplot <- function(){
#   plot(mineral_agg2_na_all, 
#        colNA = 'white', 
#        ext = my_ext,
#        axes = FALSE,
#        # legend = FALSE,
#        # main = glue('Group 2 Minerals and mixtures'),
#        col = colorspace::sequential_hcl(255, 'viridis', rev = FALSE))
# }

# group 1 minerals and mixtures

mineral_agg1 <- glue('{tet_out_dir}/03_mineral_mix/group1um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg1_crop <- crop_n_drop(mineral_agg1, my_ext)
mineral_agg1_na <- terra::subst(mineral_agg1_crop, 0, NA)
em_mineral1_max <- terra::app(mineral_agg1_na, which.max, na.rm = TRUE)

lajolla5 <- scico::scico(n = nlyr(mineral_agg1), palette = 'lajolla', direction = -1)
scales::show_col(lajolla5)
lajolla3 <- lajolla5[names(mineral_agg1) %in% names(mineral_agg1_crop)]

p1 <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral1_max, 
       colNA = 'white', 
       levels = names(mineral_agg1_crop),
       axes = FALSE,
       legend = FALSE,
       type = 'class',
       col = lajolla3)
}

# group 2 minerals and mixtures

mineral_agg2 <- glue('{tet_out_dir}/03_mineral_mix/group2um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg2_crop <- crop_n_drop(mineral_agg2, my_ext)
mineral_agg2_na <- terra::subst(mineral_agg2_crop, 0, NA)
em_mineral2_max <- terra::app(mineral_agg2_na, which.max, na.rm = TRUE)

lapaz17 <- c(scico::scico(n = 17, palette = 'lapaz', direction = 1))
lapaz13 <- lapaz17[names(mineral_agg2) %in% names(mineral_agg2_crop)]

p2 <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral2_max, 
       colNA = 'black', 
       levels = names(mineral_agg2_crop),
       ext = my_ext,
       axes = FALSE,
       legend = FALSE,
       type = 'class',
       # main = glue('Group 2 Minerals and mixtures'),
       col = lapaz13)
}


# alunite minerals

justmineral_ems_alunite <- minerals_df %>% dplyr::filter(target == 'Alunite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
em_fits_alunite <- glue('{tet_out_dir}/01_tetfits/group2um/{justmineral_ems_alunite}.fit.gz')
em_rast_alunite <- terra::rast(em_fits_alunite)
em_rast_alunite_na <- terra::subst(em_rast_alunite, 0, NA)
em_rast_alunite_na <- crop_n_drop(em_rast_alunite_na, my_ext)
em_mineral_max_alunite <- terra::app(em_rast_alunite_na, which.max, na.rm = TRUE)

n_mins <- nlyr(em_rast_alunite_na)
imola5 <- scico::scico(n_mins, palette = 'imola', direction = -1)

p3a <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_alunite,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_alunite_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = imola5)
}

# alunite minerals and mixtures
mixture_ems_alunite <- mixtures_df %>% dplyr::filter(Dominant_Mineral == 'Alunite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
justmineral_ems_alunite <- minerals_df %>% dplyr::filter(target == 'Alunite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
minmix_ems_alunite <- unique(c(justmineral_ems_alunite, mixture_ems_alunite))

em_fits_aluniteMIX <- glue('{tet_out_dir}/01_tetfits/group2um/{minmix_ems_alunite}.fit.gz')
em_rast_aluniteMIX <- terra::rast(em_fits_aluniteMIX)
em_rast_aluniteMIX_na <- terra::subst(em_rast_aluniteMIX, 0, NA)
em_rast_aluniteMIX_na <- crop_n_drop(em_rast_aluniteMIX_na, my_ext)
em_mineral_max_aluniteMIX <- terra::app(em_rast_aluniteMIX_na, which.max, na.rm = TRUE)
n_mixs <- length(names(em_rast_aluniteMIX_na))-n_mins


batlow5 <- sample(scico::scico(n_mixs, palette = 'batlow', direction = -1))

p3b <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_aluniteMIX,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_aluniteMIX_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = c(imola5, batlow5))
}

# alunite all endmembers
all_ems_alunite <- ems_df %>% dplyr::filter(target == 'Alunite', Group == 'group.2um') %>% dplyr::pull(tetracorder_file)
em_fits_aluniteALL <- glue('{tet_out_dir}/01_tetfits/group2um/{all_ems_alunite}.fit.gz')
em_rast_aluniteALL <- terra::rast(em_fits_aluniteALL)
em_rast_aluniteALL_na <- terra::subst(em_rast_aluniteALL, 0, NA)
em_rast_aluniteALL_na <- crop_n_drop(em_rast_aluniteALL_na, my_ext)
em_mineral_max_aluniteALL <- terra::app(em_rast_aluniteALL_na, which.max, na.rm = TRUE)

n_all <- length(names(em_rast_aluniteALL_na))-(n_mixs + n_mins)

roma5 <- scico::scico(n_all, palette = 'romaO', direction = -1)

# length(names(em_rast_aluniteALL_na))

p3c <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_aluniteALL,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_aluniteALL_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = c(roma5[1:3], batlow5[1:3], imola5, batlow5)) # order isnt right here
}


# Kaolnite minerals

justmineral_ems_kaol <- minerals_df %>% dplyr::filter(target == 'Kaolinite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
em_fits_kaol <- glue('{tet_out_dir}/01_tetfits/group2um/{justmineral_ems_kaol}.fit.gz')
em_rast_kaol <- terra::rast(em_fits_kaol)
em_rast_kaol_na <- terra::subst(em_rast_kaol, 0, NA)
em_rast_kaol_na <- crop_n_drop(em_rast_kaol_na, my_ext)
em_mineral_max_kaol <- terra::app(em_rast_kaol_na, which.max, na.rm = TRUE)

n_mins <- nlyr(em_rast_kaol_na)
imola5 <- scico::scico(n_mins, palette = 'imola', direction = -1)

p4a <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_kaol,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_kaol_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = imola5)
}

# kaol minerals and mixtures
mixture_ems_kaol <- mixtures_df %>% dplyr::filter(Dominant_Mineral == 'Kaolinite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
justmineral_ems_kaol <- minerals_df %>% dplyr::filter(target == 'Kaolinite', group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
minmix_ems_kaol <- unique(c(justmineral_ems_kaol, mixture_ems_kaol))

em_fits_kaolMIX <- glue('{tet_out_dir}/01_tetfits/group2um/{minmix_ems_kaol}.fit.gz')
em_rast_kaolMIX <- terra::rast(em_fits_kaolMIX)
em_rast_kaolMIX_na <- terra::subst(em_rast_kaolMIX, 0, NA)
em_rast_kaolMIX_na <- crop_n_drop(em_rast_kaolMIX_na, my_ext)
em_mineral_max_kaolMIX <- terra::app(em_rast_kaolMIX_na, which.max, na.rm = TRUE)
n_mixs <- length(names(em_rast_kaolMIX_na))-n_mins


batlow5 <- sample(scico::scico(n_mixs, palette = 'batlow', direction = -1))

p4b <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_kaolMIX,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_kaolMIX_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = c(imola5, batlow5))
}

# kaol all endmembers
all_ems_kaol <- ems_df %>% dplyr::filter(target == 'Kaolinite', Group == 'group.2um') %>% dplyr::pull(tetracorder_file)
em_fits_kaolALL <- glue('{tet_out_dir}/01_tetfits/group2um/{all_ems_kaol}.fit.gz')
em_rast_kaolALL <- terra::rast(em_fits_kaolALL)
em_rast_kaolALL_na <- terra::subst(em_rast_kaolALL, 0, NA)
em_rast_kaolALL_na <- crop_n_drop(em_rast_kaolALL_na, my_ext)
em_mineral_max_kaolALL <- terra::app(em_rast_kaolALL_na, which.max, na.rm = TRUE)

n_all <- length(names(em_rast_kaolALL_na))-(n_mixs + n_mins)

roma5 <- scico::scico(n_all, palette = 'romaO', direction = -1)

# length(names(em_rast_kaolALL_na))

p4c <- function(){
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(em_mineral_max_kaolALL,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_kaolALL_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = c(roma5[1:3], batlow5[1:3], imola5, batlow5)) # order isnt right here
}



# save combined
row1 <- cowplot::plot_grid(make_rgb_aoi, p1, p2, 
                   nrow = 1, 
                   rel_widths = c(1, 1, 1),
                   rel_heights = c(1, 1, 1))
row2 <- cowplot::plot_grid(p3a, p3b, p3c, 
                           nrow = 1)

png('test31.png', width = 1200, height = 800, res = 100)
cowplot::plot_grid(row1, NULL, row2,
                   nrow = 3,
                   rel_widths = c(1, 0, 1),
                   rel_heights = c(1, -0.2, 1))

dev.off()