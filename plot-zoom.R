# plot RGB with inset box


orig_ext <- terra::ext(orig_rast)
orig_crs <- terra::crs(orig_rast)

zoom_size <- 500

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

make_rgb_aoi <- function(){
  terra::plotRGB(orig_rast, r = 54, g = 36, b = 20,
                 scale = 10000,
                 stretch = 'lin',
                 mar = c(2, 2, 2,2), 
                 oma=c(0,0,0,0),
                 # omd
                 ext = my_ext,
                 colNA = 'white')
}

# Fit 

mixture_ems2 <- mixtures_df %>% dplyr::filter(group_name == 'group.2um') %>% dplyr::pull(tetracorder_file)
justmineral_ems2 <- minerals_df %>% dplyr::filter(group_name == 'group.2um') %>%  dplyr::pull(tetracorder_file)
mineral_ems2 <- unique(c(justmineral_ems2, mixture_ems2))

em_fits <- glue('{tet_out_dir}/01_tetfits/group2um/{mineral_ems2}.fit.gz')
em_rast <- terra::rast(em_fits)

em_rast2_crop <- crop_n_drop(em_rast, my_ext)
em_rast2_crop255 <- em_rast2_crop/255
mineral_agg2_na <- terra::subst(em_rast2_crop255, 0, NA)
mineral_agg2_na_all <- terra::app(mineral_agg2_na, sum, na.rm = TRUE)

fitplot <- function(){
  plot(mineral_agg2_na_all, 
       colNA = 'white', 
       ext = my_ext,
       axes = FALSE,
       # legend = FALSE,
       # main = glue('Group 2 Minerals and mixtures'),
       col = colorspace::sequential_hcl(255, 'viridis', rev = FALSE))
}

# group 1 minerals and mixtures

mineral_agg1 <- glue('{tet_out_dir}/03_mineral_mix/group1um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg1_crop <- crop_n_drop(mineral_agg1, my_ext)
mineral_agg1_na <- terra::subst(mineral_agg1_crop, 0, NA)
em_mineral1_max <- terra::app(mineral_agg1_na, which.max, na.rm = TRUE)

lajolla5 <- scico::scico(n = nlyr(mineral_agg1m), palette = 'lajolla', direction = -1)
scales::show_col(lajolla5)
lajolla3 <- lajolla5[names(mineral_agg1) %in% names(mineral_agg1_crop)]

p1 <- function(){
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

p3a <- function(){
  plot(em_mineral_max_alunite,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_alunite_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = scico::scico(nlyr(em_rast_alunite_na), palette = 'imola', direction = -1))
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

p3b <- function(){
  plot(em_mineral_max_aluniteMIX,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_aluniteMIX_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = scico::scico(nlyr(em_rast_aluniteMIX_na), palette = 'imola', direction = -1))
}

# alunite all endmembers
all_ems_alunite <- ems_df %>% dplyr::filter(target == 'Alunite', Group == 'group.2um') %>% dplyr::pull(tetracorder_file)

em_fits_aluniteALL <- glue('{tet_out_dir}/01_tetfits/group2um/{all_ems_alunite}.fit.gz')
em_rast_aluniteALL <- terra::rast(em_fits_aluniteALL)
em_rast_aluniteALL_na <- terra::subst(em_rast_aluniteALL, 0, NA)
em_rast_aluniteALL_na <- crop_n_drop(em_rast_aluniteALL_na, my_ext)
em_mineral_max_aluniteALL <- terra::app(em_rast_aluniteALL_na, which.max, na.rm = TRUE)

p3c <- function(){
  plot(em_mineral_max_aluniteALL,
       type = 'classes',
       axes = FALSE,
       colNA = 'black',
       legend = FALSE,
       levels = tools::file_path_sans_ext(names(em_rast_aluniteALL_na)),
       # main = glue("{my_mineral} minerals and mixtures"),
       col = scico::scico(nlyr(em_rast_alunite_na), palette = 'imola', direction = -1))
}



png('test21.png', width = 1200, height = 800, res = 100)
cowplot::plot_grid(make_rgb_aoi, NULL, p1, NULL, p2, 
                   p3a, NULL, p3b, NULL, p3c,
                   nrow = 2,
                   rel_widths = c(1, -0.5, 1, -0.5, 1,
                                  1, -0.5, 1, -0.5, 1))
# cowplot::plot_grid(make_rgb, NULL, p1, NULL, p1b, NULL, p2, NULL, p2b,
#                    labels = c('a', '', 'b', '', 'c', '', 'd', '', 'e'), label_x = 0.5,
#                    rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1), nrow = 1)
dev.off()