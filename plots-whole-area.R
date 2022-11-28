# full size minerals and mineral mixtures

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

## Whole area group 1 Minerals and Mixtures

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

## Just minerals

# Just minerals group 1 !!

mineral_agg1m <- glue('{tet_out_dir}/02_minerals_only/group1um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg1m <- terra::subst(mineral_agg1m, 0, NA)
em_mineral_max1m <- terra::app(mineral_agg1m, which.max, na.rm = TRUE)

lajolla5 <- scico::scico(n = nlyr(mineral_agg1m), palette = 'lajolla', direction = -1)
scales::show_col(lajolla5)
lajolla3 <- lajolla5[names(mineral_agg1) %in% names(mineral_agg1_crop)]

# pdf('{plots_dir}/group2_minsmix.pdf')
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

## Whole area group 2 Minerals and Mixtures

mineral_agg2 <- glue('{tet_out_dir}/03_mineral_mix/group2um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg2_na <- terra::subst(mineral_agg2, 0, NA)
em_mineral_max2 <- terra::app(mineral_agg2_na, which.max, na.rm = TRUE)

lapaz17 <- scico::scico(n = nlyr(mineral_agg2_na), palette = 'lapaz', direction = 1)
scales::show_col(lapaz17)


p2 <- function(){
  plot(em_mineral_max2, 
     colNA = 'white', 
     levels = names(mineral_agg2),
     # ext = my_ext,
     axes = FALSE,
     type = 'class',
     # main = glue('Group 2 Minerals and mixtures'),
     # legend = FALSE,
     # col = my_grp2_pal2)
     col = lapaz17) # or vanimo or imola
}


# Just minerals group 2 

mineral_agg2m <- glue('{tet_out_dir}/02_minerals_only/group2um') %>% fs::dir_ls() %>% terra::rast()
mineral_agg2m <- terra::subst(mineral_agg2m, 0, NA)
em_mineral_max2m <- terra::app(mineral_agg2m, which.max, na.rm = TRUE)

lapaz15 <- lapaz17[which(names(mineral_agg2_na) %in% names(mineral_agg2m))]
scales::show_col(lapaz15)
scales::show_col(lapaz17)

# pdf('{plots_dir}/group2_minsmix.pdf')
p2b <- function() {
  plot(em_mineral_max2m, 
     colNA = 'white', 
     # ext = my_ext,
     type = 'class',
     axes = FALSE,
     # legend = FALSE,
     levels = names(mineral_agg2m),
     # main = glue('Group 2 Minerals Only'),
     col = lapaz15)
}

pdf('test8.pdf')
cowplot::plot_grid(make_rgb, NULL, p1, NULL, p1b, NULL, p2, NULL, p2b,
                   rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1), nrow = 1)
dev.off()

png('test10.png', width = 1000, height = 800)
cowplot::plot_grid(make_rgb, NULL, p1, NULL, p1b, NULL, p2, NULL, p2b, 
                   labels = c('a', '', 'b', '', 'c', '', 'd', '', 'e'), label_x = 0.5,
                   rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1), nrow = 1)
dev.off()

png('test13.png', width = 1200, height = 800)
cowplot::plot_grid(make_rgb, NULL, p1, NULL, p1b, NULL, p2, NULL, p2b, NULL, make_g12_legend,
                      labels = c('a', '', 'b', '', 'c', '', 'd', '', 'e', '', ''), 
                      label_x = 0.5,
                      rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1, -0.1, 1), nrow = 1)
dev.off()

png('test14.png', width = 1200, height = 800)
cowplot::plot_grid(make_rgb, NULL, p1, NULL, p1b, NULL, p2, NULL, p2b, NULL, make_g12_legend,
                   labels = c('a', '', 'b', '', 'c', '', 'd', '', 'e', '', ''), 
                   label_x = 0.5,
                   rel_widths = c(1, -0.5, 1, -0.5, 1, -0.5, 1, -0.5, 1, -0.1, 1), nrow = 1)
dev.off()
