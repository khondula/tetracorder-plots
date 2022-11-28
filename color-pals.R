my_grp2_pal1 <- c('alunite' = '#F9CCF9', 
                    'calcite' = '#FCC0D7',
                    'chlorite'= '#FDB4B5',
                    'dickite'= '#FCA994',
                    'epidote'= '#F49E71',
                    'group2_amix'= '#DE9651',
                    'group2_imix'= '#BF8F38',
                    'halloysite'= '#9D892E',
                    'illite'= '#808133',
                    'jarosite'= '#65793F',
                    'kaolinite'= '#4D724C',
                    'montmorillonite'= '#366858',
                    'muscovite'= '#225B60')

my_grp2_pal2 <- c('alunite' = '#BEFDA5', 
                  'calcite' = '#9CD26B',
                  'chlorite'= '#7EAC45',
                  'dickite'= '#658C31',
                  'epidote'= '#517026',
                  'group2_amix'= '#3C531D',
                  'group2_imix'= '#293516',
                  'halloysite'= '#1C1F10',
                  'illite'= '#1A1412',
                  'jarosite' = '#24131E',
                  'kaolinite'= '#401B37',
                  'montmorillonite'= '#6A2A5B',
                  'muscovite'= '#923E80')

my_grp1_pal <- c('epidote' = '#B2F2FD',
                 'goethite' = '#883E3A',
                 'hematite' = '#E0754F',
                 'nonotronite' = '#FFFFCC',
                 'pyrite' = '#F1C659')
scales::show_col(my_grp2_pal1)

scales::show_col(scico::scico(5, palette = 'hawaii'))
scales::show_col(
  c(scico::scico(n = 17, palette = 'lapaz', direction = 1))
)

lapaz17 <- c(scico::scico(n = 17, palette = 'lapaz', direction = 1))

make_g1_legend <- function(){
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  plot.new()
  legend('bottomleft', 
         title = expression(paste('Group 1', mu, 'm')),
         title.adj = 0,
         legend = stringr::str_to_title(names(my_grp1_pal)), 
         fill = my_grp1_pal, 
         bty = 'n')
}

make_g2_legend <- function(){
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  plot.new()
  legend('topleft', 
         title = expression(paste('Group 2', mu, 'm')),
         title.adj = 0,
         legend = stringr::str_to_title(names(mineral_agg2)), 
         fill = lapaz17, 
         bty = 'n')
}

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



png('legend2.png', width = 200, height = 800)
cowplot::plot_grid(make_g12_legend)
dev.off()

make_legends <- cowplot::plot_grid(make_g1_legend, make_g2_legend, 
                                   # rel_heights = 1, -0.1, 1,
                                   ncol = 1)
  
png('legend.png', width = 200, height = 800)
cowplot::plot_grid(make_g1_legend, make_g2_legend, 
                   # rel_heights = 1, -0.1, 1,
                   ncol = 1)
dev.off()
