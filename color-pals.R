# coltab(r) <- data.frame(values=c(10:12, 23:25), cols=rainbow(6))

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


# make_g1_legend <- function(){
#   par(mar = c(0,0,0,0), oma = c(0,0,0,0))
#   plot.new()
#   legend('bottomleft', 
#          title = expression(paste('Group 1', mu, 'm')),
#          title.adj = 0,
#          legend = stringr::str_to_title(names(my_grp1_pal)), 
#          fill = my_grp1_pal, 
#          bty = 'n')
# }
# 
# make_g2_legend <- function(){
#   par(mar = c(0,0,0,0), oma = c(0,0,0,0))
#   plot.new()
#   legend('topleft', 
#          title = expression(paste('Group 2', mu, 'm')),
#          title.adj = 0,
#          legend = stringr::str_to_title(names(mineral_agg2)), 
#          fill = lapaz17, 
#          bty = 'n')
# }

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

