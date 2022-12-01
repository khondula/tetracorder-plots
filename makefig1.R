library(cowplot)
library(ggplot2)
library(magick)
# install.packages('magick')
fig1a <- 'jgre1550-fig-0002.png'
fig1b <- 'jgre1550-fig-0004.png'
p1 <- ggdraw() + draw_image(fig1a) 
p2 <- ggdraw() + draw_image(fig1b)

png('fig1.png', width = 800, height = 400, res = 100)
cowplot::plot_grid(p1, p2, labels = 'auto')
dev.off()
