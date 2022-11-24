# RGB

# RGB in context with full image

# install.packages('gridGraphics')
library(cowplot)

# need to save individual layers that will get used
cowplot::plot_grid(make_rgb, make_rgb)
