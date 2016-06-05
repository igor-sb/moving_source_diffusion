#
#  Load the output data file from COMSOL multiphysics simulations for a moving source
#  of diffusing chemical and plot concentration of diffusing molecule with time.
#
#  Author: Igor Segota

library(data.table)
library(ggplot2)

setwd("/Users/Igor/Personal/Github/moving_source_diffusion")

# Load the output data from COMSOL
input_data_name = "moving_source_fast_data_200by200_D2e11_xyStep3.txt"
raw.dt = fread(input_data_name)
input_data = file(input_data_name, "r")
raw.dt.names = strsplit(readLines(input_data, n=2)[2], " ")
close(input_data)
raw.dt.names = raw.dt.names[[1]]
raw.dt.names = raw.dt.names[!(raw.dt.names %in% c("", "%"))]
names(raw.dt) = raw.dt.names

# Melt and tidy the data
raw_tidy.dt = melt(raw.dt, id.vars=c("x", "y"), value.name="concentration", variable.name="time_step", variable.factor=F)
raw_tidy.dt[, time_step:=as.integer(gsub("Time=", "", time_step, fixed=T))]
raw_tidy.dt[, x:=x*1000]
raw_tidy.dt[, y:=y*1000]
raw_tidy.dt[concentration < 1e-5, concentration:=0]

# Generate plot for each time step, then later merge into gif using Easy GIF Animator
raw_tidy.dt[, {
  png(paste0("images_per_frame_D2e-11_xyStep3/", time_step, ".png"), width=240, heigh=240)
  t.plot = ggplot(.SD, aes(x=x, y=y, fill=concentration)) +
    geom_raster(interpolate=T) +
    scale_fill_gradientn(colors=rev(rainbow(12)[1:9]), na.value = rainbow(12)[1], limits=c(0, 1e10)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(legend.position="none", axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
          axis.line=element_blank(), plot.margin=unit(c(0,0,0,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
  print(grid::grid.draw(gtable::gtable_filter(ggplotGrob(t.plot), "panel")))
  dev.off()
  }, by=time_step]

