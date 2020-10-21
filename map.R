# https://rpubs.com/danielequs/199150

library("rgdal") # for `ogrInfo()` and `readOGR()`
library("tools") # for `file_path_sans_ext()`
library("dplyr") # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library("ggplot2") # for `fortify()` and for plotting
library("sp") # for `point.in.polygon()` and `spDists()`
library("tidyr") # for `gather()`
library("readr") # for `write_tsv()`
#install.packages("mapproj")
library("mapproj")

# Natural Earth 10-m-resolution coastline shapefile:
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-coastline/

#install.packages("mregions")
library(mregions)

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}

# domain should be a vector of four values: c(xmin, xmax, ymin, ymax)
path.ne.coast <- ("/Users/pierre-louisstenger/Desktop/Hippopus_porcellanus/ne_10m_coastline")
fnam.ne.coast <- "ne_10m_coastline.shp"
dat.coast <- readOGR(dsn = path.ne.coast, layer = file_path_sans_ext(fnam.ne.coast))
# A Large SpatialLinesDataFrame object with 4132 features and 2 fields (12.8 Mb)

# Fortify the shapefile data using `fortify.shape()`:
dat.coast <- fortify.shape(dat.coast) # a 410951x8 dataframe

# Specify the desired domain:
domain <- c(-128, -115, 29, 50)

# Specify the desired domain (PACIFIC)
domain <- c(29, 180, -27, 89)

# Extract the coastline data for the desired domain using `subset.shape()`:
dat.coast.wc <- subset.shape(dat.coast, domain) # a 4871x8 dataframe

# Specify the spatial extent for our map (i.e., our study area; notice that its
# dimensions are different from the domain for which we extracted the
# coastline):
xlims <- c(110, 179)
ylims <- c(-27, 20)

# Generate a base map with the coastline:
# With density
obs <- read.table("H_porcellanus_obs_02.txt", header=T, sep="\t")
obs
#  Obs Hippopus porcellanus
n.H_porcellanus <- obs$nb_of_obs
long <- obs$lon
lat <- obs$lat
sim.obs <- data.frame(long = long, lat = lat, n.H_porcellanus = n.H_porcellanus)


# And outgroup points
obs_out <- read.table("H_porcellanus_obs_outgroup.txt", header=T, sep="\t")
obs_out
#  Obs Hippopus porcellanus
n.H_porcellanus <- obs_out$nb_of_obs
long <- obs_out$lon
lat <- obs_out$lat
sim.obs_out <- data.frame(long = long, lat = lat, n.H_porcellanus = n.H_porcellanus)

##############################################################################################################
my_y_title <- expression(paste(italic("Hyppopus porcellanus"), " distribution"))


ggplot() + 
  geom_path(data = dat.coast.wc, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(title=my_y_title, x = "Longitude", y = "Latitude") + 
  
  # points
  geom_point(data = sim.obs_out, 
             aes(x = long, y = lat), 
             shape = 0, size = 3, colour="red") + 
  # Density
  stat_density_2d(data = sim.obs, geom = "polygon", 
                  aes(x = long, y = lat, fill = ..level..)) + 
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Probability \ndensity")) + 
  annotate(geom="text", x=160, y=-24, label="New-Caledonia", fontface =2, color="black") + 
  annotate(geom="text", x=175, y=-23, label="Tonga", fontface =2, color="black") + 
  annotate(geom="text", x=140, y=7.5, label="Palau", fontface =2, color="black") + 
  annotate(geom="text", x=132, y=11.5, label="Philppines", fontface =2, color="black")

##############################################################################################################

