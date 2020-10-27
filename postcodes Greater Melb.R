
# create a hexagon map
library(sugarbag)
library(sf)
library(absmapsdata)
library(ggplot2)

## postcode
## areas from absmapsdata package
postcode <- absmapsdata::postcode2016 %>% 
  st_transform(., crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  filter(cent_long>110, cent_long<154)

# Create centroids set
centroids <- create_centroids(postcode, "postcode_2016")

# choropleth map display
ggplot(postcode) + 
  geom_sf(fill = "grey") +
  coord_sf(crs = "+proj=longlat +datum=WGS84")


# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.1, buffer_dist = 2)

# plot of grid points over geographic polygons
ggplot(postcode) + 
  geom_sf(aes(fill = postcode_num_2016)) +
  geom_point(aes(x = hex_long, y = hex_lat), 
             colour = "orange", size = 0.002, data = grid) +
  scale_fill_distiller(type = "seq", palette = "BuGn",
                       direction = 1, na.value = "light grey") + 
  coord_sf(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

# Allocate polygon centroids to hexagon grid points
system.time(
  postcode_melb <- allocate(centroids = centroids,
                          hex_grid = grid,
                          sf_id = "postcode_2016",
                          hex_size = 0.1, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          width = 30, verbose = TRUE) # same column used in create_centroids
)

# Under 5 minutes for postcodes

# save locations of hexagons as rda and csv
save(postcode_melb, file = "data/postcode_melb1.rda")
write.csv(postcode_melb, file = "data/postcode_melb1.csv")

# Alternatively if the file exists already
#load("small_melb_city003.rda")

# convert to polygons for plotting
fort_postcode <- fortify_sfc(postcode)
hex_postcode <- fortify_hexagon(postcode_melb, "postcode_2016", hex_size = 0.1)



melb_postcode <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(postcode_2016, polygon)),
               fill = "grey", colour = "lightgrey", size = 0.02,
               data = fort_postcode) + 
  geom_polygon(aes(x = long, y = lat, group = postcode_2016,
                   fill = log(focal_dist)), colour = NA,
               data = hex_postcode) +
  # colour the hexagons for a variable
  scale_fill_distiller(type = "seq", palette = "YlOrRd",
                       direction = -1, na.value = "lightgrey") + 
  coord_sf(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") +
  theme_minimal() 
melb_postcode




ggsave(filename = "figures/aus_postcode1.pdf", plot = melb_postcode,
       device = "pdf", bg = "transparent", dpi = 600,  width = 10, height = 8)

