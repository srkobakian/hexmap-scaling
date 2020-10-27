
# create a hexagon map
library(sugarbag)
library(sf)
library(absmapsdata)
library(ggplot2)

## sa1
## areas from absmapsdata package
sa1 <- absmapsdata::sa12011 %>% 
  filter(state_name_2011 != "Other Territories") %>% 
  filter(gcc_name_2011 == "Greater Melbourne") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84")

# Create centroids set
centroids <- create_centroids(sa1, "sa1_7dig_2011")

# choropleth map display
ggplot(sa1) + 
  geom_sf(aes(fill = albers_sqkm)) +
  scale_fill_distiller(type = "seq", palette = "BuGn",
                       direction = 1, na.value = "light grey") + 
  coord_sf(crs = "+proj=longlat +datum=WGS84", xlim = c(144.4,145.8), ylim = c(-38.8, -37.1)) +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))


# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.003, buffer_dist = 0.5)

# plot of grid points over geographic polygons
ggplot(sa1) + 
  geom_sf(aes(fill = albers_sqkm)) +
  geom_point(aes(x = hex_long, y = hex_lat), 
             colour = "orange", size = 0.002, data = grid) +
  scale_fill_distiller(type = "seq", palette = "BuGn",
                       direction = 1, na.value = "light grey") + 
  coord_sf(crs = "+proj=longlat +datum=WGS84") +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

# Allocate polygon centroids to hexagon grid points
system.time(
  sa1_melb003 <- allocate(centroids = centroids,
                          hex_grid = grid,
                          sf_id = "sa1_7dig_2011",
                          hex_size = 0.003, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          width = 30, verbose = TRUE) # same column used in create_centroids
)

# save locations of hexagons as rda and csv
save(sa1_melb003, file = "small_melb_city003.rda")
write.csv(sa1_melb003, file = "data/sa1_melb003.csv")

# Alternatively if the file exists already
#load("small_melb_city003.rda")

# convert to polygons for plotting
fort_sa1 <- fortify_sfc(sa1)
hex_sa1 <- fortify_hexagon(sa1_melb, "sa1_7dig_2011", hex_size = 0.003)



melb_sa1 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = sa1_7dig_2011),
               fill = "lightgrey", colour = "white", size = 0.003,
               data = fort_sa1) + 
  geom_polygon(aes(x = long, y = lat, group = sa1_7dig_2011,
               fill = focal_dist), colour = NA,
               data = hex_sa1) +
  # colour the hexagons for a variable
     scale_fill_distiller(type = "seq", palette = "BuGn",
                        direction = 1, na.value = "lightgrey") + 
  coord_sf(crs = "+proj=longlat +datum=WGS84") +
  theme_minimal() +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))
melb_sa1 



ggsave(filename = "figures/aus_sa1_003.pdf", plot = melb_sa1,
       device = "pdf", bg = "transparent", dpi = 600,  width = 10, height = 8)

