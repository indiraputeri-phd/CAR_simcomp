load("data/lop_kc.RData")
lop_kc$ID <- 1: nrow(lop_kc)
centroid_coord  <- sf::st_coordinates(st_centroid(st_geometry(lop_kc)))
lop_kc$centroid <- centroid_coord
lopkc.nb        <- poly2nb(lop_kc, queen = FALSE)

lopkc.nb[[8]]  <- c(10L, 11L, 12L, 13L)
lopkc.nb[[9]]  <- c(1L, 2L, 6L,  7L, 14L)
lopkc.nb[[14]] <- c(2L, 4L, 9L, 15L)
lopkc.nb[[13]] <- c(8L, 16L)
lopkc.nb[[15]] <- c(4L, 10L, 12L, 14L, 18L, 27L)
lopkc.nb[[18]] <- c(15L, 17L, 21L, 27L)
lopkc.nb[[21]] <- c(17L, 18L, 19L, 22L, 25L, 34L, 47L)
lopkc.nb[[23]] <- c(20L, 22L, 24L, 28L)
lopkc.nb[[24]] <- c(11L, 16L, 20L, 23L)
lopkc.nb[[25]] <- c(19L, 21L, 22L, 26L, 28L)
lopkc.nb[[36]] <- c(33L, 45L, 46L)

lopkc.nb[[40]] <- c(42L, 35L)
lopkc.nb[[42]] <- c(44L, 40L, 49L)
lopkc.nb[[44]] <- c(35L, 48L)
lopkc.nb[[43]] <- c(33L, 34L, 37L, 45L, 47L)
lopkc.nb[[48]] <- c(29L, 35L, 44L)
lopkc.nb[[49]] <- c(42L, 51L)
lopkc.nb[[50]] <- c(51L, 53L)
lopkc.nb[[51]] <- c(49L, 50L)
lopkc.nb[[52]] <- c(53L)
lopkc.nb[[53]] <- c(50L, 52L)


# Convert the updated neighbor list to spatial lines
nb_lines <- nb2lines(lopkc.nb, 
                     coords = st_coordinates(st_centroid(st_geometry(lop_kc))),
                     proj4string = st_crs(lop_kc)$proj4string)

# Convert the lines to an sf object for plotting
nb_lines_sf <- st_as_sf(nb_lines)


# Plot the polygons and neighbor links
ggplot() +
  geom_sf(data = lop_kc, fill = NA, color = "grey") +  # Plot the polygons
  geom_sf(data = nb_lines_sf, color = "black", size = 0.5) +  # Plot the neighbor links
  geom_sf_text(data = lop_kc, aes(label = ID), size = 2, color = "red") +
  labs(title = "Polygons with Labels") +
  theme_minimal()

library(leaflet)

# Create your leaflet map
leaflet_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addTiles() %>%
  addPolylines(data = nb_lines_sf, color = "red", weight = 2) %>%
  addPolygons(data = lop_kc, fillColor = NA, color = "grey", weight = 2) 
  