install.packages("terra")
# Read inventory data
filepath <- "/Users/ashleyshieh/Documents/GitHub/ForL2/files/"
library(terra)
dem <- rast(paste0(filepath, "unit2.img"))
dem
df_dem <- as.data.frame(dem, xy = TRUE)

#Extract Slope
slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)

#Extract Aspect
aspect <- terrain(dem, v = "aspect", unit = "degrees")

#Visualize Slope and Aspect
install.packages("tmap")
library(tmap)
ttm() # Switch to interactive mode
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")

tm_shape(aspect) + tm_raster(style = "cont")

# Create Aspect Classification Matrix
asp_class <- matrix(c(
  0, 45, 1,
  45, 90, 2,
  90, 175, 2,
  175, 180, 3,
  180, 225, 3,
  225, 270, 4,
  270, 315, 4,
  315, 360, 1
), ncol = 3, byrow = TRUE)

# Reclassify Aspect
asp <- classify(aspect, asp_class)

# Visualize Reclassified Aspect
ttm()
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)

# Read Summary Table and Shapefile
sum_u2 <- read.csv(paste0(filepath, "sum_u2.csv"))
library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017.shp"))
svy_pts <- st_transform(svy_pts, 32616) # Project to WGS 84 UTM 16 N
survey_pts <- subset(svy_pts, Unit == '2') # Subset for unit 2

# Merge Summary Table with Plot Locations
sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)
unique(sum_u2$Plot) #check plot
unique(survey_pts$Plot)

# Convert to `sf` Format
sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
sum_u2

# Create Circular Plots - Buffer Zones

sf_plot <- st_buffer(sum_u2, dist = 17.83)

# Unify Coordinate Systems - Check CRS
crs(sf_plot , proj=T)
crs(asp , proj=T)

# Unify Coordinate Systems -Transform CRS
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# Visualization - Dominant Species by Aspect
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)

# Visualization - Dominant Species by Slope
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

# Visualization - Basal Area (BA) Distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette = "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# Visualization - Trees Per Acre (TPA) Distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# Visualization - Biomass Distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette = "brewer.spectral") +
  tm_layout(title = "Biomass Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()



