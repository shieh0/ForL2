setwd('C:/Users/luizv/OneDrive_Purdue/OneDrive - purdue.edu/Spring 2025/AGR 33300/Forestry_lab2')
getwd()


############
1.	Read and Explore the DEM


# Read inventory data
install.packages('terra')
library(terra)
dem <- rast(paste0('./files/', "unit2.img"))
dem

###############
# 2.	Extract Slope and Aspect
#############

# a. Extract Slope
slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)

# b. Extract Aspect:
aspect <- terrain(dem, v = "aspect", unit = "degrees")

# c. Visualize Slope and Aspect:

#install.packages("tmap")
library(tmap)
ttm() # Switch to interactive mode
tm_shape(slope, alpha = 0.5) +
      tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")

tm_shape(aspect) +
      tm_raster(style = "cont")

####################
# 3.	Reclassify Aspect
####################

# a. Create Aspect Classification Matrix:
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


# b. Reclassify Aspect:
asp <- classify(aspect, asp_class)

# c. Visualize Reclassified Aspect:
ttm()
tm_shape(asp) +
      tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
                labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)

##################
# 4.	Visualize Sample Forest Inventory Plots 
##################

sum_u2 <- read.csv('https://raw.githubusercontent.com/DS4Ag/Forestry_lab1_answers/refs/heads/main/sum_u2.csv')


library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017 - Copy.shp"))
svy_pts <- st_transform(svy_pts, 32616) # Project to WGS 84 UTM 16 N
survey_pts <- subset(svy_pts, Unit == '2') # Subset for unit 2


