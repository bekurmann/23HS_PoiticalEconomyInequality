# ##############################################################
# plot
# ##############################################################

# dependencies
library(tidyverse)
library(sf)
library(viridis)
library(here)

# checkout wd
here()

getwd()
setwd(here())
getwd()

# ##############################################################
# theming
theme_map <- function(...) {
  theme_minimal() +
    theme(
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
    )
}
# ##############################################################
# loading data
data <- readRDS("data/data.rds")

# loading swiss shapefile
# von hier: https://www.swisstopo.admin.ch/de/geodata/landscape/boundaries3d.html#download
swiss_map <- st_read("data/shapefile-new/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp")

# loading even better shapefile(s)
# https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/base-maps/cartographic-bases.assetdetail.30566934.html
swiss_municipalities <- read_sf("data/shapefile-newer/01_INST/Vegetationsfläche_vf/K4_polg20240101_vf/K4polg20240101vf_ch2007Poly.shp")
# lakes and rivers
swiss_lakes1 <- read_sf("data/shapefile-newer/00_TOPO/K4_seenyyyymmdd/k4seenyyyymmdd11_ch2007Poly.shp")
swiss_lakes2 <- read_sf("data/shapefile-newer/00_TOPO/K4_seenyyyymmdd/k4seenyyyymmdd22_ch2007Poly.shp")
# Compile the data into one table
swiss_lakes <- rbind(swiss_lakes1, swiss_lakes2)
river1_geo <- read_sf("data/shapefile-newer/00_TOPO/K4_flusyyyymmdd/k4flusyyyymmdd11_ch2007.shp")
river2_geo <- read_sf("data/shapefile-newer/00_TOPO/K4_flusyyyymmdd/k4flusyyyymmdd22_ch2007.shp")
river3_geo <- read_sf("data/shapefile-newer/00_TOPO/K4_flusyyyymmdd/k4flusyyyymmdd33_ch2007.shp")
river4_geo <- read_sf("data/shapefile-newer/00_TOPO/K4_flusyyyymmdd/k4flusyyyymmdd44_ch2007.shp")
river5_geo <- read_sf("data/shapefile-newer/00_TOPO/K4_flusyyyymmdd/k4flusyyyymmdd55_ch2007.shp")
# Compile the data into one table
swiss_rivers <- rbind(river1_geo,river2_geo,river3_geo,river4_geo,river5_geo)

# check out projection (and crs)
st_crs(swiss_map)

# ##############################################################
# Merge your data with the Swiss map data based on a common identifier (e.g., GemeindeNummer)
# in swiss_map: GMDNR
# in data:      gdenr
merge_swiss_map <- merge(swiss_map, data, by.x = "BFS_NUMMER", by.y = "gdenr")
merge_swiss_municipalities <- merge(swiss_municipalities, data, by.x = "id", by.y = "gdenr")

# ##############################################################
# plot map (settlement area) Gini-Index in %
ggplot() +
  # basemap
  geom_sf(data = swiss_map, fill = "white", color="grey") + 
  # Main municipal layer with Viridis color scale
  geom_sf(data = merge_swiss_municipalities, aes(fill = gini_steink_percent), color = "white", size = 0.1) +
  scale_fill_viridis(option = "magma", alpha = 0.8, begin = 0.1, end = 0.9, direction = 1,
                     guide = guide_legend(keyheight = unit(5, "mm"), title.position = "top", reverse = TRUE)) + 
  # Lakes and rivers
  geom_sf(data = swiss_lakes, fill = "#D6F1FF", color = "#D6F1FF") +
  geom_sf(data = swiss_rivers, fill = "#D6F1FF", color = "#D6F1FF") +
  # Titles and theme
  labs(x = NULL, y = NULL, title = "Swiss Municipalities (settlement area) and Gini-Index (in %)") +
  # add theme (function above)
  theme_map()

# ##############################################################
# find out min und max (for scale_fill_viridis)
summary(merge_swiss_municipalities$median_steink)
# plot map (settlement area) median income
ggplot() +
  # basemap
  geom_sf(data = swiss_map, fill = "white", color="grey") + 
  # Main municipal layer with Viridis color scale
  geom_sf(data = merge_swiss_municipalities, aes(fill = median_steink), color = "white", size = 0.1) +
  scale_fill_viridis(option = "magma", limits = c(17000, 100000),
                     alpha = 0.8, begin = 0.1, end = 0.9, direction = 1,
                     guide = guide_legend(keyheight = unit(5, "mm"), title.position = "top", reverse = TRUE)) + 
  # Lakes and rivers
  geom_sf(data = swiss_lakes, fill = "#D6F1FF", color = "#D6F1FF") +
  geom_sf(data = swiss_rivers, fill = "#D6F1FF", color = "#D6F1FF") +
  # Titles and theme
  labs(x = NULL, y = NULL, title = "Swiss Municipalities (settlement area) and median income") +
  # add theme (function above)
  theme_map()

# ##############################################################
# find out min und max (for scale_fill_viridis)
summary(merge_swiss_municipalities$mean_steink)
# plot map (settlement area) average income
ggplot() +
  # basemap
  geom_sf(data = swiss_map, fill = "white", color="grey") + 
  # Main municipal layer with Viridis color scale
  geom_sf(data = merge_swiss_municipalities, aes(fill = mean_steink), color = "white", size = 0.1) +
  scale_fill_viridis(option = "magma", limits = c(17500, 110000),
                     alpha = 0.8, begin = 0.1, end = 0.9, direction = 1,
                     guide = guide_legend(keyheight = unit(5, "mm"), title.position = "top", reverse = TRUE)) + 
  # Lakes and rivers
  geom_sf(data = swiss_lakes, fill = "#D6F1FF", color = "#D6F1FF") +
  geom_sf(data = swiss_rivers, fill = "#D6F1FF", color = "#D6F1FF") +
  # Titles and theme
  labs(x = NULL, y = NULL, title = "Swiss Municipalities (settlement area) and average income") +
  # add theme (function above)
  theme_map()

# ##############################################################
# plot map turnout 2023
ggplot() +
  # basemap
  geom_sf(data = swiss_map, fill = "white", color="grey") + 
  # Main municipal layer with Viridis color scale
  geom_sf(data = merge_swiss_municipalities, aes(fill = wahlbeteiligung2023), color = "white", size = 0.1) +
  scale_fill_viridis(option = "magma", alpha = 0.8, begin = 0.1, end = 0.9, direction = 1,
                     guide = guide_legend(keyheight = unit(5, "mm"), title.position = "top", reverse = TRUE)) + 
  # Lakes and rivers
  geom_sf(data = swiss_lakes, fill = "#D6F1FF", color = "#D6F1FF") +
  geom_sf(data = swiss_rivers, fill = "#D6F1FF", color = "#D6F1FF") +
  # Titles and theme
  labs(x = NULL, y = NULL, title = "Switzerland's Turnout on 2019 Election", 
       subtitle = "Turnout per Municipality in %") +
  # add theme (function above)
  theme_map()

# ##############################################################
# plot map bivariate turnout + gini

# Categorize your data (this is a simplification; you should use appropriate cutoffs)
merge_swiss_municipalities$gini_category <- cut(
  merge_swiss_municipalities$gini_steink_percent, 
  breaks=c(-Inf, 40, 60, Inf), 
  labels=c("g_Low", "g_Medium", "g_High"))

merge_swiss_municipalities$turnout_category <- cut(
  merge_swiss_municipalities$wahlbeteiligung2023, 
  breaks=c(-Inf, 40, 60, Inf), 
  labels=c("Low", "Medium", "High"))

# Step 3: Combine the categories
merge_swiss_municipalities$combined_category <- paste(
  merge_swiss_municipalities$gini_category, 
  merge_swiss_municipalities$turnout_category)

# Step 4: Define a color for each combined category
color_mapping <- c("g_Low Low" = "#CABED0", 
                   "g_Low Medium" = "#BC7C8F", 
                   "g_Low High" = "#AE3A4E",
                   "g_Medium Low" = "#89A1C8", 
                   "g_Medium Medium" = "#806A8A", 
                   "g_Medium High" = "#77324C",
                   "g_High Low" = "#4885C1",
                   "g_High Medium" = "#435786", 
                   "g_High High" = "#3F2949")

ggplot() +
  geom_sf(data = swiss_map, fill = "white", color="grey") +
  geom_sf(data = merge_swiss_municipalities, aes(fill = combined_category), color = "white", size = 0.1) +
  scale_fill_manual(values=color_mapping) +
  geom_sf(data = swiss_lakes, fill = "#D6F1FF", color = "#D6F1FF") +
  geom_sf(data = swiss_rivers, fill = "#D6F1FF", color = "#D6F1FF") +
  labs(x = NULL, y = NULL, title = "Bivariate Map of Swiss Municipalities", subtitle = "Combining Gini Index and Voter Turnout Election 2023") +
  theme_map() +
  theme(legend.position = "right")
