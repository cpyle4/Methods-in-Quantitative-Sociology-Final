path = "C:/Users/cmpyl/Documents/M1 2025-2026/QS/Final/"
#File path variable

library(sf)
library(ggplot2)
library(dplyr)
library(ggrepel) #Create labeled maps
library(gt) #Export table as .html

#Load neighborhood boundary and demographic attribute data
bounds <- read_sf(paste0(path,"data/sfnh.geojson"))
demographics <- read.csv(paste0(path,"data/sfnh_dem.csv"))

#Join attributes to neighborhood bounds
nhDemographics = merge(bounds, demographics)

#Create neighborhood racial typology variable
nhDemographics <- nhDemographics %>%
  mutate(
      racialTypology = case_when(
      pwhite > 70 ~ "White",
      pwhite <= 30 ~ "Minority",
      TRUE ~ "Mixed"
      )
  )

#Create x & y coordinates for neighborhood centroids to plot neighborhood names 
nhCentroids <- st_centroid(nhDemographics)
centroidCoords <- st_coordinates(nhCentroids)
nhCentroids <- nhCentroids %>%
  mutate(
    x = centroidCoords[, 1],
    y = centroidCoords[, 2]
  )

#Create labeled neighborhood map
ggplot(data = bounds) +
  geom_sf(fill = "beige",
          size = 0.1,
          color = "black"
          ) +
  geom_label_repel(data = nhCentroids,
                   aes(x = x, 
                       y = y,
                       label = nhood),
                   size = 2
                   ) +
  theme_void()
ggsave("labelmap.png", path = path)

#Create typology map
ggplot(data = nhDemographics) + 
  geom_sf(aes(fill = racialTypology), 
          size = 0.1,
          color = "white") +
  scale_fill_manual(values = c("White" = "deepskyblue",  #Specify colors corresponding to each category
                               "Mixed" = "seagreen2", 
                               "Minority" = "forestgreen")
  ) +
  theme_void() +
  labs(fill = "Racial Typology Category", 
       title = "Racial Typology of SF Neighborhoods"
  )
ggsave("demographicsMap.png", path = path)

#Import business data, convert to spatial format
bizSF = read.csv(paste0(path,"data/sfbiz_by_type.csv"))

bizSF <- st_as_sf(
  bizSF,
  coords = c("longitude", "latitude"),
  crs = 4326
)

#Mutate essential/discretionary business variable
bizSF <- bizSF %>%
  mutate(
    categoryDE = case_when(
      biz_type %in% c("art", "coffee") ~ "Discretionary",
      biz_type %in% c("grocery", "barber") ~ "Essential",
      TRUE ~ "Other"
    )
  )

#Plot businesses by essential/discretionary
pointMap <- ggplot() +
  geom_sf(
    data = bounds,
    fill = "beige",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = bizSF,
    aes(shape = categoryDE, fill = categoryDE),
    size = 0.6
  ) +
  scale_shape_manual(
    values = c(
      "Discretionary" = 23,
      "Essential" = 25
    )
  ) +
  labs(title = "Essential vs. Discretionary Businesses in San Francisco",) +
  guides(
    shape = guide_legend("Business Category"),
    fill  = guide_legend("Business Category")
  ) +
  theme_minimal()
  
#Perform spatial join
bizNH <- st_join(x = bounds,
  y = bizSF,
  join = st_contains,
  left = TRUE
)

#Count businesses per neighborhood
bizCounts <- bizNH %>%
  st_drop_geometry() %>%
  group_by(nhood) %>%
  summarise(nBiz = sum(!is.na(company)))

#Merge business count with neighbodhood data
bizFinal <- bounds %>%
  left_join(bizCounts, by = "nhood")

#Create centroids, plot proportional symbol map for number of businesses by neighborhood
bizPts <- st_centroid(bizFinal)
propSymMap <- ggplot() +
  geom_sf(
    data = bounds,
    fill = "beige",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = bizPts,
    aes(size = nBiz),
    shape = 21,
    fill = "red",
    alpha = 0.7,
    color = "black"
  ) +
  scale_size_area(
    max_size = 12,
    breaks = c(0, 25, 50, 100),
    labels = c("0", "25", "50", "100")
  ) +
  theme_minimal() +
  labs(
    size = "Number of Businesses",
    title = "Proportional Symbol Map of Businesses in San Francisco"
  )

#Create combined figure of both business maps
combined <- ggpubr::ggarrange(pointMap, propSymMap, nrow=1, ncol=2,
                              labels = c("Point Map", "PropSymbol Map"))

#Save the combined map
ggsave("sfMaps.png", plot = combined, path = path,
       width = 18, height = 9, 
       dpi = 300
)

#Load cultural district data, calculate centroids for distances
cultural <- read_sf(paste0(path,"data/cultural_district.geojson"))
cultCentroids <- st_centroid(cultural)
centroidCoordsCult <- st_coordinates(cultCentroids)
cultCentroids <- cultCentroids %>%
  mutate(
    x = centroidCoordsCult[, 1],
    y = centroidCoordsCult[, 2]
  )

#Create labeled map of cultural districts
ggplot(data = bounds) +
  geom_sf(fill = "beige",
          size = 0.1,
          color = "black"
  ) +
  geom_sf(data = cultural,
          fill = "violet",
          alpha = 0.6) +
  geom_label_repel(data = cultCentroids,
                   aes(x = x,
                       y = y,
                       label = district_name),
                   size = 2)
ggsave("culturalDistrict.png", path = path)

#Compute pairwise distances from neighborhood to cultural district centroids, convert to dataframe
distances <- st_distance(nhCentroids, cultCentroids)
distance_df <- as.data.frame(distances)

#Rename columns
colnames(distance_df) <- cultCentroids$district_name
rownames(distance_df) <- nhCentroids$nhood

#Reshape to long format
distanceLong <- distance_df %>%
  rownames_to_column("nhood") %>% 
  pivot_longer(
    cols = -nhood,
    names_to = "Cultural District",
    values_to = "Distance"        
  )

#Identify nearest distance cultural district
nearestCultural <- distanceLong %>%
  group_by(nhood) %>%
  slice_min(order_by = Distance)

#Drop all values except Bayview Hunters Point, Outer Richmond, Potrero Hill, and Castro/Upper Market
keeps <- nearestCultural[c(1, 3, 26, 29), ]

#Merge with demographic data, drop geometry column
keepsFinal <- merge.data.frame(keeps, nhDemographics, by="nhood") %>%
  mutate("geometry" = NULL) %>%
  rename(Neighborhood = nhood)

#Export table as HTML (to be pasted into doc)
keepsFinalGT = gt(keepsFinal)
gtsave(keepsFinalGT, "formatted_table.html", path = path)
