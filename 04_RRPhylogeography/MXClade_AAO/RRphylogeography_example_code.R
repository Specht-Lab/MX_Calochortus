#Setwd

#Install and Load Required Packages 
library(RRgeo)
library(terra)
library(sf)
library(raster)
library(readxl)
library(dplyr)

######## OCC DATA PREP #########################################################
#############CONVERTING OCCURENCE POINTS FROM DECIMAL TO WKT ###################

##Occurrence Points

flexuosus_occs <- read.csv("flex_occ.csv")
mendozae_occs <- read.csv("mend_occ.csv")

# Function to convert longitude and latitude to WKT
convert_to_wkt <- function(df) {
  # Ensure the longitude and latitude columns exist
  if ("longitude" %in% colnames(df) & "latitude" %in% colnames(df))  {
    #Convert to an sf object assuming WGS84 (EPSG:4326)
    df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
    
    # Check if the object is correctly created as an sf object
    if (inherits(df_sf, "sf")) {
      # Extract the geometry column and convert to WKT format
      wkt_points <- st_as_text(df_sf$geometry)
      
      # Return the WKT points
      return(wkt_points)
    } else {
      stop("Failed to convert the data frame to an sf object.")
    }
  } else {
    stop("Columns 'decimalLongitude' and 'decimalLatitude' must exist in the data frame.")
  }
}
# Convert longitude and latitude to WKT for both species
flexuosus_wkt <- convert_to_wkt(flexuosus_occs)
mendozae_wkt <- convert_to_wkt(mendozae_occs)

# Add the WKT points as new columns in the data frames
flexuosus_occs$WKT <- flexuosus_wkt
mendozae_occs$WKT <- mendozae_wkt

# Print the first few rows to verify
head(flexuosus_occs$WKT)
head(mendozae_occs$WKT)

# Rename 'WKT' column to 'geometry'
colnames(flexuosus_occs)[colnames(flexuosus_occs) == "WKT"] <- "geometry"
colnames(mendozae_occs)[colnames(mendozae_occs) == "WKT"] <- "geometry"

# Save the dataframes to new files (e.g., as CSV files)
write.csv(flexuosus_occs, "flexuosus_occs_with_geometry.csv", row.names = FALSE)
write.csv(mendozae_occs, "mendozae_occs_with_geometry.csv", row.names = FALSE)

###################### END OF OCC DATA PREP ##########################################


############################ START OF RRPHYLOGEOGRAPHY CODE ##########################################
############################ FUSCUS / mendozae ##########################################
########## LOAD IN SDMs // SDM were produced using MaxENT ##########################
map1 <- rast("flexuosus_avg.asc")
map2 <- rast("mendozae_avg.asc")

########## Occurrence Data #########################
flexuosus_occs <- read.csv("flexuosus_occs_with_geometry.csv")
mendozae_occs <- read.csv("mendozae_occs_with_geometry.csv")

# Convert the geometry column (WKT format) to sf points
# Convert the geometry column to actual spatial points using st_as_sf
flexuosus_sf <- st_as_sf(flexuosus_occs, wkt = "geometry", crs = 4326)
mendozae_sf <- st_as_sf(mendozae_occs, wkt = "geometry", crs = 4326)

# Create the occurrence data list (which is now an sf object)
occs <- list(Calochortus_flexuosus = flexuosus_sf, Calochortus_mendozae = mendozae_sf)

# Now both flexuosus_sf and mendozae_sf are sf objects, and you can check them
print(flexuosus_sf)
print(mendozae_sf)

# Create the prediction map list
pred <- list(Calochortus_flexuosus = map1, Calochortus_mendozae = map2)


# Run the RRphylogeography analysis
RRphylogeography(spec1 = "Calochortus_flexuosus",
                        spec2 = "Calochortus_mendozae",
                        pred = pred,
                        occs = occs,
                        aggr = 20, # 35 worked last time 4/3/25
                        time_col = NULL,
                        resistance_map = NULL,
                        th = 0.5,
                        clust = 0.5,
                        plot = TRUE,
                        mask_for_pred = NULL,
                        standardize = TRUE,
                        output.dir = '.')

############################ END OF RRgeo CODE ##########################################
