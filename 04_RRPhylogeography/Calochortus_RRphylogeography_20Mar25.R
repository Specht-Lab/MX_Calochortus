#Setwd

#Install and Load Required Packages 
library(RRgeo)
library(terra)
library(sf)
library(raster)
library(readxl)
library(dplyr)

############################MY REAL DATA##########################################
############################ FUSCUS / PRINGLEI ##########################################
########## LOAD IN SDMs ##########################
map1 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/fuscus_avg.asc")
map2 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/pringlei_avg.asc")

########## Occurrence Data #########################
fuscus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/fuscus_occs_with_geometry.csv")
pringlei_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/pringlei_occs_with_geometry.csv")

# Convert the geometry column (WKT format) to sf points
# Convert the geometry column to actual spatial points using st_as_sf
fuscus_sf <- st_as_sf(fuscus_occs, wkt = "geometry", crs = 4326)
pringlei_sf <- st_as_sf(pringlei_occs, wkt = "geometry", crs = 4326)

# Create the occurrence data list (which is now an sf object)
occs <- list(Calochortus_fuscus = fuscus_sf, Calochortus_pringlei = pringlei_sf)

# Now both fuscus_sf and pringlei_sf are sf objects, and you can check them
print(fuscus_sf)
print(pringlei_sf)

# Create the prediction map list
pred <- list(Calochortus_fuscus = map1, Calochortus_pringlei = map2)


# Run the RRphylogeography analysis
RRphylogeography(spec1 = "Calochortus_fuscus",
                        spec2 = "Calochortus_pringlei",
                        pred = pred,
                        occs = occs,
                        aggr = 20,
                        time_col = NULL,
                        resistance_map = NULL,
                        th = 0.5,
                        clust = 0.5,
                        plot = TRUE,
                        mask_for_pred = NULL,
                        standardize = TRUE,
                        output.dir = '.')
############################ VENUSTULUS / MARCELLAE ##########################################
### LOAD IN OCC DATA ###
venustulus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/venustulus_occs_with_geometry.csv")
marcellae_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/marcellae_occs_with_geometry.csv")
str(venustulus_occs)
str(marcellae_occs)

# Convert the geometry column to actual spatial points using st_as_sf
venustulus_sf <- st_as_sf(venustulus_occs, wkt = "geometry", crs = 4326)
marcellae_sf <- st_as_sf(marcellae_occs, wkt = "geometry", crs = 4326)

# Create the occurrence data list (which is now an sf object)
occs <- list(Calochortus_venustulus = venustulus_sf, Calochortus_marcellae = marcellae_sf)
str(occs)

### LOAD IN SDMs ###
map1 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/venustulus_avg.asc")
map2 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/marcellae_avg.asc")
str(map1)
str(map2)

# Create the prediction map list
pred <- list(Calochortus_venustulus = map1, Calochortus_marcellae = map2)

# Run the RRphylogeography analysis
RRphylogeography(spec1 = "Calochortus_venustulus",
                 spec2 = "Calochortus_marcellae",
                 pred = pred,
                 occs = occs,
                 aggr = 20,
                 time_col = NULL,
                 resistance_map = NULL,
                 th = 0.5,
                 clust = 0.5,
                 plot = TRUE,
                 mask_for_pred = NULL,
                 standardize = TRUE,
                 output.dir = '.')

############################ SPATULATUS / CERNUUS ##########################################
### LOAD IN OCC DATA ###
spatulatus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/spatulatus_occs_with_geometry.csv")
cernuus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/cernuus_occs_with_geometry.csv")
str(spatulatus_occs)
str(cernuus_occs)

# Convert the geometry column to actual spatial points using st_as_sf
spatulatus_sf <- st_as_sf(spatulatus_occs, wkt = "geometry", crs = 4326)
cernuus_sf <- st_as_sf(cernuus_occs, wkt = "geometry", crs = 4326)

# Create the occurrence data list (which is now an sf object)
occs <- list(Calochortus_spatulatus = spatulatus_sf, Calochortus_cernuus = cernuus_sf)
str(occs)

### LOAD IN SDMs ###
map1 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/spatulatus_avg.asc")
map2 <- rast("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/cernuus.asc")
str(map1)
str(map2)

# Create the prediction map list
pred <- list(Calochortus_spatulatus = map1, Calochortus_cernuus = map2)

# Run the RRphylogeography analysis
RRphylogeography(spec1 = "Calochortus_spatulatus",
                 spec2 = "Calochortus_cernuus",
                 pred = pred,
                 occs = occs,
                 aggr = 20,
                 time_col = NULL,
                 resistance_map = NULL,
                 th = 0.5,
                 clust = 0.5,
                 plot = TRUE,
                 mask_for_pred = NULL,
                 standardize = TRUE,
                 output.dir = '.')

############################ END OF RRgeo CODE ##########################################

#############CONVERTING OCCURENCE POINTS FROM DECIMAL TO WKT #########

##Occurrence Points

spatulatus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/spatulatus_occs.csv")
cernuus_occs <- read.csv("/Users/jjh374/Desktop/RRphylogeography_code_data/x_calochortus_20Mar25/cernuus_occs.csv")

# Function to convert longitude and latitude to WKT
convert_to_wkt <- function(df) {
  # Ensure the longitude and latitude columns exist
  if ("decimalLongitude" %in% colnames(df) & "decimalLatitude" %in% colnames(df))  {
    #Convert to an sf object assuming WGS84 (EPSG:4326)
    df_sf <- st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
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
spatulatus_wkt <- convert_to_wkt(spatulatus_occs)
cernuus_wkt <- convert_to_wkt(cernuus_occs)

# Add the WKT points as new columns in the data frames
spatulatus_occs$WKT <- spatulatus_wkt
cernuus_occs$WKT <- cernuus_wkt

# Print the first few rows to verify
head(spatulatus_occs$WKT)
head(cernuus_occs$WKT)

# Rename 'WKT' column to 'geometry'
colnames(spatulatus_occs)[colnames(spatulatus_occs) == "WKT"] <- "geometry"
colnames(cernuus_occs)[colnames(cernuus_occs) == "WKT"] <- "geometry"

# Save the dataframes to new files (e.g., as CSV files)
write.csv(spatulatus_occs, "spatulatus_occs_with_geometry.csv", row.names = FALSE)
write.csv(cernuus_occs, "cernuus_occs_with_geometry.csv", row.names = FALSE)
