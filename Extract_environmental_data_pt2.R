#Don't forget to set your working directory! (1)
setwd('/Users/jjh374/Desktop/calochortus_paper/Speciation/speciation_12Aug24')

#INSTALL PACKAGES (2)
install.packages("dismo")   # For accessing WorldClim data
install.packages("rgbif")   # For accessing GBIF data
install.packages("raster")  # For working with raster data
install.packages("sp")  # For working with spatial data
install.packages("geodata") # For accessing WorldClim data
install.packages("terra") # For accessing WorldClim data
install.packages("openxlsx")

#Library Packages
library(dismo)
library(rgbif)
library(raster)
library(sp)
library(geodata)
library(terra)
library(openxlsx)

###FOR MEAN DIURNAL RANGE###
    #Load in Species Occurence Data
    occurrences <- read.csv("8Aug24mx_calo_occ.csv", header = TRUE)
        # Extract species names (assuming they are in the first column)
        species_names <- occurrences[, 1]
    
    # Ensure 'occurrences' dataframe has only 'longitude' and 'latitude' columns
    occurrences <- occurrences[, c("decimalLongitude", "decimalLatitude")]
    
    # Load the raster file (adjust the file path as necessary)
    bio_2 <- raster("wc2.1_2.5m_bio_2.tif")
    
    # Extract temperature values for occurrence points
    temperature_values <- extract(bio_2, occurrences)
    
    # Create a dataframe combining occurrence data with extracted temperatures and species names
    occurrences_with_temperatures <- cbind(species = species_names, occurrences, temperature = unlist
        (temperature_values))
    print(occurrences_with_temperatures)

    # Write the combined dataframe to a excel file
    write.xlsx(occurrences_with_temperatures, "12Aug24_bio_2.xlsx")
    ### Manually removed the lat/long columns for eco analysis

###FOR TEMPERATURE ANNUAL RANGE###
    # Load the raster file (adjust the file path as necessary)
    bio_7 <- raster("wc2.1_2.5m_bio_7.tif")
    
    # Extract temperature values for occurrence points
    temperature_values2 <- extract(bio_7, occurrences)
    
    # Create a dataframe combining occurrence data with extracted temperatures and species names
    occurrences_with_temperatures2 <- cbind(species = species_names, occurrences, temperature = unlist    
    (temperature_values2))
    print(occurrences_with_temperatures2)
    
    # Write the combined dataframe to a excel file
    write.xlsx(occurrences_with_temperatures2, "12Aug24_bio_7.xlsx")
    ###Removed the lat/long columns for eco analysis
    
###FOR PRECIPITATION SEASONALITY###
    # Load the raster file (adjust the file path as necessary)
    bio_15 <- raster("wc2.1_2.5m_bio_15.tif")
    
    # Extract temperature values for occurrence points
    precipitation_values <- extract(bio_15, occurrences)
    
    # Create a dataframe combining occurrence data with extracted temperatures and species names
    occurrences_with_precip <- cbind(species = species_names, occurrences, precipitation_seasonality =     unlist     (precipitation_values))
    print(occurrences_with_precip)
    
    # Write the combined dataframe to a excel file
    write.xlsx(occurrences_with_precip, "12Aug24_bio_15.xlsx")
    ###Removed the lat/long columns for eco analysis
    
###FOR ELEVATION###
    # Load the raster file (adjust the file path as necessary)
    elevation <- raster("wc2.1_30s_elev.tif")
    
    # Extract temperature values for occurrence points
    elevation_values <- extract(elevation, occurrences)
    
    # Create a dataframe combining occurrence data with extracted temperatures and species names
    occurrences_with_elevation <- cbind(species = species_names, occurrences, Elevation =     unlist     (elevation_values))
    print(occurrences_with_elevation)
    
    # Write the combined dataframe to a excel file
    write.xlsx(occurrences_with_elevation, "12Aug24_elevation.xlsx")   
    ###Removed the lat/long columns for eco analysis
    