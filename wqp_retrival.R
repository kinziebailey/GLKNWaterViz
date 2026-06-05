# Information ----
# This code will need to be run annually, after the WQP data have been 
# updated. It will make sure the downloaded data are in the correct format 
# to run with the GLKNWaterViz dashboard. It will only need to be run once 
# before the dashboard is updated and republished. 
# 
# Before this code is run. Please make sure you have added a Data folder to the 
# working directory. Run the following in the console: dir.create("Data")
# Please make sure stations.csv, chr_lookup.csv, and thresholds.csv are up to 
# date and added to the Data folder. In the Data folder, run the following code
# to add an additional folder for the water quality portal data:
# dir.create("Data/GLKN")

# Once those folders and data files have been added/updated, you should be able to
# run this code and then run the app.


# Loading required packages ----
# Ignore any warnings and messages from the libraries
library(dataRetrieval) # download from WQP
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(stringr) # data wrangling

# Loading data ----
# station data 
glkn_stations <- read_csv("./Data/station.csv")

# char lookup data 
chr_lookup <- read_csv("./Data/chr_lookup.csv")

# wqp threshold data 
thresholds <- read_csv("./Data/thresholds.csv")

# Getting WQP Data ---- 
# THIS IS EXTREMELY SLOW
# parks <- sort(unique(glkn_stations$Park))
# 
# WQPViews <- lapply(parks, function(park){
#   
#   sites <- glkn_stations |>
#     dplyr::filter(Park == park) |>
#     dplyr::pull(MonitoringLocationIdentifier)
#   
#   message("\nPulling WQP data for ", park)
#   
#   # Create progress bar for THIS park
#   pb <- txtProgressBar(min = 0, max = length(sites), style = 3)
#   
#   # Download each site with progress bar
#   dat_list <- vector("list", length(sites))
#   
#   for (i in seq_along(sites)) {
#     dat_list[[i]] <- suppressMessages(readWQPdata(siteid = sites[i])) |>
#       dplyr::mutate(ResultMeasureValue = as.character(ResultMeasureValue))
#     
#     setTxtProgressBar(pb, i)
#   }
#   
#   close(pb)
#   
#   # Combine all sites for this park
#   dplyr::bind_rows(dat_list)
# })
# 
# # Combine all parks
# wqp_data_all <- dplyr::bind_rows(WQPViews)


# Looping through parks to get WQP data. This will give you updated data for all
# parks and sites listed in stations.csv. 
WQPViews <- lapply(sort(unique(glkn_stations$Park)), function(park){
  
  # Getting site ID for park
  sites <- glkn_stations |>
    dplyr::filter(Park == park) |>
    dplyr::pull(MonitoringLocationIdentifier)
  
  message("Pulling WQP data for ", park)
  
  # Getting WQP Data
  dat <- tryCatch(
    {
      suppressMessages(readWQPdata(siteid = sites))
    },
    # warning: no data for site, still returns partial data
    warning = function(mess){
      warning("Warning: ", conditionMessage(mess), " while pulling for ", park)
              suppressMessages(readWQPdata(siteid = sites))
    },
    # error: park failed to download
    error = function(err){
      warning("ERROR: ", conitionMessages(err), " while pulling for ", park)
      return(NULL)
    }
    )
  
  # Standardize column type 
  if(!is.null(dat)){
  dat <- dat |> 
    dplyr::mutate(ResultMeasureValue = as.character(ResultMeasureValue))
  }
  
  dat
  
})

# creating dataframe
wqp_data_all <- bind_rows(WQPViews)

# Data Wrangling ----

## removing unneeded data ----
wqp_data1 <- wqp_data_all |> 
  # removing unneeded CharacteristicNames
  semi_join(chr_lookup,
            by = join_by(CharacteristicName)) |> 
  # removing quality control
  filter(!grepl("Quality Control",
                ActivityTypeCode)) |> 
  # removing air and other
  filter(!grepl("Air|Other",
                ActivityMediaName)) |> 
  filter(!grepl("/", 
                ActivityIdentifier)) |> 
  # adding censored data conditions
  mutate(ResultMeasureValue = case_when(ResultDetectionConditionText == "Present Below Quantification Limit" ~ 
                                          str_extract(ResultCommentText, "\\d*\\.?\\d+"),
                                        TRUE ~ ResultMeasureValue)) |> 
  # removing no detection/not reported
  # filter(!grepl("Not Detected|Not Reported",
  #               ResultDetectionConditionText)) |> # leaving these in for now so we can try to report how many values are there. 
  # correcting depth measurements
  mutate(ActivityDepthHeightMeasure.MeasureValue = if_else(ActivityDepthHeightMeasure.MeasureValue < -0.03, 0,
                                                           ActivityDepthHeightMeasure.MeasureValue),
         ActivityDepthHeightMeasure.MeasureValue = -abs(ActivityDepthHeightMeasure.MeasureValue),
         ResultMeasureValue = as.numeric(ResultMeasureValue))

## Edit column names to match NCRN data ---- 
wqp_data <- wqp_data1 |>
  # adding station data
  left_join(glkn_stations) |>
  # adding year for cleaning purposes
  mutate(year = format(ActivityStartDate, "%Y")) |> 
  # filtering by sites that have >= 5 years of data
  filter(n_distinct(year) >= 5,
         .by = c(MonitoringLocationIdentifier,
                 CharacteristicName)) |>
  select(-year) |> 
  mutate(OrganizationIdentifier = "GLKN",
         ActivityRelativeDepthName = "Depth",
         ProjectIdentifier = "USNPS GLKN Water Quality Monitoring",
         ProjectName = ProjectIdentifier) |> 
  rename(ActivityLocation.LatitudeMeasure = LatitudeMeasure,
         ActivityLocation.LongitudeMeasure = LongitudeMeasure,
         SampleCollectionMethod.MethodDescriptionText = MethodDescriptionText)

# Writing the new wqp_data ----
write_csv(wqp_data,
          "./Data/GLKN/wqp_glkn.csv")

# Creating Metadata ----

## Initial metadataset ----
meta_data_large <- wqp_data |> 
  select(MonitoringLocationIdentifier,
         CharacteristicName,
         ResultMeasure.MeasureUnitCode) |> 
  distinct(MonitoringLocationIdentifier,
           CharacteristicName,
           .keep_all = T) |> 
  left_join(glkn_stations)

## Altering Column information ----
meta_data <- meta_data_large |> 
  # adding thresholds
  left_join(thresholds) |>
  # adding char names
  left_join(chr_lookup) |>
  mutate(Network = "GLKN",
         ShortName = case_when(Park == "APIS" ~ "Apostle Islands",
                               Park == "INDU" ~ "Indiana Dunes",
                               Park == "ISRO" ~ "Isle Royale",
                               Park == "PIRO" ~ "Pictured Rocks",
                               Park == "SLBE" ~ "Sleeping Bear",
                               Park == "VOYA" ~ "Voyageurs",
                               Park == "SACN" ~ "St. Croix",
                               TRUE ~ "OTHER"),
         LongName = case_when(Park == "APIS" ~ "Apostle Islands National Lakeshore",
                              Park == "INDU" ~ "Indiana Dunes National Park",
                              Park == "ISRO" ~ "Isle Royale National Park",
                              Park == "PIRO" ~ "Pictured Rocks National Lakeshore",
                              Park == "SLBE" ~ "Sleeping Bear Dunes National Lakeshore",
                              Park == "VOYA" ~ "Voyageurs National Park",
                              Park == "SACN" ~ "St. CroixNational Scenic Riverway",
                              TRUE ~ "OTHER"),
         SiteCode = MonitoringLocationIdentifier,
         SiteCodeWQX = SiteCode, # I'm not sure this is the correct information? 
         DataType = "numeric",
         AssessmentDetails = "Nothing",
         IsActiveSiteCode = TRUE,
         IsActiveCharacteristicName = TRUE,
         IsActive = TRUE) |> 
  rename(ParkCode = Park,
         SiteName = MonitoringLocationName,
         Lat = LatitudeMeasure,
         Long = LongitudeMeasure,
         Type = MonitoringLocationTypeName,
         Units = ResultMeasure.MeasureUnitCode) |> 
  select(Network,  # selected columns to match NCRN 
         ParkCode,
         ShortName,
         LongName,
         SiteCode,
         SiteCodeWQX,
         SiteName,
         Lat,
         Long,
         Type,
         CharacteristicName,
         DisplayName,
         DataName,
         Category,
         CategoryDisplay,
         Units,
         LowerPoint,
         UpperPoint,
         DataType,
         LowerDescription,
         UpperDescription,
         AssessmentDetails,
         IsActiveSiteCode,
         IsActiveCharacteristicName,
         IsActive)

# Writing Metadata ----
write_csv(meta_data,
          "./Data/GLKN/MetaData.csv")
