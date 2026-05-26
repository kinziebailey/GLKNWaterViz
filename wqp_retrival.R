# Information ----
# This code will need to be run annually, after the WQP data have been 
# updated. It will make sure the downloaded data are in the correct format 
# to run with the GLKNWaterViz dashboard. It will only need to be run once 
# before the dashboard is updated and republished. 
# 
# Before this code is run. Please make sure stations.csv, chr_lookup.csv, and
# thresholds.csv are up to date. 


# Loading required packages ----
library(dataRetrieval) # download from WQP
library(readr) # tidyverse data import
library(dplyr) # data wrangling

# Loading data ----
# station data 
glkn_stations <- read_csv("./Data/station.csv")

# char lookup data 
chr_lookup <- read_csv("./Data/chr_lookup.csv")

# wqp threshold data 
thresholds <- read_csv("./Data/thresholds.csv")

# Getting WQP Data ---- 
# Looping through parks to get WQP data
WQPViews <- lapply(sort(unique(glkn_stations$Park)), function(park){
  
  # Getting site ID for park
  sites <- glkn_stations |>
    filter(Park == park) |>
    pull(MonitoringLocationIdentifier)
  
  message("Pulling WQP data for ", park)
  
  # Getting WQP Data
  dat <- readWQPdata(siteid = sites)
  
  dat
})

# creating dataframe
wqp_data_all <- bind_rows(WQPViews)

# Data Wrangling ----
## CharacteristicNames to remove ----
chr_terms <- c("Age",
               "Carbon, isotope of mass 13",
               "Carbon-13/Carbon-12 ratio",
               "Chlorophyll/Pheophytin ratio",
               "Cloud cover (choice list)",
               "External condition (text)",                           
               "General observation (text)",                          
               "Head Capsule Width",                                  
               "Hind Femur Length",                                   
               "Hindwing Length",                                     
               "Length",                                              
               "Length, total",
               "Mercury",
               "Methylmercury(1+)",
               "Nitrogen-15",
               "Nitrogen-15/Nitrogn-14 ratio",
               "Secchi Reading Condition (choice list)",
               "Sex (choice list)",
               "Water appearance (text)",
               "Wave height",
               "Weather coments (text)",
               "Weight",
               "Wind Condition (choice list)",
               "Wind direction (direction from, expressed 0-360 deg)")

## removing unneeded data ----
wqp_data1 <- wqp_data_all |> 
  # removing quality control
  filter(!grepl("Quality Control",
                ActivityTypeCode)) |> 
  # removing air and other
  filter(!grepl("Air|Other",
                ActivityMediaName)) |> 
  # removing low detection
  filter(!grepl("Quantification Limit|Not Detected|Not Reported",
                ResultDetectionConditionText)) |> 
  filter(!CharacteristicName %in% chr_terms) |> 
  # filter(!grepl(paste(chr_terms,
  #                     collapse = "|"),
  #               CharacteristicName)) |>
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
         # ,
         #        SiteCode = as.character(str_split(MonitoringLocationIdentifier, # I'm not sure this is the correct information
         #                             pattern = "_") |>
         #          map(~ paste(.x[1:2],
         #                      collapse = "_"))),
         SiteCodeWQX = SiteCode, # I'm not sure this is the correct information? 
         # DisplayName = CharacteristicName,
         # DataName = CharacteristicName,
         # Category = CharacteristicName,
         # CategoryDisplay = CharacteristicName,
         # LowerPoint = 0,
         # UpperPoint = 0,
         # LowerDescription = "Nothing",
         # UpperDescription = "Nothing",
         DataType = "numeric",
         AssessmentDetails = "Nothing",
         IsActiveSiteCode = TRUE,
         IsActiveCharacteristicName = TRUE,
         IsActive = TRUE) |> 
  # select(-SiteCode) |> 
  rename(ParkCode = Park,
         SiteName = MonitoringLocationName,
         # SiteCode = MonitoringLocationName,
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
