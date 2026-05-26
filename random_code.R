

full_join(
  tibble(column = names(meta_data), in_glkn = TRUE),
  tibble(column = names(ncrn_meta), in_ncrn = TRUE),
  by = "column"
) %>%
  mutate(
    in_glkn = if_else(is.na(in_glkn), FALSE, TRUE),
    in_ncrn = if_else(is.na(in_ncrn), FALSE, TRUE)
  ) %>%
  dplyr::filter(
    in_glkn==F | in_ncrn==F
  ) |>  View()


chr_terms <- c("Wave height",
               "Water appearance",
               "Chlorophyll/Pheophytin ratio",
               "External condition",
               "Head Capsule Width",
               "Length",
               # "Hindwing Length",
               # "Hind Femur Length",
               "Mercury",
               "Methylmercury",
               "Carbon, isotope of mass 13",
               "Age",
               "Sex",
               "Weight",
               "Secchi Reading Condition",
               "Carbon-13/Carbon-12 ratio",
               "Nitrogen-15",
               "Nitrogen-15/Nitrogn-14 ratio")

# Data Wrangling ----
## removing unneeded data ----
wqp_data1 <- wqp_data_all |> 
  # removing quality control
  filter(!grepl("Quality Control",
                ActivityTypeCode)) |> 
  # removing air and other
  filter(!grepl("Air|Other",
                ActivityMediaName)) |> 
  # removing charateristicnames 
  filter(!grepl(paste(chr_terms,
                      collapse = "|"),
                CharacteristicName)) |> 
  # changing phosphorus if depth measurement is not 0
  mutate(CharacteristicName = case_when(ActivityDepthHeightMeasure.MeasureValue != 0 &
                                          CharacteristicName == "Total Phosphorus, mixed forms" ~ "Bottom Phosphorus",
                                        TRUE ~ CharacteristicName),
         # making numeric
         ResultMeasureValue = as.numeric(ResultMeasureValue))





# manually gets rid of local caches of the package
unlink(file.path(.libPaths()[1], "NCRNWater"), recursive = TRUE, force = TRUE)
unlink(tempdir(), recursive = TRUE, force = TRUE)


# manually gets rid of local caches of the package
unlink(file.path(.libPaths()[1], "NCRNWater"), recursive = TRUE, force = TRUE)
unlink(tempdir(), recursive = TRUE, force = TRUE)
library(remotes)
options(download.file.method = "wininet")
remotes::install_github('https://github.com/ncrn/ncrnwater', ref = "site_dupe_bugfix")



# figuring out coding for depth averages in plots


depth_filter <- wqp_data |> 
  filter(is.na(ActivityDepthHeightMeasure.MeasureValue) == TRUE |
           ActivityDepthHeightMeasure.MeasureValue >= -2) 

avg_data <- depth_filter |> 
  # group_by(MonitoringLocationIdentifier,
  #          ActivityEndDate,
  #          CharacteristicName) |> 
  summarise(n_depths = sum(!is.na(ActivityDepthHeightMeasure.MeasureValue)),
            result_summary = case_when(n_depths == 0 ~ ResultMeasureValue[1],
                                       n_depths == 1 ~ ResultMeasureValue[!is.na(ActivityDepthHeightMeasure.MeasureValue)][1],
                                       n_depths == 2 ~ mean(ResultMeasureValue[!is.na(ActivityDepthHeightMeasure.MeasureValue)], 
                                                            na.rm = TRUE),
                                       n_depths >= 3 ~ median(ResultMeasureValue[!is.na(ActivityDepthHeightMeasure.MeasureValue)], 
                                                              na.rm = TRUE)),
            .by = c(MonitoringLocationIdentifier,
                    ActivityEndDate,
                    CharacteristicName))
  
  

ggplot() + 
  geom_point(data = wqp_data,
             aes(x = ActivityStartDate,
                 y = ResultMeasureValue))
