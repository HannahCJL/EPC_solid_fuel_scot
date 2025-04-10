# Scotland solid fuel burning
# EPC

# 1/4/25

# File paths ----
setwd("P:/08092_ScotGov_AirQuality/Data/D1_2_PM_WorkDayTask1/")
wd <- "./ImprovingSpatialDomesticSF/Data/"
uprn_fp <- "./DomesticBurningStats/UPRN Data/Data/"
epc_fp <- "./DomesticBurningStats/D_EPC_data_2015Q1-2024Q4/"

# Packages ----
pacman::p_load("dplyr","stringr","ggplot2","readr","DT","tidyr","sf","terra")

# Exploratory analysis  ----

epc_24q4 <- read_csv(str_c(epc_fp, "2024Q4.csv"))
cols_epc <- data.frame(colnames(epc_24q4))

# Mainheat
mainheat_cols <- str_subset(colnames(epc_24q4),"MAINHEAT")
hotwater_cols <- str_subset(colnames(epc_24q4),"HOTWATER|HOT_WATER")

epc_mh <- epc_24q4 %>%
  select(mainheat_cols)
unique(epc_24q4$MAINHEAT_DESCRIPTION)
unique(epc_24q4$HOTWATER_DESCRIPTION)
epc_hw <- epc_24q4 %>%
  select(any_of(hotwater_cols))
# separate out?

# Split the entries by '|', unnest, and extract unique values
unique_hotwater_descriptions <- epc_24q4 %>%
  mutate(HOTWATER_DESCRIPTION = str_split(HOTWATER_DESCRIPTION, " \\| |, ")) %>%
  unnest(HOTWATER_DESCRIPTION) %>%
  distinct(HOTWATER_DESCRIPTION)
# separate by commas too?

# Split the entries by '|' and ',', unnest, and extract unique values
unique_mainheat_descriptions <- epc_mh %>%
  mutate(MAINHEAT_DESCRIPTION = str_split(MAINHEAT_DESCRIPTION, " \\| |, ")) %>%
  unnest(MAINHEAT_DESCRIPTION) %>%
  distinct(MAINHEAT_DESCRIPTION)

# solid fuel sources
sfb_heat <- c("coal", "wood logs", "smokeless fuel", "wood chips",
              "wood pellets", "dual fuel (mineral and wood)", "anthracite")
sfb_hw <- c("Solid fuel boiler/circulator",
                  "Solid fuel range cooker")

epc_w <- epc_24q4 %>%
  select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL) %>%
  filter(str_detect(HOTWATER_DESCRIPTION, paste(sfb_hw, collapse = "|")))

epc_sfb <- epc_24q4 %>%
  select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL) %>%
  filter(str_detect(MAINHEAT_DESCRIPTION, paste(sfb_heat, collapse = "|"))
         | str_detect(HOTWATER_DESCRIPTION, paste(sfb_hw, collapse = "|"))) 


# might need to have a think about removing commas - jsut use str_detect and select if any use that
# is solid fuel their main fuel?
# percentage of solid fuel as main fuel users vs not etc?

# load in all EPCs from past 10 years
# clean and check for further solid fuel types
# extract for matches like wood, show warning that new types are being added

# unique mainheat desc, main fuel, hotwater?
# wood, fuel, coal, anthracite

# Clean EPC data ----

# Use 2015 Q1 data for baseline mainheat descriptions
epc_15q1 <- read_csv(str_c(epc_fp, "2015Q1.csv"))

# Split the entries by '|' and ',', unnest, and extract unique values
unique_mainheat_descriptions <- epc_15q1 %>%
  rename_all(tolower) %>%
  mutate(mainheat_description = str_split(mainheat_description, " \\| ")) %>%
  unnest(mainheat_description) %>%
  distinct(mainheat_description)

epc_clean <- function(yr, q) {

  # file <- read_csv(str_c(epc_fp, "2024Q4.csv")) 
  print(str_c(yr, "Q", q, ".csv"))
  file <- read_csv(str_c(epc_fp, str_c(yr, "Q", q, ".csv")))
  
  clean_file <- file %>%
    rename_all(tolower) %>%
    select(osg_reference_number, address1, postcode, mainheat_description, 
           hotwater_description, number_open_fireplaces, main_fuel) %>%
    # Separate mainheat, hotwater columns where multiple heat sources are present 
    separate_rows(mainheat_description, sep = "\\|") %>%
    separate_rows(hotwater_description, sep = "\\|") %>%
    # this bit removed NAs, so
    filter(osg_reference_number != "OSG_UPRN"| is.na(osg_reference_number)) %>% 
    mutate(osg_reference_number = as.numeric(osg_reference_number))
  
  
  # TODO: add in bit for secondary fuel sources
    
  # Extract unique main heat descriptions from clean_file
  unique_clean_file_descriptions <- clean_file %>%
    distinct(mainheat_description) %>%
    pull(mainheat_description)
  
  # Check for new unique main heat descriptions
  new_descriptions <- setdiff(unique_clean_file_descriptions, 
                              unique_mainheat_descriptions$mainheat_description)
  
  if (length(new_descriptions) > 0) {
    message("New unique main heat descriptions not present in 2015 data: ", 
            paste(new_descriptions, collapse = "; "))
  }
    
  # Column for solid fuel
  # Select strings containing wood anthracite fuel coal
  sfb <- clean_file %>%
    mutate(solid_fuel_flag = if_else(
      str_detect(mainheat_description, "wood|anthracite|fuel|coal"), TRUE, 
      FALSE))
  
  
  # add columns for which Q yr it came from
  sfb <- sfb %>%
    mutate(data_year = yr) %>% 
    mutate(data_q = q)
}

# q <- c(1:4)
# yr <- c(2015:2024)

# Initialize an empty list to store results
all_cleaned_files <- list()

# Loop over years and quarters
for (year in 2015:2024) {
  for (quarter in 1:4) {
    cleaned_file <- epc_clean(year, quarter)
    all_cleaned_files[[str_c(year, "Q", quarter)]] <- cleaned_file
  }
}

# Combine all cleaned files into one data frame
final_cleaned_file <- bind_rows(all_cleaned_files)

# Save out file
# write_csv(final_cleaned_file, str_c(wd, "/EPC/output/epc_clean_data_2015_2024.csv"))
final_cleaned_file <- read.csv(str_c(wd, "/EPC/output/epc_clean_data_2015_2024.csv"))

# UPRN code match ----
# Read in UPRN code lookup

uprn_read <- read_csv(str_c(uprn_fp, "NSUL_FEB_2025_SC.csv"))
uprn_clean <- uprn_read %>%
  select(UPRN, GRIDGB1E, GRIDGB1N, PCDS) %>%
  rename_all(tolower)

epc_uprn <- final_cleaned_file %>%
  left_join(., uprn_clean, by = join_by(osg_reference_number == uprn),keep=T)

# Postcode match ----
# For rows where UPRN code is missing?
# Read in Postcode lookup shpaefile
v_pc <- vect(paste0(wd,"GeographicalUnits/pc_cut_25_1/PC_Cut_25_1.shp"))
# Centre point of postcode areas
v_pc_centr <- centroids(v_pc)
v_pc_centr <- cbind(v_pc_centr, geom(v_pc_centr)[, c("x", "y")]) %>% as.data.frame
rm(v_pc)
# might result in multiple houses ontop of eachother of mulitple
# buildings in the same postcode and all given the centroid coords
epc_uprn_pc <- epc_uprn %>%
  left_join(., v_pc_centr[,c('Postcode', 'x', 'y')],
            by = join_by(postcode == Postcode),keep=T)

## check for duplicates
# maybe look at addresses? 
epc_uprn_pc[duplicated(osg_reference_number) | duplicated(osg_reference_number, fromLast = TRUE)]
# for UPRN the above is fine, were uprn is not available, use addresses?
# what about buildings with no uprn number?
test <- epc_uprn_pc %>%  as.data.table %>% 
  .[duplicated(address1) | duplicated(address1, fromLast = TRUE)]

# Save out file
# write_csv(epc_uprn, str_c(wd, "/EPC/output/epc_matchedUPRN_clean_data_2015_2024.csv"))

# write_csv(epc_uprn_pc, str_c(wd, "/EPC/output/epc_matchedUPRNPC_clean_data_2015_2024.csv"))
epc_uprn_pc <- read.csv(str_c(wd, "/EPC/output/epc_matchedUPRNPC_clean_data_2015_2024.csv"))



require(data.table)
epc_uprn_pc <- epc_uprn_pc %>% as.data.table %>% 
  .[, c('x_used', 'y_used') := .(gridgb1e, gridgb1n)]
epc_uprn_pc[, c('x_used', 'y_used') := .(ifelse(is.na(gridgb1e), x ,gridgb1e),
                                         ifelse(is.na(gridgb1n), y ,gridgb1n))]
require(terra)
v <- vect(epc_uprn_pc, geom=c('x_used', 'y_used'))

v[v$solid_fuel_flag ==TRUE,]
writeVector(v[v$solid_fuel_flag ==TRUE,], paste0(wd,"/EPC/output/EPC_SF_2015_2024.shp"))



# epc_uprn_pc[is.na(gridgb1e),] # check

# remove duplicates - select more recent year
epc_uprn_pc <- epc_uprn_pc %>%
  filter(data_year = max(data_year),
         data_q = max(data_q))


# Data manipulation ----
pc_match <- epc_uprn %>%
  filter(postcode != pcds) %>%
  select(-(3:7))

# plot
shp <- st_as_sf(epc_uprn_pc, coords=c("gridgb1e","gridgb1n"))
st_crs(shp)<- 27700
shp <- st_transform(shp,4326)
write_sf(shp, str_c(wd, "/EPC/output/shp_epc_clean_data_2015_2024.shp"))

# Represent spatially
# raster or vector - raster?

epc_uprn_pc <- st_as_sf(epc_uprn_pc, coords = c("x_used", "y_used"), crs = 4326)

map <- epc_uprn_pc %>%
  ggplot() +
  # geom_sf(data = uk_border) +
  geom_sf(aes(color = solid_fuel_flag), size = 3) +
  labs(title = "Solid Fuel Flag in Scotland",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
