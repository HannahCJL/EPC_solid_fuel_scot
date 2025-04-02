# Scotland solid fuel burning
# EPC

# 1/4/25

# File paths

uprn_fp <- "P:/08092_ScotGov_AirQuality/Data/D1_2_PM_WorkDayTask1/DomesticBurningStats/UPRN Data/Data/"
epc_fp <- "P:/08092_ScotGov_AirQuality/Data/D1_2_PM_WorkDayTask1/DomesticBurningStats/D_EPC_data_2015Q1-2024Q4/"
wd <- "P:/08092_ScotGov_AirQuality/Data/D1_2_PM_WorkDayTask1/ImprovingSpatialDomesticSF/Data"

pacman::p_load("dplyr","stringr","ggplot2","readr","DT","tidyr")

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
sfb_heat <- c("coal", "wood logs", "smokeless fuel", "wood chips",  "wood pellets", "dual fuel (mineral and wood)", "anthracite")
sfb_hw <- c("Solid fuel boiler/circulator",
                  "Solid fuel range cooker")

  
epc_w <- epc_24q4 %>%
  select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL) %>%
  filter(str_detect(HOTWATER_DESCRIPTION, paste(sfb_w, collapse = "|")))

epc_sfb <- epc_24q4 %>%
  select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL) %>%
  filter(str_detect(MAINHEAT_DESCRIPTION, paste(sfb_heat, collapse = "|"))
         | str_detect(HOTWATER_DESCRIPTION, paste(sfb_w, collapse = "|"))) 


# might need to have a think about removing commas - jsut use str_detect and select if any use that
# is solid fuel their main fuel?
# percentage of solid fuel as main fuel users vs not etc?

# check if need to load all epc's

# load in all EPCs from past 10 years
# clean and check for further solid fuel types
# extract for matches like wood, show warning that new types are being added

# unique mainheat desc, main fuel, hotwater?
# wood, fuel, coal, anthracite
# mutate column instead of filter

# cant just remove all commas for hotwater ----
# need to check documentation for data

epc_24q4 <- read_csv(str_c(epc_fp, "2024Q3.csv"))

epc_15q1 <- read_csv(str_c(epc_fp, "2015Q1.csv"))
unique(epc_15q1$MAINHEAT_DESCRIPTION)
unique(epc_15q1$HOTWATER_DESCRIPTION)

# Split the entries by '|' and ',', unnest, and extract unique values
unique_mainheat_descriptions <- epc_15q1 %>%
  mutate(MAINHEAT_DESCRIPTION = str_split(MAINHEAT_DESCRIPTION, " \\| ")) %>%
  # mutate(MAINHEAT_DESCRIPTION = str_split(MAINHEAT_DESCRIPTION, " \\| |, ")) %>% # commas and |
  unnest(MAINHEAT_DESCRIPTION) %>%
  distinct(MAINHEAT_DESCRIPTION)


# select wood anthracite fuel coal

epc_clean <- function(yr, q) {
  # file <- read_csv(str_c(epc_fp, "2024Q4.csv")) 
  file <- read_csv(str_c(epc_fp, str_c(yr, "Q", q, ".csv")))
  
  clean_file <- file %>%
    rename_all(tolower) %>%
    select(osg_reference_number, postcode, mainheat_description, hotwater_description, number_open_fireplaces, main_fuel) %>%
    # Separate mainheat, hotwater columns where multiple heat sources are present 
    separate_rows(mainheat_description, sep = "\\|") %>%
    separate_rows(hotwater_description, sep = "\\|") 
    # Check for new unique main heat descriptions
  
  # Extract unique main heat descriptions from clean_file
  unique_clean_file_descriptions <- clean_file %>%
    distinct(mainheat_description) %>%
    pull(mainheat_description)
  
  # Check for new unique main heat descriptions
  new_descriptions <- setdiff(unique_clean_file_descriptions, unique_mainheat_descriptions$mainheat_description)
  
  if (length(new_descriptions) > 0) {
    message("New unique main heat descriptions not present in 2015 data: ", paste(new_descriptions, collapse = "; "))
  }
    
  # Column for solid fuel
  # select wood anthracite fuel coal
  sfb <- clean_file %>%
    mutate(solid_fuel_flag = if_else(str_detect(mainheat_description, "wood|anthracite|fuel|coal"), TRUE, FALSE))
}

q <- c(1:4)
yr <- c(2015:2024)
