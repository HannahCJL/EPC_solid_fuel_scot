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

epc_clean <- function(yr, q) {
  file <- read_csv(str_c(epc_fp, "2024Q4.csv")) 
  
  clean_file <- file %>%
    select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL) %>%
    # Separate mainheat, hotwaters columns where multiple heat sources are present - commas and |
    mutate(MAINHEAT_DESCRIPTION = str_replace_all(MAINHEAT_DESCRIPTION, "\\|", ",")) %>%
    separate_rows(MAINHEAT_DESCRIPTION, sep = ",\\s*") %>%
    mutate(HOTWATER_DESCRIPTION = str_replace_all(HOTWATER_DESCRIPTION, "\\|", ",")) %>%
    separate_rows(HOTWATER_DESCRIPTION, sep = ",\\s*")
  

  
}

q <- c(1:4)
yr <- c(2015:2024)
