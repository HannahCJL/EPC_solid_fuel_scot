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
epc_mh <- epc_24q4 %>%
  select(mainheat_cols)
unique(epc_24q4$MAINHEAT_DESCRIPTION)
# separate out?

# Split the entries by '|', unnest, and extract unique values
unique_mainheat_descriptions <- epc_24q4 %>%
  mutate(MAINHEAT_DESCRIPTION = str_split(MAINHEAT_DESCRIPTION, " \\| ")) %>%
  unnest(MAINHEAT_DESCRIPTION) %>%
  distinct(MAINHEAT_DESCRIPTION)

epc_sfb <- epc_24q4 %>%
  select(OSG_REFERENCE_NUMBER, POSTCODE, MAINHEAT_DESCRIPTION, HOTWATER_DESCRIPTION, NUMBER_OPEN_FIREPLACES, MAIN_FUEL)
