library(dplyr)
library(openxlsx)
library(haven)
library(tidyr)

#Read data
Diary <- read_sav("/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality and travel dissonance/Data/2022/MPNWAVE10_DAGBOEKdata_en.sav")

#Select columns which are needed
Diary <- Diary[c(3,8,9,12,14,19,28,38,40,42,83,84)]

#------------------------------------------Screening data----------------------------------------------------------

#Remove data rows where the individual did not travel on that day
Diary <- Diary[Diary$WEGGEWEEST == 1, ]
#Remove data rows where the trip day is before the three days of formal survey
Diary <- Diary[Diary$VPLDAGNR > 0, ]
#Remove data rows where transport is as a profession in the trip
Diary <- Diary[Diary$MOTIEF != 3, ]

# Identify and remove rows where "Walk" is adjacent to "Train"
Diary_cleaned <- Diary %>%
  group_by(VPLID) %>%
  arrange(RITNR) %>%
  mutate(
    next_mode = lead(KRVM),  # Get next mode
    prev_mode = lag(KRVM)   # Get previous mode
  ) %>%    
  filter(!(KRVM == 7) |
           (KRVM == 7 & (next_mode == 3 | prev_mode == 3)) |
           (KRVM == 7 & is.na(next_mode) & is.na(prev_mode))) %>%  # Filter out unwanted rows
  select(-next_mode, -prev_mode)  # Drop the helper columns

#----------------------------------------Reclassification----------------------------------------------------------

# Reclassify trip segment travel mode
Diary_moderec <- Diary_cleaned %>%
  mutate(
    Mode_rec = case_when(
      KRVM %in% c(8, 98, 99) ~ 999,      
      TRUE ~  KRVM                              
    )
  )

# Reclassify travel purposes
Diary_purposerec <- Diary_moderec %>%
  mutate(
    Purpose_rec = case_when(
      MOTIEF %in% c(1) ~ 1,
      MOTIEF %in% c(2) ~ 2,
      MOTIEF %in% c(6) ~ 3,
      MOTIEF %in% c(7) ~ 4,
      MOTIEF %in% c(4, 5, 12) ~ 5,
      MOTIEF %in% c(8) ~ 6,
      MOTIEF %in% c(9) ~ 7,
      MOTIEF %in% c(10) ~ 8,
      MOTIEF %in% c(11) ~ 9,
      TRUE ~ 999
    )
  )

write_sav(Diary_purposerec, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2022/MPNWAVE10_Diary_cleaned.sav")


#----------------------------------------Summarise mode use by trip segments----------------------------------------------------------

# Create new columns of using each transport mode (whether used for the segment or not)

Diary_summary <- Diary_purposerec %>%
  mutate(CarDriver = if_else(Mode_rec == 1, 1 ,0), missing = NULL) %>%
  mutate(CarPassenger = if_else(Mode_rec == 2, 1 ,0), missing = NULL) %>%
  mutate(Train = if_else(Mode_rec == 3, 1 ,0), missing = NULL) %>%
  mutate(BTM = if_else(Mode_rec == 4, 1 ,0), missing = NULL) %>%
  mutate(ScoMop = if_else(Mode_rec == 5, 1 ,0), missing = NULL) %>%
  mutate(Bike = if_else(Mode_rec == 6, 1 ,0), missing = NULL) %>%
  mutate(Walk = if_else(Mode_rec == 7, 1 ,0), missing = NULL) %>%
  mutate(Other = if_else(Mode_rec == 999, 1 ,0), missing = NULL)

# Summarise mode use frequency by trip segments at individual level
Mode_use <- Diary_summary %>%
  group_by(PERSID) %>%
  summarise_at(vars(CarDriver, CarPassenger, Train, BTM, 
                    ScoMop, Bike, Walk, Other), sum, na.rm = TRUE)

write.csv(Mode_use, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2022/MPNWAVE10_ModeUse.csv")





