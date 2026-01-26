library(dplyr)
library(openxlsx)
library(haven)
library(tidyr)
library(purrr)
library(stringr)


#Processing travel diary data

#Read data
Diary <- read_sav("/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality and travel dissonance/Data/2018/MPNWAVE6_DAGBOEKdata.sav")

#Select columns which are needed
Diary <- Diary[c(3,8,9,12,14,19,28,37,39,41,82,83)]

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

write_sav(Diary_purposerec, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/MPNWAVE6_Diary_cleaned.sav")


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

write.csv(Mode_use, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/MPNWAVE6_ModeUse.csv")



#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#Processing trip segments data characterised by mode use and travel purposes

Segmode <- Diary_purposerec[c(1, 8, 14, 13)]

#Commute
Segmode_commute <- Segmode %>%
  filter(Purpose_rec == 1)

Segmode_commute$Mode_rec <- as.numeric(Segmode_commute$Mode_rec)

Segmode_commute$Mode_rec <- recode(Segmode_commute$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                   `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                   `999` = NA_character_)

#Business
Segmode_business <- Segmode %>%
  filter(Purpose_rec == 2)

Segmode_business$Mode_rec <- as.numeric(Segmode_business$Mode_rec)

Segmode_business$Mode_rec <- recode(Segmode_business$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                   `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                   `999` = NA_character_)

#Education
Segmode_education <- Segmode %>%
  filter(Purpose_rec == 3)

Segmode_education$Mode_rec <- as.numeric(Segmode_education$Mode_rec)

Segmode_education$Mode_rec <- recode(Segmode_education$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Shopping
Segmode_shopping <- Segmode %>%
  filter(Purpose_rec == 4)

Segmode_shopping$Mode_rec <- as.numeric(Segmode_shopping$Mode_rec)

Segmode_shopping$Mode_rec <- recode(Segmode_shopping$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Caregiving
Segmode_caregiving <- Segmode %>%
  filter(Purpose_rec == 5)

Segmode_caregiving$Mode_rec <- as.numeric(Segmode_caregiving$Mode_rec)

Segmode_caregiving$Mode_rec <- recode(Segmode_caregiving$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Visitation
Segmode_visitation <- Segmode %>%
  filter(Purpose_rec == 6)

Segmode_visitation$Mode_rec <- as.numeric(Segmode_visitation$Mode_rec)

Segmode_visitation$Mode_rec <- recode(Segmode_visitation$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Leisure
Segmode_leisure <- Segmode %>%
  filter(Purpose_rec %in% c(7, 9))

Segmode_leisure$Mode_rec <- as.numeric(Segmode_leisure$Mode_rec)

Segmode_leisure$Mode_rec <- recode(Segmode_leisure$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Sports
Segmode_sports <- Segmode %>%
  filter(Purpose_rec == 8)

Segmode_sports$Mode_rec <- as.numeric(Segmode_sports$Mode_rec)

Segmode_sports$Mode_rec <- recode(Segmode_sports$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                      `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                      `999` = NA_character_)




#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#Processing personal preference data

#Read data
Preference <- read_sav("/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/MPNWAVE6_Pdata_en.sav")

#-----------------------------------------------Commute------------------------------------------------------------------------------------

#Select columns which are needed
Preference_commute <- Preference[c(6, 68:78)]

Preference_commute <- Preference_commute %>%
  mutate(Preference_commute_3 = if_else(Preference_commute[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_commute_4 = if_else(Preference_commute[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_commute_5 = if_else(Preference_commute[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_commute_6 = if_else(Preference_commute[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_commute_7 = if_else(Preference_commute[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_commute_8 = if_else(Preference_commute[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_commute_9 = if_else(Preference_commute[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_commute_10 = if_else(Preference_commute[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_commute_11 = if_else(Preference_commute[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_commute_12 = if_else(Preference_commute[12] == 1, "Other" , NA_character_))

Preference_commute <- Preference_commute[c(1,13:22)]

# Merge selected columns into a list
Preference_commute <- Preference_commute %>%
  mutate(Commute = pmap(list(Preference_commute_3, Preference_commute_4, Preference_commute_5,
                             Preference_commute_6, Preference_commute_7, Preference_commute_8,
                             Preference_commute_9, Preference_commute_10, Preference_commute_11,
                             Preference_commute_12), ~ na.omit(c(...))))  # Remove NAs

Preference_commute <- Preference_commute[c(1,12)]

#-----------------------------------------------Business------------------------------------------------------------------------------------

#Select columns which are needed
Preference_business <- Preference[c(6, 79:89)]

Preference_business <- Preference_business %>%
  mutate(Preference_business_3 = if_else(Preference_business[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_business_4 = if_else(Preference_business[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_business_5 = if_else(Preference_business[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_business_6 = if_else(Preference_business[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_business_7 = if_else(Preference_business[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_business_8 = if_else(Preference_business[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_business_9 = if_else(Preference_business[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_business_10 = if_else(Preference_business[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_business_11 = if_else(Preference_business[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_business_12 = if_else(Preference_business[12] == 1, "Other" , NA_character_))

Preference_business <- Preference_business[c(1,13:22)]

# Merge selected columns into a list
Preference_business <- Preference_business %>%
  mutate(Business = pmap(list(Preference_business_3, Preference_business_4, Preference_business_5,
                             Preference_business_6, Preference_business_7, Preference_business_8,
                             Preference_business_9, Preference_business_10, Preference_business_11,
                             Preference_business_12), ~ na.omit(c(...))))  # Remove NAs

Preference_business <- Preference_business[c(1,12)]


#-----------------------------------------------Education------------------------------------------------------------------------------------

#Select columns which are needed
Preference_education <- Preference[c(6, 90:100)]

Preference_education <- Preference_education %>%
  mutate(Preference_education_3 = if_else(Preference_education[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_education_4 = if_else(Preference_education[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_education_5 = if_else(Preference_education[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_education_6 = if_else(Preference_education[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_education_7 = if_else(Preference_education[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_education_8 = if_else(Preference_education[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_education_9 = if_else(Preference_education[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_education_10 = if_else(Preference_education[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_education_11 = if_else(Preference_education[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_education_12 = if_else(Preference_education[12] == 1, "Other" , NA_character_))

Preference_education <- Preference_education[c(1,13:22)]

# Merge selected columns into a list
Preference_education <- Preference_education %>%
  mutate(Education = pmap(list(Preference_education_3, Preference_education_4, Preference_education_5,
                              Preference_education_6, Preference_education_7, Preference_education_8,
                              Preference_education_9, Preference_education_10, Preference_education_11,
                              Preference_education_12), ~ na.omit(c(...))))  # Remove NAs

Preference_education <- Preference_education[c(1,12)]


#-----------------------------------------------Shopping------------------------------------------------------------------------------------

#Select columns which are needed
Preference_grocery <- Preference[c(6, 101:111)]

Preference_grocery <- Preference_grocery %>%
  mutate(Preference_grocery_3 = if_else(Preference_grocery[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_grocery_4 = if_else(Preference_grocery[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_grocery_5 = if_else(Preference_grocery[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_grocery_6 = if_else(Preference_grocery[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_grocery_7 = if_else(Preference_grocery[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_grocery_8 = if_else(Preference_grocery[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_grocery_9 = if_else(Preference_grocery[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_grocery_10 = if_else(Preference_grocery[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_grocery_11 = if_else(Preference_grocery[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_grocery_12 = if_else(Preference_grocery[12] == 1, "Other" , NA_character_))

Preference_grocery <- Preference_grocery[c(1,13:22)]

#Select columns which are needed
Preference_shopping <- Preference[c(6, 112:122)]

Preference_shopping <- Preference_shopping %>%
  mutate(Preference_shopping_3 = if_else(Preference_shopping[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_shopping_4 = if_else(Preference_shopping[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_shopping_5 = if_else(Preference_shopping[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_shopping_6 = if_else(Preference_shopping[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_shopping_7 = if_else(Preference_shopping[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_shopping_8 = if_else(Preference_shopping[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_shopping_9 = if_else(Preference_shopping[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_shopping_10 = if_else(Preference_shopping[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_shopping_11 = if_else(Preference_shopping[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_shopping_12 = if_else(Preference_shopping[12] == 1, "Other" , NA_character_))

Preference_shopping <- Preference_shopping[c(1,13:22)]


Preference_shopping <- merge(Preference_grocery, Preference_shopping, by = "PERSID")


# Merge selected columns into a list
Preference_shopping <- Preference_shopping %>%
  mutate(Shopping = pmap(list(Preference_grocery_3, Preference_grocery_4, Preference_grocery_5,
                               Preference_grocery_6, Preference_grocery_7, Preference_grocery_8,
                               Preference_grocery_9, Preference_grocery_10, Preference_grocery_11,
                               Preference_grocery_12, Preference_shopping_3, Preference_shopping_4, 
                               Preference_shopping_5, Preference_shopping_6, Preference_shopping_7, 
                               Preference_shopping_8, Preference_shopping_9, Preference_shopping_10, 
                               Preference_shopping_11, Preference_shopping_12), ~ na.omit(c(...))))  # Remove NAs

Preference_shopping <- Preference_shopping[c(1,22)]

Preference_shopping$Shopping <- lapply(Preference_shopping$Shopping, unique)


#-----------------------------------------------Visitation------------------------------------------------------------------------------------

#Select columns which are needed
Preference_visitation <- Preference[c(6, 134:144)]

Preference_visitation <- Preference_visitation %>%
  mutate(Preference_visitation_3 = if_else(Preference_visitation[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_visitation_4 = if_else(Preference_visitation[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_visitation_5 = if_else(Preference_visitation[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_visitation_6 = if_else(Preference_visitation[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_visitation_7 = if_else(Preference_visitation[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_visitation_8 = if_else(Preference_visitation[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_visitation_9 = if_else(Preference_visitation[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_visitation_10 = if_else(Preference_visitation[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_visitation_11 = if_else(Preference_visitation[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_visitation_12 = if_else(Preference_visitation[12] == 1, "Other" , NA_character_))

Preference_visitation <- Preference_visitation[c(1,13:22)]

# Merge selected columns into a list
Preference_visitation <- Preference_visitation %>%
  mutate(Visitation = pmap(list(Preference_visitation_3, Preference_visitation_4, Preference_visitation_5,
                               Preference_visitation_6, Preference_visitation_7, Preference_visitation_8,
                               Preference_visitation_9, Preference_visitation_10, Preference_visitation_11,
                               Preference_visitation_12), ~ na.omit(c(...))))  # Remove NAs

Preference_visitation <- Preference_visitation[c(1,12)]


#-----------------------------------------------Daytrip------------------------------------------------------------------------------------

#Select columns which are needed
Preference_daytrip <- Preference[c(6, 145:155)]

Preference_daytrip <- Preference_daytrip %>%
  mutate(Preference_daytrip_3 = if_else(Preference_daytrip[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_daytrip_4 = if_else(Preference_daytrip[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_daytrip_5 = if_else(Preference_daytrip[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_daytrip_6 = if_else(Preference_daytrip[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_daytrip_7 = if_else(Preference_daytrip[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_daytrip_8 = if_else(Preference_daytrip[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_daytrip_9 = if_else(Preference_daytrip[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_daytrip_10 = if_else(Preference_daytrip[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_daytrip_11 = if_else(Preference_daytrip[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_daytrip_12 = if_else(Preference_daytrip[12] == 1, "Other" , NA_character_))

Preference_daytrip <- Preference_daytrip[c(1,13:22)]

# Merge selected columns into a list
Preference_daytrip <- Preference_daytrip %>%
  mutate(Daytrip = pmap(list(Preference_daytrip_3, Preference_daytrip_4, Preference_daytrip_5,
                                Preference_daytrip_6, Preference_daytrip_7, Preference_daytrip_8,
                                Preference_daytrip_9, Preference_daytrip_10, Preference_daytrip_11,
                                Preference_daytrip_12), ~ na.omit(c(...))))  # Remove NAs

Preference_daytrip <- Preference_daytrip[c(1,12)]


#-----------------------------------------------Sports------------------------------------------------------------------------------------

#Select columns which are needed
Preference_sports <- Preference[c(6, 156:166)]

Preference_sports <- Preference_sports %>%
  mutate(Preference_sports_3 = if_else(Preference_sports[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_sports_4 = if_else(Preference_sports[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_sports_5 = if_else(Preference_sports[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_sports_6 = if_else(Preference_sports[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_sports_7 = if_else(Preference_sports[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_sports_8 = if_else(Preference_sports[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_sports_9 = if_else(Preference_sports[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_sports_10 = if_else(Preference_sports[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_sports_11 = if_else(Preference_sports[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_sports_12 = if_else(Preference_sports[12] == 1, "Other" , NA_character_))

Preference_sports <- Preference_sports[c(1,13:22)]

# Merge selected columns into a list
Preference_sports <- Preference_sports %>%
  mutate(Sports = pmap(list(Preference_sports_3, Preference_sports_4, Preference_sports_5,
                             Preference_sports_6, Preference_sports_7, Preference_sports_8,
                             Preference_sports_9, Preference_sports_10, Preference_sports_11,
                             Preference_sports_12), ~ na.omit(c(...))))  # Remove NAs

Preference_sports <- Preference_sports[c(1,12)]


#-----------------------------------------------Caregiving------------------------------------------------------------------------------------

#Select columns which are needed
Preference_caregiving <- Preference[c(6, 167:177)]

Preference_caregiving <- Preference_caregiving %>%
  mutate(Preference_caregiving_3 = if_else(Preference_caregiving[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_caregiving_4 = if_else(Preference_caregiving[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_caregiving_5 = if_else(Preference_caregiving[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_caregiving_6 = if_else(Preference_caregiving[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_caregiving_7 = if_else(Preference_caregiving[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_caregiving_8 = if_else(Preference_caregiving[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_caregiving_9 = if_else(Preference_caregiving[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_caregiving_10 = if_else(Preference_caregiving[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_caregiving_11 = if_else(Preference_caregiving[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_caregiving_12 = if_else(Preference_caregiving[12] == 1, "Other" , NA_character_))

Preference_caregiving <- Preference_caregiving[c(1,13:22)]

# Merge selected columns into a list
Preference_caregiving <- Preference_caregiving %>%
  mutate(Caregiving = pmap(list(Preference_caregiving_3, Preference_caregiving_4, Preference_caregiving_5,
                            Preference_caregiving_6, Preference_caregiving_7, Preference_caregiving_8,
                            Preference_caregiving_9, Preference_caregiving_10, Preference_caregiving_11,
                            Preference_caregiving_12), ~ na.omit(c(...))))  # Remove NAs

Preference_caregiving <- Preference_caregiving[c(1,12)]


#-----------------------------------------------Leisure------------------------------------------------------------------------------------

#Select columns which are needed
Preference_leisure <- Preference[c(6, 178:188)]

Preference_leisure <- Preference_leisure %>%
  mutate(Preference_leisure_3 = if_else(Preference_leisure[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_leisure_4 = if_else(Preference_leisure[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_leisure_5 = if_else(Preference_leisure[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_leisure_6 = if_else(Preference_leisure[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_leisure_7 = if_else(Preference_leisure[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_leisure_8 = if_else(Preference_leisure[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_leisure_9 = if_else(Preference_leisure[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_leisure_10 = if_else(Preference_leisure[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_leisure_11 = if_else(Preference_leisure[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_leisure_12 = if_else(Preference_leisure[12] == 1, "Other" , NA_character_))

Preference_leisure <- Preference_leisure[c(1,13:22)]

# Merge selected columns into a list
Preference_leisure <- Preference_leisure %>%
  mutate(Leisure = pmap(list(Preference_leisure_3, Preference_leisure_4, Preference_leisure_5,
                                Preference_leisure_6, Preference_leisure_7, Preference_leisure_8,
                                Preference_leisure_9, Preference_leisure_10, Preference_leisure_11,
                                Preference_leisure_12), ~ na.omit(c(...))))  # Remove NAs

Preference_leisure <- Preference_leisure[c(1,12)]


#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#Macthing used modes and preferred modes

#-----------------------------------------Commute-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_commute <- Segmode_commute%>%
  left_join(Preference_commute, by = "PERSID")

as.vector(Merged_commute$Commute)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_commute$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_commute$Mode_rec, Merged_commute$Commute)


# Step 3: Summarize the data
Result_commute <- Merged_commute %>%
  group_by(PERSID) %>%
  summarize(
    Total_Commute = n(),
    Consonant_Commute = sum(Consonant)
  )

#-----------------------------------------Business-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_business <- Segmode_business%>%
  left_join(Preference_business, by = "PERSID")

as.vector(Merged_business$Business)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_business$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_business$Mode_rec, Merged_business$Business)


# Step 3: Summarize the data
Result_business <- Merged_business %>%
  group_by(PERSID) %>%
  summarize(
    Total_Business = n(),
    Consonant_Business = sum(Consonant)
  )



#-----------------------------------------Education-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_education <- Segmode_education%>%
  left_join(Preference_education, by = "PERSID")

as.vector(Merged_education$Education)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_education$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_education$Mode_rec, Merged_education$Education)


# Step 3: Summarize the data
Result_education <- Merged_education %>%
  group_by(PERSID) %>%
  summarize(
    Total_Education = n(),
    Consonant_Education = sum(Consonant)
  )


#-----------------------------------------Shopping-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_shopping <- Segmode_shopping%>%
  left_join(Preference_shopping, by = "PERSID")

as.vector(Merged_shopping$Shopping)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_shopping$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_shopping$Mode_rec, Merged_shopping$Shopping)


# Step 3: Summarize the data
Result_shopping <- Merged_shopping %>%
  group_by(PERSID) %>%
  summarize(
    Total_Shopping = n(),
    Consonant_Shopping = sum(Consonant)
  )


#-----------------------------------------Visitation-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_visitation <- Segmode_visitation%>%
  left_join(Preference_visitation, by = "PERSID")

as.vector(Merged_visitation$Visitation)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_visitation$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_visitation$Mode_rec, Merged_visitation$Visitation)


# Step 3: Summarize the data
Result_visitation <- Merged_visitation %>%
  group_by(PERSID) %>%
  summarize(
    Total_Visitation = n(),
    Consonant_Visitation = sum(Consonant)
  )


#-----------------------------------------Sports-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_sports <- Segmode_sports%>%
  left_join(Preference_sports, by = "PERSID")

as.vector(Merged_sports$Sports)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_sports$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_sports$Mode_rec, Merged_sports$Sports)


# Step 3: Summarize the data
Result_sports <- Merged_sports %>%
  group_by(PERSID) %>%
  summarize(
    Total_Sports = n(),
    Consonant_Sports = sum(Consonant)
  )



#-----------------------------------------Leisure-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_leisure <- Segmode_leisure%>%
  left_join(Preference_leisure, by = "PERSID")

as.vector(Merged_leisure$Leisure)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_leisure$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_leisure$Mode_rec, Merged_leisure$Leisure)


# Step 3: Summarize the data
Result_leisure <- Merged_leisure %>%
  group_by(PERSID) %>%
  summarize(
    Total_Leisure = n(),
    Consonant_Leisure = sum(Consonant)
  )

#-----------------------------------------Caregiving-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_caregiving <- Segmode_caregiving%>%
  left_join(Preference_caregiving, by = "PERSID")

as.vector(Merged_caregiving$Caregiving)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_caregiving$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_caregiving$Mode_rec, Merged_caregiving$Caregiving)


# Step 3: Summarize the data
Result_caregiving <- Merged_caregiving %>%
  group_by(PERSID) %>%
  summarize(
    Total_Caregiving = n(),
    Consonant_Caregiving = sum(Consonant)
  )


#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Calculate dissonance levels for each type of travel

#Mandatory (Commute, Business, Education)
Mandatory <- full_join(Result_commute, Result_business, by = "PERSID")
Mandatory <- full_join(Mandatory, Result_education, by = "PERSID")

Mandatory[is.na(Mandatory)] <- 0

Mandatory$Total_mandatory <- Mandatory$Total_Commute + Mandatory$Total_Business + Mandatory$Total_Education
Mandatory$Cosonant_mandatory <- Mandatory$Consonant_Commute + Mandatory$Consonant_Business + Mandatory$Consonant_Education
Mandatory$Dissonance_mandatory <- Mandatory$Total_mandatory - Mandatory$Cosonant_mandatory


#Maintenance (Shopping, Caregiving)

Maintenance <- full_join(Result_shopping, Result_caregiving, by = "PERSID")

Maintenance[is.na(Maintenance)] <- 0

Maintenance$Total_maintenance <- Maintenance$Total_Shopping + Maintenance$Total_Caregiving
Maintenance$Cosonant_maintenance <- Maintenance$Consonant_Shopping + Maintenance$Consonant_Caregiving
Maintenance$Dissonance_maintenance <- Maintenance$Total_maintenance - Maintenance$Cosonant_maintenance


#Discretionary (Visitation, Leisure, Sports)
Discretionary <- full_join(Result_visitation, Result_leisure, by = "PERSID")
Discretionary <- full_join(Discretionary, Result_sports, by = "PERSID")

Discretionary[is.na(Discretionary)] <- 0

Discretionary$Total_discretionary <- Discretionary$Total_Visitation + Discretionary$Total_Leisure + Discretionary$Total_Sports
Discretionary$Cosonant_discretionary <- Discretionary$Consonant_Visitation + Discretionary$Consonant_Leisure + Discretionary$Consonant_Sports
Discretionary$Dissonance_discretionary <- Discretionary$Total_discretionary - Discretionary$Cosonant_discretionary


Dissonance <- full_join(Mandatory, Maintenance, by = "PERSID")
Dissonance <- full_join(Dissonance, Discretionary, by = "PERSID")

Dissonance[is.na(Dissonance)] <- 0


Dissonance$Total_seg <- Dissonance$Total_mandatory + Dissonance$Total_maintenance + Dissonance$Total_discretionary
Dissonance$Total_dissonance <- Dissonance$Dissonance_mandatory + Dissonance$Dissonance_maintenance + Dissonance$Dissonance_discretionary

Dissonance$Dlevel_mandatory <- Dissonance$Dissonance_mandatory/Dissonance$Total_mandatory
Dissonance$Dlevel_maintenance <- Dissonance$Dissonance_maintenance/Dissonance$Total_maintenance
Dissonance$Dlevel_discretionary <- Dissonance$Dissonance_discretionary/Dissonance$Total_discretionary
Dissonance$Dlevel_total <- Dissonance$Total_dissonance/Dissonance$Total_seg

write.csv(Dissonance, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Dissonance.csv")




#---------------------------Calculate dissonance level after merging type of travel----------------------------------------------------------

#----------------------------------------Reclassification mode use----------------------------------------------------------

Segmode_mandatory <- bind_rows(Segmode_commute, Segmode_business, Segmode_education)
Segmode_maintenance <- bind_rows(Segmode_caregiving, Segmode_shopping)
Segmode_discretionary <- bind_rows(Segmode_visitation, Segmode_sports, Segmode_leisure)


#----------------------------------------Reclassification preference----------------------------------------------------------

Preference_mandatory <- full_join(Preference_commute, Preference_business, by = "PERSID")
Preference_mandatory <- full_join(Preference_mandatory, Preference_education, by = "PERSID")
Preference_mandatory <- Preference_mandatory %>%
  mutate(Mandatory = pmap(list(Commute, Business, Education), ~ unique(c(..1, ..2, ..3))))

Preference_maintenance <- full_join(Preference_caregiving, Preference_shopping, by = "PERSID")
Preference_maintenance <- Preference_maintenance %>%
  mutate(Maintenance = map2(Caregiving, Shopping, ~ unique(c(.x,.y))))


Preference_discretionary <- full_join(Preference_visitation, Preference_sports, by = "PERSID")
Preference_discretionary <- full_join(Preference_discretionary, Preference_leisure, by = "PERSID")
Preference_discretionary <- Preference_discretionary %>%
  mutate(Discretionary = pmap(list(Visitation, Sports, Leisure), ~ unique(c(..1, ..2, ..3))))


#----------------------------------------Mandatory dissonance calculation----------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_mandatory <- Segmode_mandatory%>%
  left_join(Preference_mandatory, by = "PERSID")

as.vector(Merged_mandatory$Mandatory)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_mandatory$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_mandatory$Mode_rec, Merged_mandatory$Mandatory)


# Step 3: Summarize the data
Result_mandatory <- Merged_mandatory %>%
  group_by(PERSID) %>%
  summarize(
    Total_Mandatory = n(),
    Consonant_Mandatory= sum(Consonant)
  )


#----------------------------------------Maintenance dissonance calculation----------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_maintenance <- Segmode_maintenance%>%
  left_join(Preference_maintenance, by = "PERSID")

as.vector(Merged_maintenance$Maintenance)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_maintenance$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_maintenance$Mode_rec, Merged_maintenance$Maintenance)


# Step 3: Summarize the data
Result_maintenance <- Merged_maintenance %>%
  group_by(PERSID) %>%
  summarize(
    Total_Maintenance = n(),
    Consonant_Maintenance = sum(Consonant)
  )

#----------------------------------------Discretionary dissonance calculation----------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_discretionary <- Segmode_discretionary%>%
  left_join(Preference_discretionary, by = "PERSID")

as.vector(Merged_discretionary$Discretionary)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_discretionary$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_discretionary$Mode_rec, Merged_discretionary$Discretionary)


# Step 3: Summarize the data
Result_discretionary <- Merged_discretionary %>%
  group_by(PERSID) %>%
  summarize(
    Total_Discretionary = n(),
    Consonant_Discretionary = sum(Consonant)
  )




#---------------------------------------Dissonance patterns used modes---------------------------------------------

Dissonance_pattern_commute <- Merged_commute %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_business <- Merged_business %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_education <- Merged_education %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_caregiving <- Merged_caregiving %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_shopping <- Merged_shopping %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_visitation <- Merged_visitation %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_sports <- Merged_sports%>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_leisure <- Merged_leisure %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

#---------------------------------------Dissonance patterns used modes---------------------------------------------

Dissonance_pattern_commute <- Merged_commute %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_business <- Merged_business %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_education <- Merged_education %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_caregiving <- Merged_caregiving %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_shopping <- Merged_shopping %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_visitation <- Merged_visitation %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_sports <- Merged_sports%>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))

Dissonance_pattern_leisure <- Merged_leisure %>%
  group_by(Mode_rec) %>% 
  summarise(Mode_fre = n(), Consonance = sum(Consonant))


write.csv(Result_mandatory, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Dissonance_mandatory_merged.csv")
write.csv(Result_maintenance, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Dissonance_maintenance_merged.csv")
write.csv(Result_discretionary, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Dissonance_discretionary_merged.csv")


#-----------------------------------------------------preference summary-----------------------------------------------------

# Reframe the dataset
pre_mandatory <- Preference_mandatory %>%
  mutate(
    Car = if_else(str_detect(Mandatory, "Car"), 1, 0),
    Train = if_else(str_detect(Mandatory, "Train"), 1, 0),
    BTM = if_else(str_detect(Mandatory, "BTM"), 1, 0),
    Bike = if_else(str_detect(Mandatory, "Bike"), 1, 0),
    Walk = if_else(str_detect(Mandatory, "Walk"), 1, 0)
  )


# Reframe the dataset
pre_maintenance <- Preference_maintenance %>%
  mutate(
    Car = if_else(str_detect(Maintenance, "Car"), 1, 0),
    Train = if_else(str_detect(Maintenance, "Train"), 1, 0),
    BTM = if_else(str_detect(Maintenance, "BTM"), 1, 0),
    Bike = if_else(str_detect(Maintenance, "Bike"), 1, 0),
    Walk = if_else(str_detect(Maintenance, "Walk"), 1, 0)
  )


# Reframe the dataset
pre_discretionary <- Preference_discretionary %>%
  mutate(
    Car = if_else(str_detect(Discretionary, "Car"), 1, 0),
    Train = if_else(str_detect(Discretionary, "Train"), 1, 0),
    BTM = if_else(str_detect(Discretionary, "BTM"), 1, 0),
    Bike = if_else(str_detect(Discretionary, "Bike"), 1, 0),
    Walk = if_else(str_detect(Discretionary, "Walk"), 1, 0)
  )

pre_mandatory <- pre_mandatory[c(1, 6:10)]
pre_maintenance <- pre_maintenance[c(1, 5:9)]
pre_discretionary <- pre_discretionary[c(1, 6:10)]

write.csv(pre_mandatory, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Preference_mandatory.csv")
write.csv(pre_maintenance, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Preference_maintenance.csv")
write.csv(pre_discretionary, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality by captive and cognitive dissonance/Data/2018/Preference_discretionary.csv")





