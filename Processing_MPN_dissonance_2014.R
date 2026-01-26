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
      MOTIEF %in% c(4) ~ 4,
      MOTIEF %in% c(5) ~ 5,
      MOTIEF %in% c(6) ~ 6,
      MOTIEF %in% c(7) ~ 7,
      MOTIEF %in% c(8) ~ 8,
      MOTIEF %in% c(9) ~ 9,
      MOTIEF %in% c(10) ~ 10,
      MOTIEF %in% c(11) ~ 11,
      MOTIEF %in% c(12) ~ 12,
      MOTIEF %in% c(13) ~ 13,
      TRUE ~ 999
    )
  )

#---------------------------------------------------------------------------------------------------------------------

#Processing trip segments data characterised by mode use and travel purposes

Segmode <- Diary_purposerec[c(1, 8, 14, 13)]

#To and from work
Segmode_1 <- Segmode %>%
  filter(Purpose_rec == 1)

Segmode_1$Mode_rec <- as.numeric(Segmode_1$Mode_rec)

Segmode_1$Mode_rec <- recode(Segmode_1$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                   `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                   `999` = NA_character_)

#Business-related visit in travel context
Segmode_2 <- Segmode %>%
  filter(Purpose_rec == 2)

Segmode_2$Mode_rec <- as.numeric(Segmode_2$Mode_rec)

Segmode_2$Mode_rec <- recode(Segmode_2$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                   `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                   `999` = NA_character_)

#Dropping off, picking up people
Segmode_4 <- Segmode %>%
  filter(Purpose_rec == 4)

Segmode_4$Mode_rec <- as.numeric(Segmode_4$Mode_rec)

Segmode_4$Mode_rec <- recode(Segmode_4$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Delivering, picking up goods
Segmode_5 <- Segmode %>%
  filter(Purpose_rec == 5)

Segmode_5$Mode_rec <- as.numeric(Segmode_5$Mode_rec)

Segmode_5$Mode_rec <- recode(Segmode_5$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Following education study courses
Segmode_6 <- Segmode %>%
  filter(Purpose_rec == 6)

Segmode_6$Mode_rec <- as.numeric(Segmode_6$Mode_rec)

Segmode_6$Mode_rec <- recode(Segmode_6$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Shopping, doing grocery shopping
Segmode_7 <- Segmode %>%
  filter(Purpose_rec == 7)

Segmode_7$Mode_rec <- as.numeric(Segmode_7$Mode_rec)

Segmode_7$Mode_rec <- recode(Segmode_7$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Visitation
Segmode_8 <- Segmode %>%
  filter(Purpose_rec == 8)

Segmode_8$Mode_rec <- as.numeric(Segmode_8$Mode_rec)

Segmode_8$Mode_rec <- recode(Segmode_8$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                    `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                    `999` = NA_character_)

#Touring, hiking
Segmode_9 <- Segmode %>%
  filter(Purpose_rec == 9)

Segmode_9$Mode_rec <- as.numeric(Segmode_9$Mode_rec)

Segmode_9$Mode_rec <- recode(Segmode_9$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                                      `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                                      `999` = NA_character_)

#Sports, hobby
Segmode_10 <- Segmode %>%
  filter(Purpose_rec == 8)

Segmode_10$Mode_rec <- as.numeric(Segmode_10$Mode_rec)

Segmode_10$Mode_rec <- recode(Segmode_10$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                             `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                             `999` = NA_character_)

#Other leisure time activities
Segmode_11 <- Segmode %>%
  filter(Purpose_rec == 11)

Segmode_11$Mode_rec <- as.numeric(Segmode_11$Mode_rec)

Segmode_11$Mode_rec <- recode(Segmode_11$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                             `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                             `999` = NA_character_)


#Services, presonal care
Segmode_12 <- Segmode %>%
  filter(Purpose_rec == 8)

Segmode_12$Mode_rec <- as.numeric(Segmode_12$Mode_rec)

Segmode_12$Mode_rec <- recode(Segmode_12$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                              `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                              `999` = NA_character_)


#Other purposes
Segmode_13 <- Segmode %>%
  filter(Purpose_rec == 13)

Segmode_13$Mode_rec <- as.numeric(Segmode_13$Mode_rec)

Segmode_13$Mode_rec <- recode(Segmode_13$Mode_rec, `1` = "Car", `2` = "Car", `3` = "Train",
                              `4` = "BTM", `5` = "ScoMop", `6` = "Bike", `7` = "Walk", 
                              `999` = NA_character_)



#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#Processing personal preference data

#Read data
Preference <- read_sav("/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality and travel dissonance/Data/2018/MPNWAVE6_Pdata_en.sav")


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
Preference_hotel <- Preference[c(6, 123:133)]

Preference_hotel <- Preference_hotel %>%
  mutate(Preference_hotel_3 = if_else(Preference_hotel[3] == 1, "Car", NA_character_)) %>%
  mutate(Preference_hotel_4 = if_else(Preference_hotel[4] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_hotel_5 = if_else(Preference_hotel[5] == 1, "ScoMop", NA_character_)) %>%
  mutate(Preference_hotel_6 = if_else(Preference_hotel[6] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_hotel_7 = if_else(Preference_hotel[7] == 1, "Train", NA_character_)) %>%
  mutate(Preference_hotel_8 = if_else(Preference_hotel[8] == 1, "BTM", NA_character_)) %>%
  mutate(Preference_hotel_9 = if_else(Preference_hotel[9] == 1, "Walk", NA_character_)) %>%
  mutate(Preference_hotel_10 = if_else(Preference_hotel[10] == 1, "Other", NA_character_)) %>%
  mutate(Preference_hotel_11 = if_else(Preference_hotel[11] == 1, "Bike", NA_character_)) %>%
  mutate(Preference_hotel_12 = if_else(Preference_hotel[12] == 1, "Other" , NA_character_))

Preference_hotel <- Preference_hotel[c(1,13:22)]

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

Preference_visitation <- merge(Preference_hotel, Preference_visitation, by = "PERSID")

# Merge selected columns into a list
Preference_visitation <- Preference_visitation %>%
  mutate(Visitation = pmap(list(Preference_hotel_3, Preference_hotel_4, Preference_hotel_5,
                              Preference_hotel_6, Preference_hotel_7, Preference_hotel_8,
                              Preference_hotel_9, Preference_hotel_10, Preference_hotel_11,
                              Preference_hotel_12, Preference_visitation_3, Preference_visitation_4, 
                              Preference_visitation_5, Preference_visitation_6, Preference_visitation_7, 
                              Preference_visitation_8, Preference_visitation_9, Preference_visitation_10, 
                              Preference_visitation_11, Preference_visitation_12), ~ na.omit(c(...))))  # Remove NAs

Preference_visitation <- Preference_visitation[c(1,22)]

Preference_visitation$Visitation <- lapply(Preference_visitation$Visitation, unique)

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

#-----------------------------------------To and from work-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_1 <- Segmode_1%>%
  left_join(Preference_commute, by = "PERSID")

as.vector(Merged_1$Commute)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_1$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_1$Mode_rec, Merged_1$Commute)


# Step 3: Summarize the data
Result_1 <- Merged_1 %>%
  group_by(PERSID) %>%
  summarize(
    Total_1 = n(),
    Consonant_1 = sum(Consonant)
  )


#-----------------------------------------Business-related visit in travel context-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_2 <- Segmode_2%>%
  left_join(Preference_business, by = "PERSID")

as.vector(Merged_2$Business)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_2$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_2$Mode_rec, Merged_2$Business)


# Step 3: Summarize the data
Result_2 <- Merged_2 %>%
  group_by(PERSID) %>%
  summarize(
    Total_2 = n(),
    Consonant_2 = sum(Consonant)
  )


#-----------------------------------------Dropping off, picking up people-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_4 <- Segmode_4%>%
  left_join(Preference_caregiving, by = "PERSID")

as.vector(Merged_4$Caregiving)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_4$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_4$Mode_rec, Merged_4$Caregiving)


# Step 3: Summarize the data
Result_4 <- Merged_4 %>%
  group_by(PERSID) %>%
  summarize(
    Total_4 = n(),
    Consonant_4 = sum(Consonant)
  )


#-----------------------------------------Delivering, picking up goods-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_5 <- Segmode_5%>%
  left_join(Preference_caregiving, by = "PERSID")

as.vector(Merged_5$Caregiving)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_5$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_5$Mode_rec, Merged_5$Caregiving)


# Step 3: Summarize the data
Result_5 <- Merged_5 %>%
  group_by(PERSID) %>%
  summarize(
    Total_5 = n(),
    Consonant_5 = sum(Consonant)
  )


#-----------------------------------------Following education study courses-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_6 <- Segmode_6%>%
  left_join(Preference_education, by = "PERSID")

as.vector(Merged_6$Education)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_6$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_6$Mode_rec, Merged_6$Education)


# Step 3: Summarize the data
Result_6 <- Merged_6 %>%
  group_by(PERSID) %>%
  summarize(
    Total_6 = n(),
    Consonant_6 = sum(Consonant)
  )


#-----------------------------------------Shopping, doing grocery shopping-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_7 <- Segmode_7%>%
  left_join(Preference_shopping, by = "PERSID")

as.vector(Merged_7$Shopping)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_7$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_7$Mode_rec, Merged_7$Shopping)


# Step 3: Summarize the data
Result_7 <- Merged_7 %>%
  group_by(PERSID) %>%
  summarize(
    Total_7 = n(),
    Consonant_7 = sum(Consonant)
  )


#-----------------------------------------Visitation-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_8 <- Segmode_8%>%
  left_join(Preference_visitation, by = "PERSID")

as.vector(Merged_8$Visitation)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_8$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_8$Mode_rec, Merged_8$Visitation)


# Step 3: Summarize the data
Result_8 <- Merged_8 %>%
  group_by(PERSID) %>%
  summarize(
    Total_8 = n(),
    Consonant_8 = sum(Consonant)
  )


#-----------------------------------------Touring, hiking-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_9 <- Segmode_9%>%
  left_join(Preference_leisure, by = "PERSID")

as.vector(Merged_9$Leisure)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_9$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_9$Mode_rec, Merged_9$Leisure)


# Step 3: Summarize the data
Result_9 <- Merged_9 %>%
  group_by(PERSID) %>%
  summarize(
    Total_9 = n(),
    Consonant_9 = sum(Consonant)
  )


#-----------------------------------------Sports, hobby-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_10 <- Segmode_10%>%
  left_join(Preference_sports, by = "PERSID")

as.vector(Merged_10$Sports)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_10$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_10$Mode_rec, Merged_10$Sports)


# Step 3: Summarize the data
Result_10 <- Merged_10 %>%
  group_by(PERSID) %>%
  summarize(
    Total_10 = n(),
    Consonant_10 = sum(Consonant)
  )


#-----------------------------------------Other leisure time activities-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_11 <- Segmode_11%>%
  left_join(Preference_leisure, by = "PERSID")

as.vector(Merged_11$Leisure)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_11$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_11$Mode_rec, Merged_11$Leisure)


# Step 3: Summarize the data
Result_11 <- Merged_11 %>%
  group_by(PERSID) %>%
  summarize(
    Total_11 = n(),
    Consonant_11 = sum(Consonant)
  )


#-----------------------------------------Services, personal care-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_12 <- Segmode_12%>%
  left_join(Preference_caregiving, by = "PERSID")

as.vector(Merged_12$Caregiving)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_12$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_12$Mode_rec, Merged_12$Caregiving)


# Step 3: Summarize the data
Result_12 <- Merged_12 %>%
  group_by(PERSID) %>%
  summarize(
    Total_12 = n(),
    Consonant_12 = sum(Consonant)
  )


#-----------------------------------------Other purposes-------------------------------------------------------------------------

# Step 1: Merge the trips with preferences
Merged_13 <- Segmode_13%>%
  left_join(Preference_leisure, by = "PERSID")

as.vector(Merged_13$Leisure)

# Step 2: Check if the used mode is in the preferred modes list and create a new column 'Match'

Merged_13$Consonant <- mapply(function(used_mode, preferred_modes) {
  if (is.na(used_mode)) {
    return(0)  # Handle NA values
  } else {
    return(as.integer(used_mode %in% preferred_modes))  # Check if used mode is in preferred modes
  }
}, Merged_13$Mode_rec, Merged_13$Leisure)


# Step 3: Summarize the data
Result_13 <- Merged_13 %>%
  group_by(PERSID) %>%
  summarize(
    Total_13 = n(),
    Consonant_13 = sum(Consonant)
  )


#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Calculate dissonance levels for each type of travel

#Mandatory (1, 2, 6)
Mandatory <- full_join(Result_1, Result_2, by = "PERSID")
Mandatory <- full_join(Mandatory, Result_6, by = "PERSID")

Mandatory[is.na(Mandatory)] <- 0

Mandatory$Total_mandatory <- Mandatory$Total_1 + Mandatory$Total_2 + Mandatory$Total_6
Mandatory$Cosonant_mandatory <- Mandatory$Consonant_1 + Mandatory$Consonant_2 + Mandatory$Consonant_6
Mandatory$Dissonance_mandatory <- Mandatory$Total_mandatory - Mandatory$Cosonant_mandatory


#Maintenance (4, 5, 7, 12)

Maintenance <- full_join(Result_4, Result_5, by = "PERSID")
Maintenance <- full_join(Maintenance, Result_7, by = "PERSID")
Maintenance <- full_join(Maintenance, Result_12, by = "PERSID")

Maintenance[is.na(Maintenance)] <- 0

Maintenance$Total_maintenance <- Maintenance$Total_4 + Maintenance$Total_5 + Maintenance$Total_7 + Maintenance$Total_12
Maintenance$Cosonant_maintenance <- Maintenance$Consonant_4 + Maintenance$Consonant_5 + Maintenance$Consonant_7 + Maintenance$Consonant_12
Maintenance$Dissonance_maintenance <- Maintenance$Total_maintenance - Maintenance$Cosonant_maintenance


#Discretionary (8, 9, 10, 11, 13)
Discretionary <- full_join(Result_8, Result_9, by = "PERSID")
Discretionary <- full_join(Discretionary, Result_10, by = "PERSID")
Discretionary <- full_join(Discretionary, Result_11, by = "PERSID")
Discretionary <- full_join(Discretionary, Result_13, by = "PERSID")

Discretionary[is.na(Discretionary)] <- 0

Discretionary$Total_discretionary <- Discretionary$Total_8 + Discretionary$Total_9 + Discretionary$Total_10 + Discretionary$Total_11 + Discretionary$Total_13
Discretionary$Cosonant_discretionary <- Discretionary$Consonant_8 + Discretionary$Consonant_9 + Discretionary$Consonant_10 + Discretionary$Consonant_11 + Discretionary$Consonant_13
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

write.csv(Dissonance, "/Users/xingxing/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Paper 5 Multimodality and travel dissonance/Data/2018/Dissonance_2018_final.csv")





