# Data Processing of the Netherlands Mobility Panel(MPN) Activity Participation Records in the Travel Diary
> **Note:** These are R codes for processing the Netherlands Mobility Panel(MPN) activity participation records in the travel diary (https://english.kimnet.nl/the-netherlands-mobility-panel/access-to-mpn-data).

## Introduction of MPN activity participation records in the travel diary
In the MPN travel diary, individuals report all trips they have taken over three consecutive days, providing details on the purpose and location of each origin and destination.
the travel diary includes 12 travel purpose categories, only seven of which can be directly matched with the preference categories, including commute, business-related travel, education/study, shopping, visitation, sports and leisure activities. To enable comparison, we operationalised the remaining travel-diary purpose categories. The category “transport as a profession” was excluded from the analysis. The categories “dropping off/picking up people”, “delivering/picking up goods”, and “services/personal care” were mapped to the mode preference category volunteer/caregiving, as they all represent caregiving-related activities. Similarly, the category “touring, hiking” was mapped to the mode preference category leisure activities. <img width="468" height="247" alt="image" src="https://github.com/user-attachments/assets/b7f43bd2-f9a2-4c76-a9b3-3dd8d176bf68" />


## File description
Files with names in the format "Processing_MPN_Activity_participation_****" are used to summarise the frequency with which an individual participated in each type of activity over the three consecutive days.
Files with names in the format "Processing_MPN_Dissonance_**" are used to analyse the extent to which the actual travel mode used for each type of activity matches the preferred mode for that activity.

## Variable descriptions
* Input variables
  - HHID: Unique Household ID
  - PERSID: Unique ID of Person
  - PHH_VALID: Complete household yes or no
  - P_VALID:	Person completed the survey
  - JAAR:	Research year
  - VPLID:	Unique ID per trip
  - WEGGEWEEST: Person travelled on trip day
  - VERTREKP:	Departure point 1st trip
  - AANTVPL: Number of trips per day
  - VPLDAGNR: Trip Day Number
  - VERPL:	New trip
  - VERPLNR:	Trip Number
  - AANTRIT:	Number of trip segments per trip
  - MOTIEF:	Purpose of trip	directly
  - KMOTIEF: Classification of purpose
  - AFSTV: Distance of trip (in km)
  - KAFSTV: Distance class trip
  - HVM: Main transport mode trip
  - KHVM: Main transport mode class trip
  - KVERTTIJD: Departure time class trip
  - REISDUUR: Travel time of trip in minutes
  - KREISDUUR: Travel duration class of trip
  - ROLAUTO: Role in car trip
  - RitID: Unique ID Trip-Segment
  - RIT: Trip-segment type
  - RITNR: Trip-segment number of Trip
  - RVM: Trip-segment transport mode
  - KRVM: Class trip-segment transport mode
  - Bestuurder: Driver trip-segment transport mode
  - SAMENREIZEN: Number of people travelling together (not including the person itself) on the trip-segment level
  - woonpc2: Postal code (PC2) place of residence
  - DAGSOORT: type of day: weekday, weekend
  - vertpc2: Trip Departure postal code (PC2)
  - aankpc2: Trip Arrival postal code (PC2)
  - VOORKEUR_WERK1: Preferred transport mode for home-to-work commute: not applicable
  - VOORKEUR_WERK2: Preferred means of transport for home-to-work commute: car
  - VOORKEUR_WERK3: Preferred means of transport for home-to-work commute: moped/scooter
  - VOORKEUR_WERK4: Preferred means of transport for home-to-work commute: motorcycle
  - VOORKEUR_WERK5: Preferred transport mode for home-to-work commute: bicycle/e-bike
  - VOORKEUR_WERK6: Preferred transport mode for home-to-work commute: train
  - VOORKEUR_WERK7: Preferred transport mode for home-to-work commute: bus/tram/metro
  - VOORKEUR_WERK8: Preferred transport mode for home-to-work commute: walking
  - VOORKEUR_ZAKELIJK1: Preferred transport mode for business-related travel: not applicable
  - VOORKEUR_ZAKELIJK2: Preferred transport mode for business-related travel: car
  - VOORKEUR_ZAKELIJK3: Preferred transport mode for business-related travel: moped/scooter
  - VOORKEUR_ZAKELIJK4: Preferred transport mode for business-related travel: motorcycle
  - VOORKEUR_ZAKELIJK5: Preferred transport mode for business-related travel: bicycle/e-bike
  - VOORKEUR_ZAKELIJK6: Preferred transport mode for business-related travel: train
  - VOORKEUR_ZAKELIJK7: Preferred transport mode for business-related travel: bus/tram/metro
  - VOORKEUR_ZAKELIJK8: Preferred transport mode for business-related travel: walking
  - VOORKEUR_SCHOOL1: 	Preferred transport mode for school/study: not applicable
  - VOORKEUR_SCHOOL2: 	Preferred transport mode for school/study: car
  - VOORKEUR_SCHOOL3: 	Preferred transport mode for school/study: moped/scooter
  - VOORKEUR_SCHOOL4: 	Preferred transport mode for school/study: motorcycle
  - VOORKEUR_SCHOOL5: 	Preferred transport mode for school/study: bicycle/e-bike
  - VOORKEUR_SCHOOL6: 	Preferred transport mode for school/study: train
  - VOORKEUR_SCHOOL7: 	Preferred transport mode for school/study: bus/tram/metro
  - VOORKEUR_SCHOOL8: 	Preferred transport mode for school/study: walking
  - VOORKEUR_BOODSCH1: Preferred transport mode for daily grocery shopping: not applicable
  - VOORKEUR_BOODSCH2: Preferred transport mode for daily grocery shopping: car
  - VOORKEUR_BOODSCH3: Preferred transport mode for daily grocery shopping: moped/scooter
  - VOORKEUR_BOODSCH4: Preferred transport mode for daily grocery shopping: motorcycle
  - VOORKEUR_BOODSCH5: Preferred transport mode for daily grocery shopping: bicycle/e-bike
  - VOORKEUR_BOODSCH6: Preferred transport mode for daily grocery shopping: train
  - VOORKEUR_BOODSCH7: Preferred transport mode for daily grocery shopping: bus/tram/metro
  - VOORKEUR_BOODSCH8: Preferred transport mode for daily grocery shopping: walking
  - VOORKEUR_WINKEL1: Preferred transport mode for shopping: not applicable
  - VOORKEUR_WINKEL2: Preferred transport mode for shopping: car
  - VOORKEUR_WINKEL3: Preferred transport mode for shopping: moped/scooter
  - VOORKEUR_WINKEL4: Preferred transport mode for shopping: motorcycle
  - VOORKEUR_WINKEL5: Preferred transport mode for shopping: bicycle/e-bike
  - VOORKEUR_WINKEL6: Preferred transport mode for shopping: train
  - VOORKEUR_WINKEL7: Preferred transport mode for shopping: bus/tram/metro
  - VOORKEUR_WINKEL8: Preferred transport mode for shopping: walking
  - VOORKEUR_VISITE1: Preferred transport mode for visiting other people: not applicable
  - VOORKEUR_VISITE2: Preferred transport mode for visiting other people: car
  - VOORKEUR_VISITE3: Preferred transport mode for visiting other people: moped/scooter
  - VOORKEUR_VISITE4: Preferred transport mode for visiting other people: motorcycle
  - VOORKEUR_VISITE5: Preferred transport mode for visiting other people: bicycle/e-bike
  - VOORKEUR_VISITE6: Preferred transport mode for visiting other people: train
  - VOORKEUR_VISITE7: Preferred transport mode for visiting other people: bus/tram/metro
  - VOORKEUR_VISITE8: Preferred transport mode for visiting other people: walking
  - VOORKEUR_UITGAAN1: Preferred transport mode for going out: not applicable
  - VOORKEUR_UITGAAN2: Preferred transport mode for going out: car
  - VOORKEUR_UITGAAN3: Preferred transport mode for going out: moped/scooter
  - VOORKEUR_UITGAAN4: Preferred transport mode for going out: motorcycle
  - VOORKEUR_UITGAAN5: Preferred transport mode for going out: bicycle/e-bike
  - VOORKEUR_UITGAAN6: Preferred transport mode for going out: train
  - VOORKEUR_UITGAAN7: Preferred transport mode for going out: bus/tram/metro
  - VOORKEUR_UITGAAN8: Preferred transport mode for going out: walking
  - VOORKEUR_EROPUIT1: Preferred transport mode for a day trip: not applicable
  - VOORKEUR_EROPUIT2: Preferred transport mode for a day trip: car
  - VOORKEUR_EROPUIT3: Preferred transport mode for a day trip: moped/scooter
  - VOORKEUR_EROPUIT4: Preferred transport mode for a day trip: motorcycle
  - VOORKEUR_EROPUIT5: Preferred transport mode for a day trip: bicycle/e-bike
  - VOORKEUR_EROPUIT6: Preferred transport mode for a day trip: train
  - VOORKEUR_EROPUIT7: Preferred transport mode for a day trip: bus/tram/metro
  - VOORKEUR_EROPUIT8: Preferred transport mode for a day trip: walking
  - VOORKEUR_SPORTEN1: Preferred transport mode for sports: not applicable
  - VOORKEUR_SPORTEN2: Preferred transport mode for sports: car
  - VOORKEUR_SPORTEN3: Preferred transport mode for sports: moped/scooter
  - VOORKEUR_SPORTEN4: Preferred transport mode for sports: motorcycle
  - VOORKEUR_SPORTEN5: Preferred transport mode for sports: bicycle/e-bike
  - VOORKEUR_SPORTEN6: Preferred transport mode for sports: train
  - VOORKEUR_SPORTEN7: Preferred transport mode for sports: bus/tram/metro
  - VOORKEUR_SPORTEN8: Preferred transport mode for sports: walking
    
* Output variables 
