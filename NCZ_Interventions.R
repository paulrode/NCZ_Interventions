
# Load packages 

my_packages <- c("tidyverse", "vroom" , "janitor" , "glue" , "tsibble" , "tidytext","lubridate", "fable", "tsibbledata", "ggplot2", "forecast", "tseries", "rio", "zoo", "readxl", 
                 "tsibbledata", "knitr", "formattable", "scales", "tidyr" ,"kableExtra", "dplyr", "gridExtra", "writexl")   

# Took these out but need to keep them front of mind "timetk" , "recipes" 

invisible( lapply(my_packages, require, character.only = TRUE))  


# Alternate Start Point     
#Set up environment 
`%notin%` <- Negate(`%in%`)
# place <- "Home"  #Where are we working today. 
 place <- "work"
if (place == "Home"){setwd("C:/Users/paulr/Documents/R/NCZ_Interventions")} else {setwd("C:/Users/prode/OneDrive - Tishman Speyer/Documents/R/NCZ_Interventions")}
if (!file.exists("data")) { dir.create("data")}
rm(place, my_packages ) #Clean up
options(dplyr.summarise.inform = FALSE)  # Suppress text in Knit printout. 

#
# This script will push out to interventions. Interventions are to have reduction percentages in the fuel columns, and for electrificaiton reduction in the fuel
# column and in the electric column the COP of the replacements. 
# Need to add 3 columns to the intervention sheet.Building name as it appears in Portfolio Manager, then the savings catagories, then the order to be taken. 
# Savings categories are Heating, Cooling, Base, Heating & Cooling & Base, Heating & Cooling, 
#

# Read in data
# Import first sheet
TSUS_EPA_DATA_SHEETS <-excel_sheets("data/Energy Star_Energy Use by Calendar Month_US Properties.xlsx")
TSUS_EPA_DATA <- read_excel("data/Energy Star_Energy Use by Calendar Month_US Properties.xlsx",skip = 5, na = "Not Available", sheet = 1)
apply(TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)], 2, function(row) as.numeric(row)) -> TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)]
gather(TSUS_EPA_DATA, key = "CarbonSource", value = "Value", -Month) %>% 
mutate(Building = TSUS_EPA_DATA_SHEETS[1])-> TSUS_EPA_DATA_LONG
TSUS_EPA_DATA_LONG$Month <-  my(TSUS_EPA_DATA_LONG$Month)
TSUS_EPA_DATA_LONG_ALL <- TSUS_EPA_DATA_LONG
# Get rest of sheets in
for (i  in 2:length(TSUS_EPA_DATA_SHEETS)) {
  "TSUS_EPA_DATA" <- read_excel("data/Energy Star_Energy Use by Calendar Month_US Properties.xlsx",skip = 5, na = "Not Available", sheet = i)
   TSUS_EPA_DATA$Month <-  my(TSUS_EPA_DATA$Month)
   apply(TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)], 2, function(row) as.numeric(row)) -> TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)]
   gather(TSUS_EPA_DATA, key = "CarbonSource", value = "Value", -Month) %>% 
   mutate( Building = TSUS_EPA_DATA_SHEETS[i])-> TSUS_EPA_DATA_LONG
   rbind(TSUS_EPA_DATA_LONG_ALL, TSUS_EPA_DATA_LONG) -> TSUS_EPA_DATA_LONG_ALL }
spread(TSUS_EPA_DATA_LONG_ALL, key = CarbonSource, value = Value) -> TSUS_EPA_DATA_SHORT_ALL 
 TSUS_EPA_DATA_SHORT_ALL %>% 
  select(2,1,3:ncol(TSUS_EPA_DATA_SHORT_ALL)) %>% 
  arrange(Building, Month) %>% 
   mutate(DateM = month(Month), DateY = year(Month)) %>% 
   filter(DateY > 2018 & DateY < 2022) %>% 
   select(1,10,9, 3:8) -> TSUS_EPA_DATA_SHORT_ALL
 remove("TSUS_EPA_DATA", "TSUS_EPA_DATA_LONG", "TSUS_EPA_DATA_LONG_ALL", i )
 
 
 # Average out 'Y' years 
 Y <- 3
 TSUS_EPA_DATA_SHORT_ALL %>% 
   group_by(Building, DateM) %>% 
   summarise(Elect_kBTU = sum(`Electric - Grid\r\n(kBtu)`/Y, na.rm=TRUE),
             NGas_kbtu = sum(`Natural Gas\r\n(kBtu)`/Y, na.rm=TRUE),
             Steam_btu = sum(`District Steam\r\n(kBtu)`/Y, na.rm=TRUE),
             Oil2_btu = sum(`Fuel Oil (No. 2)\r\n(kBtu)`/Y, na.rm=TRUE),
             Oil4_btu = sum(`Fuel Oil (No. 4)\r\n(kBtu)`/Y, na.rm=TRUE),
             Diesel_btu = sum(`Diesel\r\n(kBtu)`/Y, na.rm=TRUE)) -> TSUS_EPA_DATA_SHORT_ALL
 
 # Puttin in here the total BTU sum over all fules. 
 
 TSUS_EPA_DATA_SHORT_ALL[is.na(TSUS_EPA_DATA_SHORT_ALL)] = 0
 TSUS_EPA_DATA_SHORT_ALL %>% 
     mutate(Total_btu = Elect_kBTU + NGas_kbtu + Steam_btu + Oil2_btu + Oil4_btu + Diesel_btu) -> TSUS_EPA_DATA_SHORT_ALL

 
 #############################################################################################################################################
 #  Base calculations for all fuel types   
 # This is the main logic for assigning uses. Need sequence that allows assignment based on building configurations. 
 # Try this identify for all fuel types the breakdown and have logive assign use accordingly. 
   
 #############################################################################################################################################

 
 TSUS_EPA_DATA_SHORT_ALL %>% 
  group_by(Building) %>% 
  reframe(DateM, Elect_kBTU, NGas_kbtu, Steam_btu, Oil2_btu, Oil4_btu, Diesel_btu, Total_btu,"Base_E" = min(Elect_kBTU), "Base_NG" = min(NGas_kbtu), "Base_S" = min(Steam_btu), "Base_2" = min(Oil2_btu), "Base_4" = min(Oil4_btu), "Base_D" = min(Diesel_btu)) %>% 
  mutate(Elect_use = ifelse(Base_E == Elect_kBTU, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>%
  mutate(NG_use = ifelse(Base_NG == NGas_kbtu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
  mutate(Steam_use = ifelse(Base_S == Steam_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
  mutate(Oil2_use = ifelse(Base_2 == Oil2_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
  mutate(Oil4_use = ifelse(Base_2 == Oil2_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
  mutate(Diesel_use = ifelse(Base_D == Diesel_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) -> TSUS_EPA_DATA_SHORT_ALL

 
#############################################################################################################################################
#  Base calculations for all fuel types   

#############################################################################################################################################

TSUS_EPA_DATA_SHORT_ALL %>%  
  group_by(Building, Elect_use, NG_use, Steam_use, Oil2_use, Oil4_use, Diesel_use) %>%  
  summarise(Elect = sum(Elect_kBTU), NGas = sum(NGas_kbtu), Steam = sum(Steam_btu), Oil2 = sum(Oil2_btu), Oil4 = sum(Oil4_btu), Diesel = sum(Diesel_btu), Total = sum(Total_btu)) %>%  
  mutate("Elect_kWH" = Elect/3.418, "Steam_Mlb" = Steam/1194) %>%  
  select(Building, Elect_kWH, Steam_Mlb, Elect, NGas, Steam, Oil2, Oil4, Diesel, Elect_use, NG_use, Steam_use, Oil2_use, Oil4_use, Diesel_use ) -> EndUseAllocation 

#Buildings with missing data not making it though analysis. 
(TSUS_EPA_DATA_SHEETS[TSUS_EPA_DATA_SHEETS %notin% unique(TSUS_EPA_DATA_SHORT_ALL$Building)] )
# output of above line is: "1395 Crossman"   "66 Hudson Blvd."

#############################################################################################################################################
# Make interventions table in excel and read here, then pull in EndUseAllocations 

#############################################################################################################################################



# Make a building data file and input here. Ease building will have sf for ratioing costs and a configuration code
#Building Data File 
BuildingData <- read_excel("data/BuildingData.xlsx", na = "Not Available", sheet = 1)

#Join BuildingData with EndUseAllocation 
left_join(EndUseAllocation, BuildingData, by = "Building") -> EndUseAllocation

##############################################################################################################################################
# Try removing buildings we dont own anymore, and clean up environment. 

##############################################################################################################################################

EndUseAllocation %>% 
  filter(Building != "1275 Crossman" & Building != "1345 Crossman" & Building != "1395 Crossman" & Building != "1375 Crossman") %>% 
  select( -Note) -> EndUseAllocation


remove(TSUS_EPA_DATA_SHORT_ALL, TSUS_EPA_DATA_SHEETS) 


#####################################################################################################################
#Building Intervention File                                                                                         #
# when filling out the intervention excel sheet place % reductions in fuel typ, use negitive as an increase in load.#
#                                                                                                                   #
#####################################################################################################################


Interventions <- read_excel("data/Interventions_One_Federal_source.xlsx",skip = 16, na = "Not Available", sheet = 1)%>% 
  select(1:12) %>% 
  select( -6, -7, -8,-12) %>% 
  select(1, 4,2,3,5,6,7,8)
##### Reorder Interventions Here
Interventions <-Interventions %>% arrange(Order)

EUA_Elect <- EndUseAllocation[c("Building", "Elect", "Elect_use")]
EUA_Elect %>%  
  group_by(Building, Elect_use) %>%  
  summarise(Elect = sum(Elect))  %>% 
  rename( Use = Elect_use) -> EUA_Elect 

  
EUA_Oil2 <- EndUseAllocation[c("Building", "Oil2", "Oil2_use")]
EUA_Oil2 %>% 
  group_by(Building, Oil2_use) %>%  
  summarise(Oil2 = sum(Oil2)) %>% 
  select("Building", "Oil2", "Oil2_use") -> EUA_Oil2
 # EUA_Oil2$Oil2_use = "Heating Loads"
  rename(EUA_Oil2, Use = Oil2_use) -> EUA_Oil2
  

EUA_Oil4 <- EndUseAllocation[c("Building", "Oil4", "Oil4_use")]
EUA_Oil4 %>% 
  group_by(Building, Oil4_use) %>%  
  summarise(Oil4 = sum(Oil4)) %>%  
  select("Building", "Oil4", "Oil4_use") -> EUA_Oil4 
 # EUA_Oil4$Oil4_use = "Heating Loads"
  rename(EUA_Oil4, Use = Oil4_use) -> EUA_Oil4

EUA_Steam <- EndUseAllocation[c("Building", "Steam", "Steam_use")]
EUA_Steam %>% 
  group_by(Building, Steam_use) %>%  
  summarise(Steam = sum(Steam)) %>% 
  select("Building", "Steam", "Steam_use") %>% 
  rename(Use = Steam_use)-> EUA_Steam


EUA_NGas <- EndUseAllocation[c("Building", "NGas", "NG_use")]
EUA_NGas %>% 
  group_by(Building, NG_use) %>%  
  summarise(NGas = sum(NGas)) %>%
  select("Building", "NGas", "NG_use") %>% 
  rename(Use = NG_use)-> EUA_NGas


left_join(EUA_Elect, EUA_NGas, by = c("Building", "Use")) %>% 
left_join(EUA_Steam, by = c("Building", "Use")) %>% 
left_join(EUA_Oil2, by = c("Building", "Use")) %>% 
left_join(EUA_Oil4, by = c("Building", "Use")) -> EndUseAllocation
EndUseAllocation[is.na(EndUseAllocation)] <-0

#Join BuildingData with EndUseAllocation 
left_join(EndUseAllocation, BuildingData, by = "Building") -> EndUseAllocation

remove(EUA_Elect, EUA_NGas, EUA_Oil2, EUA_Oil4, EUA_Steam, BuildingData)

EndUseAllocation %>% 
  gather(key = "Load", value = value, 3:7) %>% 
  filter(value != 0 ) %>% 
  spread(key = "Use", value = value, fill = 0) -> testfit1

testfit1 %>% 
  select(1, 3, 9, 10:12) -> testfit1

# Bringing in Interventions. 
right_join(testfit1, Interventions, by = "Building", multiple = "all") %>% 
na.omit() -> Savings

# For calculations of savings setting key catagories to zero
Savings %>% 
  mutate("Saved_Base" = 0, "Saved_Cooling" = 0, "Saved_Heating" = 0) %>% 
  arrange(desc(Load))-> Savings


EndUseAllocation_Wide <- testfit1
remove(testfit1)
unique(Savings$Load) -> loads
length(loads) -> fuels
length(Savings$Load) -> rows
rows/fuels
Electrification_f <- function(i){a <- i + 2* i}

Savings %>% filter(`Description of Measure` == "Electrificaiton") -> Savings_Electrificaiton
Savings %>%  filter(`Description of Measure` != "Electrificaiton") -> Savings_Measures

rm(Savings)

###########################################################################################################
###########################################################################################################
#    In this block elect savings not being calculated. 
###########################################################################################################
###########################################################################################################
# Run down Savings Measures 

for(i in 1:length(Savings_Measures$Load)) { 
  if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton"  )  {
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
    
  if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" ) {
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" ) {
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
  
  if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
    
  if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton")  {
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
    
    if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton")  {
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
  
  if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Heating[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{
    
  if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Heating[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Heating[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
    
  if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
    
  if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i]  -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
    
  if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Cooling[i];
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
      
  if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs` [i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs` [i]  -> Savings_Measures$Saved_Cooling[i];
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{
    
    if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] != "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)` [i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)` [i]  -> Savings_Measures$Saved_Cooling[i];
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{
    
  if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Base[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
      
  if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" ) {
      Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Base[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {    
      Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Base" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" ) {
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {    
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{ 
        
  if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton")  {
      Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Cooling[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
          
  if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Cooling[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {        
      Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
    
    if(Savings_Measures$Savings[i] == "Cooling" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {        
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Heating Loads`[i] -> Savings_Measures$`Heating Loads`[i+1] }}
  }else{
            
  if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Heating[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{
              
  if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton")  {
      Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Heating[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Heating" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton")  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Heating[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
                
  if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Heating[i];
      Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Cooling[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
                  
  if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i]  -> Savings_Measures$Saved_Heating[i];
      Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i]  -> Savings_Measures$Saved_Cooling[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
      Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating L`[i+1];
      Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
      Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Heating & Cooling" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i]  -> Savings_Measures$Saved_Cooling[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating L`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1];
    Savings_Measures$`Base Loads`[i] -> Savings_Measures$`Base Loads`[i+1] }}
  }else{ 
                    
  if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "Elect" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton")  {
     Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Heating[i];
     Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings_Measures$Saved_Cooling[i];
     Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings_Measures$Saved_Base[i];
     if (i < length(Savings_Measures$Load)) {
     if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
     Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
     Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
     Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
    
    if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "NGas" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton")  {
    Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i]  -> Savings_Measures$Saved_Heating[i];
    Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i]  -> Savings_Measures$Saved_Cooling[i];
    Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Measures$Saved_Base[i];
    if (i < length(Savings_Measures$Load)) {
    if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) {
    Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
    Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
    Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
  }else{ 
                      
  if(Savings_Measures$Savings[i] == "Heating & Cooling & Base" & Savings_Measures$Load[i] == "Steam_Mlb" & Savings_Measures$`Description of Measure`[i] == "Electrificaiton" )  {
      Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs` [i]  -> Savings_Measures$Saved_Heating[i];
      Savings_Measures$`Cooling Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs` [i]  -> Savings_Measures$Saved_Cooling[i];
      Savings_Measures$`Base Loads`[i] * Savings_Measures$`Change in Steam Consumption, kLbs`[i] -> Savings_Measures$Saved_Base[i];
      if (i < length(Savings_Measures$Load)) {
      if (Savings_Measures$Load[i] == Savings_Measures$Load[i+1]) { 
      Savings_Measures$`Heating Loads`[i] - Savings_Measures$Saved_Heating[i] -> Savings_Measures$`Heating Loads`[i+1];
      Savings_Measures$`Base Loads`[i] - Savings_Measures$Saved_Base[i] -> Savings_Measures$`Base Loads`[i+1];
      Savings_Measures$`Cooling Loads`[i] - Savings_Measures$Saved_Cooling[i] -> Savings_Measures$`Cooling Loads`[i+1] }}
                        
    
  }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}




# Moving over loads to Electrification Data Frame

nrow(Savings_Electrificaiton) -> b
b2 <- b/2
if(Savings_Measures$Load[1] == "NGas"){
filter(Savings_Measures, Load == "NGas")[nrow(filter(Savings_Measures, Load == "NGas")),4:6] -> Savings_Electrificaiton[1:(b/2),4:6]
}else{
filter(Savings_Measures, Load == "Steam_Mlb")[nrow(filter(Savings_Measures, Load == "Steam_Mlb")),4:6] -> Savings_Electrificaiton[1:(b/2),4:6]}

filter(Savings_Measures, Load == "Elect")[nrow(filter(Savings_Measures, Load == "Elect")),4:6] -> Savings_Electrificaiton[((b/2)+1):b,4:6]


# Do the calculations for savings here. 

Savings_Electrificaiton %>% filter(Load != "Elect") -> Savings_Electrificaiton

for(i in 1:length(Savings_Electrificaiton$Load)) { 
  if(Savings_Electrificaiton$Savings[i] == "Base" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
    Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Base[i];
    if(Savings_Electrificaiton$Saved_Base[i] != 0) {Savings_Electrificaiton$`Base Loads` <- Savings_Electrificaiton$`Base Loads` - Savings_Electrificaiton$Saved_Base[i]};
  }else{
    
    if(Savings_Electrificaiton$Savings[i] == "Base" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
      Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Base[i];
      if(Savings_Electrificaiton$Saved_Base[i] != 0 ) { Savings_Electrificaiton$`Base Loads` <- Savings_Electrificaiton$`Base Loads`[i] - Savings_Electrificaiton$Saved_Base[i]};
   }else{
    
    if(Savings_Electrificaiton$Savings[i] == "Cooling" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
      Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
      ifelse(Savings_Electrificaiton$Saved_Cooling == 0, Savings_Electrificaiton$`Cooling Loads` , Savings_Electrificaiton$`Cooling Loads` <- Savings_Electrificaiton$`Cooling Loads`  - Savings_Electrificaiton$Saved_Cooling[i]);
    }else{
      
      if(Savings_Electrificaiton$Savings[i] == "Cooling" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
        Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
        ifelse(Savings_Electrificaiton$Saved_Cooling == 0, Savings_Electrificaiton$`Cooling Loads` , Savings_Electrificaiton$`Cooling Loads` <- Savings_Electrificaiton$`Cooling Loads`  - Savings_Electrificaiton$Saved_Cooling[i]);
      }else{
      
      if(Savings_Electrificaiton$Savings[i] == "Heating" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
        Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Heating[i];
        ifelse(Savings_Electrificaiton$Saved_Heating == 0, Savings_Electrificaiton$`Heating Loads` , Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads`  - Savings_Electrificaiton$Saved_Heating[i]);
      }else{
        
        if(Savings_Electrificaiton$Savings[i] == "Heating" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
          Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Heating[i];
          Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads` - Savings_Electrificaiton$Saved_Heating[i];
          ifelse(Savings_Electrificaiton$Saved_Heating == 0, Savings_Electrificaiton$`Heating Loads` , Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads`  - Savings_Electrificaiton$Saved_Heating[i]);
          }else{
        
        if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
          Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Heating[i];
          Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
          ifelse(Savings_Electrificaiton$Saved_Heating == 0, Savings_Electrificaiton$`Heating Loads` , Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads`  - Savings_Electrificaiton$Saved_Heating[i]);
          ifelse(Savings_Electrificaiton$Saved_Cooling == 0, Savings_Electrificaiton$`Cooling Loads` , Savings_Electrificaiton$`Cooling Loads` <- Savings_Electrificaiton$`Cooling Loads`  - Savings_Electrificaiton$Saved_Cooling[i]);
        }else{
          
          if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
            Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Heating[i];
            Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
            ifelse(Savings_Electrificaiton$Saved_Heating == 0, Savings_Electrificaiton$`Heating Loads` , Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads`  - Savings_Electrificaiton$Saved_Heating[i]);
            ifelse(Savings_Electrificaiton$Saved_Cooling == 0, Savings_Electrificaiton$`Cooling Loads` , Savings_Electrificaiton$`Cooling Loads` <- Savings_Electrificaiton$`Cooling Loads`  - Savings_Electrificaiton$Saved_Cooling[i]);
            
          }else{
          
          if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling & Base" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
            Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Base[i];
            Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Heating[i];
            Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
      
          }else{
            
            if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling & Base" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
              Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Base[i];
              Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Heating[i];
              Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Cooling[i];

      
                    }}}}}}}}}}}

#############################################################################################################


# Some Cleanup 
#remove(EndUseAllocation, EndUseAllocation_Wide)


Savings_Electrificaiton  %>%
  mutate("Saved" = Saved_Base + Saved_Cooling + Saved_Heating) %>% 
  select(-c(4,5,6,8,14,15,16)) -> Savings_Electrificaiton 



if(Savings_Measures$Load[1] == "NGas"){
  filter(Savings_Electrificaiton,  Load == "NGas") -> Savings_Electrificaiton 
}else{
  filter(Savings_Electrificaiton,  Load == "Steam_Mlb") -> Savings_Electrificaiton }





# Move savings to native innervation columns for Electrification  

for (i in 1:nrow(Savings_Electrificaiton)) { 
if(Savings_Electrificaiton$Load[i] == "Steam_Mlb") {Savings_Electrificaiton$Saved[i] / 1194 -> Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i];
  Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- ( 0.29307 * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] / Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i]);
}else{ 
  if(Savings_Electrificaiton$Load[i] == "NGas") {Savings_Electrificaiton$Saved[i]/1000 -> Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i];
    Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- 293.07 * (Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i])/ Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i];
  }else{   Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- 0.29307 * Savings_Electrificaiton$Saved[i]  }
}}
  

  
  # Move savings to native innervation columns for Measures  
  
  Savings_Measures  %>%
    mutate("Saved" = Saved_Base + Saved_Cooling + Saved_Heating) %>% 
    select(-c(4,5,6,8,14,15,16)) -> Savings_Measures

for (i in 1: nrow(Savings_Measures)) { 
  if(Savings_Measures$Load[i] == "Steam_Mlb") {Savings_Measures$Saved[i] * 1.194 -> Savings_Measures$`Change in Steam Consumption, kLbs`[i];
    Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] <- 0;
    Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] <- 0;
  }else{ 
    if(Savings_Measures$Load[i] == "NGas") {Savings_Measures$Saved[i]/1000 -> Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i];
       Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] <- 0;
  }else{ Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] <- 0.29307 * Savings_Measures$Saved[i];
    Savings_Measures$`Change in Steam Consumption, kLbs`[i] <- 0;
    Savings_Measures$`Change in Natural Gas Use(MMBtu)`[i] <- 0;
  }}}
  
  
  # Moving Electric up 
  Savings_Measures %>% 
    filter( Load == "Elect_kWH") %>% 
    select(7) -> change_kWH 
  rbind(change_kWH, change_kWH) -> change_kWH
  Savings_Measures$`Change in Electricity Consumption Reduction (kWh)` <- change_kWH$`Change in Electricity Consumption Reduction (kWh)`
  
 
  
  
   #cleaning up measures
 
  if(Savings_Measures$Load[1] == "Steam_Mlb") { Savings_Measures <-  filter(Savings_Measures, Savings_Measures$Load == "Steam_Mlb")
    }else{
      if(Savings_Measures$Load[1] == "NGas") {Savings_Measures <- filter(Savings_Measures, Savings_Measures$Load == "NGas")}}
       
        
  select(Savings_Measures, -3,-10) -> Savings_Measures;
  

  Savings_Electrificaiton %>% 
    select(-3,-10) -> Savings_Electrificaiton
  
  rbind(Savings_Measures, Savings_Electrificaiton) -> Savings 
  
  remove(change_kWH, Savings_Electrificaiton, Savings_Measures, b, b2, fuels, i, loads, Interventions, rows)
  
  
  write_csv(Savings, "data/Savings.csv" )