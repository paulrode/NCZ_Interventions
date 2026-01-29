

my_packages <- c("tidyverse", "vroom" , "janitor" , "glue" , "tsibble" , "tidytext","lubridate", "fable", "tsibbledata", "ggplot2", "forecast", "tseries", "rio", "zoo", "readxl", 
                 "tsibbledata", "knitr", "purrr" ,"formattable", "scales", "tidyr" , "gridExtra")
  
  
# Took these out but need to keep them front of mind "timetk" , "recipes" 

invisible( lapply(my_packages, require, character.only = TRUE))# Alternate Start Point     

#Set up environment 
`%notin%` <- Negate(`%in%`)
place <- "Home"  #Where are I working today. 
# place <- "Work"
if (place == "Home"){setwd("/Users/paulrode/Documents/R/NCZ_Interventions")} else {setwd("C:/Users/prode/OneDrive - Tishman Speyer/Documents/R/NCZ_Interventions")}
if (!file.exists("data")) { dir.create("data")}
rm(place, my_packages ) #Clean up
options(dplyr.summarise.inform = FALSE)  # Suppress text in Knit printout. 


# This script will push out to interventions. Interventions are to have reduction percentages in the fuel columns, and for electrificaiton reduction in the fuel
# column and in the electric column the COP of the replacements. 
# Need to add 3 columns to the intervention sheet.Building name as it appears in Portfolio Manager, then the savings catagories, then the order to be taken. 
# Savings categories are Heating, Cooling, Base, Heating & Cooling & Base, Heating & Cooling, 

# Read in Portfolio Manager data
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
   # This is where you change the date range of what is pulled from the EPA month file 
   filter(DateY > 2019 & DateY < 2023) %>% 
   select(1,10,9, 3:8) -> TSUS_EPA_DATA_SHORT_ALL
 remove("TSUS_EPA_DATA", "TSUS_EPA_DATA_LONG", "TSUS_EPA_DATA_LONG_ALL", i )

 # just above is where you can select the Portfolio Manager years to use. ####
 
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
 
 

 # Putting in here the total BTU sum over all fuels.
 
 TSUS_EPA_DATA_SHORT_ALL[is.na(TSUS_EPA_DATA_SHORT_ALL)] = 0
 TSUS_EPA_DATA_SHORT_ALL %>% 
     mutate(Total_btu = Elect_kBTU + NGas_kbtu + Steam_btu + Oil2_btu + Oil4_btu + Diesel_btu) -> TSUS_EPA_DATA_SHORT_ALL
 as.numeric(format(TSUS_EPA_DATA_SHORT_ALL$NGas_kbtu, digits = 2, scientific = FALSE)) -> TSUS_EPA_DATA_SHORT_ALL$NGas_kbtu
 
 
 TSUS_EPA_DATA_SHORT_ALL %>% 
  group_by(Building) %>% 
  reframe(DateM, Elect_kBTU, NGas_kbtu, Steam_btu, Oil2_btu, Oil4_btu, Diesel_btu, Total_btu, "Base_kBTU" = min(Total_btu),  "Base_E" = min(Elect_kBTU), "Base_NG" = min(NGas_kbtu), "Base_S" = min(Steam_btu), "Base_2" = min(Oil2_btu), "Base_4" = min(Oil4_btu), "Base_D" = min(Diesel_btu)) -> TSUS_EPA_DATA_SHORT_ALL
 
 BuildingData <- read_excel("data/BuildingData.xlsx", na = "Not Available", sheet = 1)
 
 left_join(TSUS_EPA_DATA_SHORT_ALL, BuildingData, by = "Building") -> TSUS_EPA_DATA_SHORT_ALL
 
 BuildingData %>% 
   mutate(Code = str_c(
  str_sub(BuildingData$`Primary Heating`, start = 1L, end = 1L), 
  str_sub(BuildingData$`Primary Cooling`, start = 1L, end = 1L),
  str_sub(BuildingData$`Primary DHW`, start = 1L, end = 1L),
  str_sub(BuildingData$Generator, start = 1L, end = 1L), sep=""))-> BuildingData

 
 
 
 
 
 
 
 
 
 # Here is where the load types are put in. Heating, Cooling Base. 
 
  TSUS_EPA_DATA_SHORT_ALL %>% 
   mutate(Elect_use = ifelse(Base_E == Elect_kBTU, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>%
   mutate(NG_use = ifelse(Base_NG == NGas_kbtu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
   mutate(Steam_use = ifelse(Base_S == Steam_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
   mutate(Oil2_use = ifelse(Generator == "Oil2", "Generator", ifelse(Base_2 == Oil2_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads")))) %>% 
   mutate(Oil4_use = ifelse(Base_2 == Oil2_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) %>% 
   mutate(Diesel_use = ifelse(Generator == "Diesel", "Generator", ifelse(Base_D == Diesel_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads")))) -> TSUS_EPA_DATA_SHORT_ALL
 
  
  # Approach on how to separate tenant load from buildings loads. 
  # Select base_e as the lowest electric use month and place in a column. 
  # Take Y percentage of Base_E and associate that to what tenants use non-weather dependent. 
  # { Base_E' = (1 - TenantBasePercentage / 100) * Base_E)  } -> Base_E, keep total as is.  
  # The assumption is that Tenant Base Energy is constant. Take this out of decarb 
  # efficiency measures. Need to remember to put this back somewhere to balance out the btu's 
  
  TenantBasePercentage <- 60
  TSUS_EPA_DATA_SHORT_ALL %>%  
  mutate(Base_E = (1 - TenantBasePercentage / 100) * Base_E)  -> TSUS_EPA_DATA_SHORT_ALL

 
  # This is where we introduce EndUseAllocation made from TSUS_EPA_DATA_SHORT_ALL
  #  Base calculations for all fuel types   
TSUS_EPA_DATA_SHORT_ALL %>%  
  group_by(Building, Elect_use, NG_use, Steam_use, Oil2_use, Oil4_use, Diesel_use) %>%  
  summarise(Elect = sum(Elect_kBTU), Base_E = sum(Base_E) , NGas = sum(NGas_kbtu), Steam = sum(Steam_btu), Oil2 = sum(Oil2_btu), Oil4 = sum(Oil4_btu), Diesel = sum(Diesel_btu), Total = sum(Total_btu)) %>%  
  mutate("Elect_kWH" = Elect/3.418, "Base_E_kWH" = Base_E/3.418, "Steam_Mlb" = Steam/1194) %>%  
  select(Building, Elect_kWH, Base_E_kWH, Steam_Mlb, Elect, NGas, Steam, Oil2, Oil4, Diesel, Elect_use, NG_use, Steam_use, Oil2_use, Oil4_use, Diesel_use ) -> EndUseAllocation


# Buildings with missing data not making it though analysis. 
(TSUS_EPA_DATA_SHEETS[TSUS_EPA_DATA_SHEETS %notin% unique(TSUS_EPA_DATA_SHORT_ALL$Building)] )


EndUseAllocation %>% 
  filter(Building != "1275 Crossman" & Building != "1345 Crossman" & Building != "1395 Crossman" & Building != "1375 Crossman")  -> EndUseAllocation

remove(TSUS_EPA_DATA_SHORT_ALL, TSUS_EPA_DATA_SHEETS) 

# Read in Building Intervention File                                                                                         
# when filling out the intervention excel sheet place % reductions in fuel typ, use negitive as an increase in load.
 

Interventions <- read_excel("data/Interventions.xlsx",skip = 16, na = "Not Available", sheet = 1)%>% 
  select(1:12) %>% 
  select( -6, -7, -8,-12) %>% 
  select(1, 4,2,3,5,6,7,8)
##### Reorder Interventions Here
Interventions <-Interventions %>% arrange(Order)


EUA_Elect <- EndUseAllocation[c("Building", "Elect", "Elect_use")]
EUA_Elect %>%  
  group_by(Building, Elect_use) %>%  
  summarise(Elect = sum(Elect)) %>% 
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


testfit_cols <- colnames(testfit1)


testfit1 %>% 
  select(1, 3, 10, 11:14) -> testfit1

# Bringing in Interventions. 
right_join(testfit1, Interventions, by = "Building", multiple = "all", relationship = "many-to-many") -> Savings

#pulled this out from above 

# For calculations of savings setting key categories to zero
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
#as.numeric(format(Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`, scientific = NA ) -> Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`)

rm(Savings)



##   9/23/2025 why is change in electric in savings measures a chr? 
##   Need a check on a measure being in interventions and the affected fule source does not exist. 
##   Need a loop to allow no electrificaiton measures to exist. 
##   For some reason I have a chr format now in change in elect feature. 
##
##
##
##   Need to account for a large tenant base load. For now say 60% of the lowest electric consumption (Base_E)
##   Period is the fixed base. The other base will be the building services base. Made 60% a variable
##   I can change at anytime, it is at line 120. Will need to build in a trigger for direct metered verses sub-metered here.  
##   However note that tenants will also have usage associated with temperature control and this may show
##   up in my interventions. Also I need to move to a base that factors on consumption per-workday. 
##   
##   Subtract 60% of Base_E from Elect_kBtu at every month, and remember this.
##   On command line 108 need to get into this data frame the 60% base i subtracted from Elect_kBTU
##   Make the Base_E column .6 * Base_E and carry this as the tenant base load. 
##
##   Need to debug the electrification table buld also I see ngas allocated to cooling need to correct that in the creation of the end use. 
##   Found that Base and cooling alone are in the wrong "Saved..." columns.  
##
##   
##
##
##   Run down Savings Measures tabulating savings from ratios manuallyto check. 


##    1/5/2026 Error below in the code Error in Savings_Measures$`Heating Loads`[i] * Savings_Measures$`Change in Electricity Consumption Reduction (kWh)`[i] : 
##    non-numeric argument to binary operator. 
##.   1/6/2026 the problme is on line 240 where i am changing the format from sci to num 


for(i in 1: length(Savings_Measures$Load)) { 
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
        
    
    # 10/31/2024 - put solar in as a key to sort on and in the interventin file place the AC kWh in the % file. 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
                    
    
  }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


# Moving over loads to Electrification Data Frame

nrow(Savings_Electrificaiton) -> b

#  Now make the electrification tables 

if(b != 0){

if(Savings_Measures$Load[1] == "NGas"){
filter(Savings_Measures, Load == "NGas")[nrow(filter(Savings_Measures, Load == "NGas")),4:6] -> Savings_Electrificaiton[1:(b/2),4:6]
}else{
filter(Savings_Measures, Load == "Steam_Mlb")[nrow(filter(Savings_Measures, Load == "Steam_Mlb")),4:6] -> Savings_Electrificaiton[1:(b/2),4:6]}

filter(Savings_Measures, Load == "Elect")[nrow(filter(Savings_Measures, Load == "Elect")),4:6] -> Savings_Electrificaiton[((b/2)+1):b,4:6]

Savings_Electrificaiton %>% filter(Load != "Elect") -> Savings_Electrificaiton



for(i in 1:length(Savings_Electrificaiton$Load)) { 
  if(Savings_Electrificaiton$Savings[i] == "Base" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
    Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Base[i];
    if(Savings_Electrificaiton$Saved_Base[i] != 0) {Savings_Electrificaiton$`Base Loads`[i] <- Savings_Electrificaiton$`Base Loads`[i] + Savings_Electrificaiton$Saved_Base[i]};
  }else{
    
    if(Savings_Electrificaiton$Savings[i] == "Base" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
      Savings_Electrificaiton$`Base Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Base[i];
      if (i < length(Savings_Electrificaiton$Load)) {
      if(Savings_Electrificaiton$Order[i] == Savings_Electrificaiton$Order[i+1]){Savings_Electrificaiton$`Base Loads`[i+1] <- Savings_Electrificaiton$`Base Loads`[i] + Savings_Electrificaiton$Saved_Base[i]} else {
      if(Savings_Electrificaiton$Saved_Base[i] != 0 & i < length(Savings_Electrificaiton$Load) +1 ) { Savings_Electrificaiton$`Base Loads`[i+1] <- Savings_Electrificaiton$`Base Loads`[i] + Savings_Electrificaiton$Saved_Base[i]}}};
   }else{
    
    if(Savings_Electrificaiton$Savings[i] == "Cooling" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
      Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
      ifelse(Savings_Electrificaiton$Saved_Cooling[i] == 0, Savings_Electrificaiton$`Cooling Loads`[i] , Savings_Electrificaiton$`Cooling Loads`[i] <- Savings_Electrificaiton$`Cooling Loads`[i]  + Savings_Electrificaiton$Saved_Cooling[i]);
    }else{
      
      if(Savings_Electrificaiton$Savings[i] == "Cooling" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
        Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
        if (i < length(Savings_Electrificaiton$Load)) {
        ifelse(Savings_Electrificaiton$Saved_Cooling[i] == 0, Savings_Electrificaiton$`Cooling Loads`[i] , Savings_Electrificaiton$`Cooling Loads`[i] <- Savings_Electrificaiton$`Cooling Loads`[i]  + Savings_Electrificaiton$Saved_Cooling[i]);
        if(Savings_Electrificaiton$Saved_Cooling[i] != 0 ) { Savings_Electrificaiton$`Cooling Loads`[i+1] <- Savings_Electrificaiton$`Cooling Loads`[i] + Savings_Electrificaiton$Saved_Cooling[i]}};
     }else{
      
      if(Savings_Electrificaiton$Savings[i] == "Heating" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
        Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Heating[i];
        ifelse(Savings_Electrificaiton$Saved_Heating[i] == 0, Savings_Electrificaiton$`Heating Loads`[i] , Savings_Electrificaiton$`Heating Loads`[i] <- Savings_Electrificaiton$`Heating Loads`[i]  + Savings_Electrificaiton$Saved_Heating[i]);
      }else{
        
        if(Savings_Electrificaiton$Savings[i] == "Heating" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
          Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Heating[i];
          Savings_Electrificaiton$`Heating Loads` <- Savings_Electrificaiton$`Heating Loads`[i] - Savings_Electrificaiton$Saved_Heating[i];
          if (i < length(Savings_Electrificaiton$Load)) {
          ifelse(Savings_Electrificaiton$Saved_Heating[i] == 0, Savings_Electrificaiton$`Heating Loads`[i] , Savings_Electrificaiton$`Heating Loads`[i] <- Savings_Electrificaiton$`Heating Loads`[i]  + Savings_Electrificaiton$Saved_Heating[i]);
          if(Savings_Electrificaiton$Saved_Heating[i] != 0 ) { Savings_Electrificaiton$`Heating Loads`[i+1] <- Savings_Electrificaiton$`Heating Loads`[i] + Savings_Electrificaiton$Saved_Heating[i]}};
            }else{
        
        if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling" & Savings_Electrificaiton$Load[i] == "Steam_Mlb" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
          Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Heating[i];
          Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
          ifelse(Savings_Electrificaiton$Saved_Heating[i] == 0, Savings_Electrificaiton$`Heating Loads`[i] , Savings_Electrificaiton$`Heating Loads`[i] <- Savings_Electrificaiton$`Heating Loads`[i]  + Savings_Electrificaiton$Saved_Heating[i]);
          ifelse(Savings_Electrificaiton$Saved_Cooling[i] == 0, Savings_Electrificaiton$`Cooling Loads`[i] , Savings_Electrificaiton$`Cooling Loads`[i] <- Savings_Electrificaiton$`Cooling Loads`[i]  + Savings_Electrificaiton$Saved_Cooling[i]);
        }else{
          
          if(Savings_Electrificaiton$Savings[i] == "Heating & Cooling" & Savings_Electrificaiton$Load[i] == "NGas" & Savings_Electrificaiton$`Description of Measure`[i] == "Electrificaiton"  )  {
            Savings_Electrificaiton$`Heating Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Heating[i];
            Savings_Electrificaiton$`Cooling Loads`[i] * Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i] -> Savings_Electrificaiton$Saved_Cooling[i];
            if (i < length(Savings_Electrificaiton$Load)) {
            ifelse(Savings_Electrificaiton$Saved_Heating[i] == 0, Savings_Electrificaiton$`Heating Loads`[i] , Savings_Electrificaiton$`Heating Loads`[i] <- Savings_Electrificaiton$`Heating Loads`[i]  + Savings_Electrificaiton$Saved_Heating[i]);
            ifelse(Savings_Electrificaiton$Saved_Cooling[i] == 0, Savings_Electrificaiton$`Cooling Loads`[i] , Savings_Electrificaiton$`Cooling Loads`[i] <- Savings_Electrificaiton$`Cooling Loads`[i]  + Savings_Electrificaiton$Saved_Cooling[i])};
            
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
              if (i < length(Savings_Electrificaiton$Load)) {
              if(Savings_Electrificaiton$Order[i] == Savings_Electrificaiton$Order[i+1]) {Savings_Electrificaiton$`Base Loads`[i+1] <- Savings_Electrificaiton$`Base Loads`[i] + Savings_Electrificaiton$Saved_Base[i]} else {
              if(Savings_Electrificaiton$Saved_Base[i] != 0 & i < length(Savings_Electrificaiton$Load)+1){Savings_Electrificaiton$`Base Loads`[i+1] <- Savings_Electrificaiton$`Base Loads`[i] + Savings_Electrificaiton$Saved_Base[i]}}};
      
                    }}}}}}}}}}}


Savings_Electrificaiton  %>%
  mutate("Saved" = Saved_Base + Saved_Cooling + Saved_Heating) %>% 
  select(-c(4,5,6,8,14,15,16)) -> Savings_Electrificaiton 



if(Savings_Measures$Load[1] == "NGas"){
  filter(Savings_Electrificaiton,  Load == "NGas") -> Savings_Electrificaiton 
}else{
  filter(Savings_Electrificaiton,  Load == "Steam_Mlb") -> Savings_Electrificaiton }


for (i in 1:nrow(Savings_Electrificaiton)) { 
if(Savings_Electrificaiton$Load[i] == "Steam_Mlb") {Savings_Electrificaiton$Saved[i] / 1194 -> Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i];
  Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- ( 0.29307 * Savings_Electrificaiton$`Change in Steam Consumption, kLbs`[i] / Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i]);
}else{ 
  if(Savings_Electrificaiton$Load[i] == "NGas") {Savings_Electrificaiton$Saved[i]/1000 -> Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i];
    Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- 293.07 * (Savings_Electrificaiton$`Change in Natural Gas Use(MMBtu)`[i])/ Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i];
  }else{   Savings_Electrificaiton$`Change in Electricity Consumption Reduction (kWh)`[i] <- 0.29307 * Savings_Electrificaiton$Saved[i]  }
}}
  
}

#
#
#    STOP HERE 1/27/2026
#
#



# This is where we jump down to if there is no lines in the Savings_Electrification table. 

# Move savings to native innervation columns for Measures  
  
  Savings_Measures  %>%
    mutate("Saved" = Saved_Base + Saved_Cooling + Saved_Heating)  -> Savings_Measures
#  select(-c(4,5,6,8,14,15,16)) -> Savings_Measures (this is what the line above was before 1/27/2026)
  
  
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
  
  #################################################################################################################
  #
  #
  #
  #  Need plan and code for when there is no electrification strategies 
  #  I think this block of code is for buildings with 2 load types so i assumed i needed to double up the change in kWh
  #  Why is cooling being assigned to oil in case where oil is delivered in summer? Beause I make assigments by date of fuel delivery.
  # *******************************************************************************
  # *******************************************************************************
  #################################################################################################################
  
  
  # Moving Electric up 
  Savings_Measures %>% 
    filter( Load == "Elect") %>% 
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
  
  remove(change_kWH, Savings_Electrificaiton, Savings_Measures, b, fuels, i, loads, Interventions, rows)
  
  
  write.csv(Savings, "data/Savings.csv" )
  
  