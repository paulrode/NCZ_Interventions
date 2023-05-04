
# Load packages 

my_packages <- c("tidyverse", "vroom" , "janitor" , "glue" , "tsibble" , "tidytext","lubridate", "fable", "tsibbledata", "ggplot2", "forecast", "tseries", "rio", "zoo", "readxl", 
                 "tsibbledata", "knitr", "formattable", "scales", "tidyr" ,"kableExtra", "dplyr", "gridExtra", "writexl")   

# Took these out but need to keep them front of mind "timetk" , "recipes" 

invisible( lapply(my_packages, require, character.only = TRUE))

#Set up environment 
`%notin%` <- Negate(`%in%`)
 place <- "Home"  #Where are we working today. 
# place <- "work"
if (place == "Home"){setwd("C:/Users/paulr/Documents/R/NCZ_Interventions")} else {setwd("C:/Users/prode/OneDrive - Tishman Speyer/Documents/R/NCZ_Interventions")}
if (!file.exists("data")) { dir.create("data")}
rm(place, my_packages ) #Clean up
options(dplyr.summarise.inform = FALSE)  # Suppress text in Knit printout. 

#
# This script will push out to interventions. 
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
   filter(DateY > 2017 & DateY < 2020) %>% 
   select(1,10,9, 3:8) -> TSUS_EPA_DATA_SHORT_ALL
 remove("TSUS_EPA_DATA", "TSUS_EPA_DATA_LONG", "TSUS_EPA_DATA_LONG_ALL", i )
 
 
 TSUS_EPA_DATA_SHORT_ALL %>% 
   group_by(Building, DateM) %>% 
   summarise(Elect_kBTU = sum(`Electric - Grid\r\n(kBtu)`/2, na.rm=TRUE),
             NGas_kbtu = sum(`Natural Gas\r\n(kBtu)`/2, na.rm=TRUE),
             Steam_btu = sum(`District Steam\r\n(kBtu)`/2, na.rm=TRUE),
             Oil2_btu = sum(`Fuel Oil (No. 2)\r\n(kBtu)`/2, na.rm=TRUE),
             Oil4_btu = sum(`Fuel Oil (No. 4)\r\n(kBtu)`/2, na.rm=TRUE),
             Diesel_btu = sum(`Diesel\r\n(kBtu)`/2, na.rm=TRUE)) -> TSUS_EPA_DATA_SHORT_ALL
 TSUS_EPA_DATA_SHORT_ALL[is.na(TSUS_EPA_DATA_SHORT_ALL)] = 0
 TSUS_EPA_DATA_SHORT_ALL %>% 
     mutate(Total_btu = Elect_kBTU + NGas_kbtu + Steam_btu + Oil2_btu + Oil4_btu + Diesel_btu) -> TSUS_EPA_DATA_SHORT_ALL

TSUS_EPA_DATA_SHORT_ALL %>% 
  group_by(Building) %>% 
  summarise(DateM, Elect_kBTU, NGas_kbtu, Steam_btu, Oil2_btu, Oil4_btu, Diesel_btu, Total_btu,"Base" = min(Total_btu)) -> TSUS_EPA_DATA_SHORT_ALL

TSUS_EPA_DATA_SHORT_ALL %>% 
  mutate(use = ifelse(Base == Total_btu, "Base Loads", ifelse(DateM %in% c(1,2,3,11,12,10), "Heating Loads", "Cooling Loads"))) -> TSUS_EPA_DATA_SHORT_ALL

TSUS_EPA_DATA_SHORT_ALL %>% 
  group_by(Building, use) %>% 
  summarise(Elect = sum(Elect_kBTU), NGas = sum(NGas_kbtu), Steam = sum(Steam_btu), Oil2 = sum(Oil2_btu), Oil4 = sum(Oil4_btu), Diesel = sum(Diesel_btu), Total = sum(Total_btu)) %>% 
  mutate("Elect_kWH" = Elect/3.418, "Steam_Mlb" = Steam/1194) %>% 
  select(Building, use, Elect_kWH, Steam_Mlb, Elect, NGas, Steam, Oil2, Oil4, Diesel, Total) -> EndUseAllocation




#Buildings with missing data not making it though analysis. 
(TSUS_EPA_DATA_SHEETS[TSUS_EPA_DATA_SHEETS %notin% unique(TSUS_EPA_DATA_SHORT_ALL$Building)] )
#"1395 Crossman"   "66 Hudson Blvd."


# Make interventions table in excel and read here, then pull in EndUseAllocations 



# Make a building data file and input here. Ease building will have sf for ratioing costs and a configuration code
#Building Data File 
BuildingData <- read_excel("data/BuildingData.xlsx", na = "Not Available", sheet = 1)

#Join BuildingData with EndUseAllocation 
left_join(EndUseAllocation, BuildingData, by = "Building") -> EndUseAllocation

#Building Configuration File 
#data.frame(Building = unique(EndUseAllocation$Building),  Heating = rep("type", 67), Cooling = rep("type", 67), DomesticHotWater = rep("type", 67),CoolingTower = rep("type", 67) ) -> BuildingConfiguration
# write.csv(EndUseAllocation, "C:/Users/prode/OneDrive - Tishman Speyer/Documents/R/NCZ_Interventions/data/EndUseAllocation.csv")




remove(BuildingData, TSUS_EPA_DATA_SHORT_ALL, TSUS_EPA_DATA_SHEETS) 


#Building Intervention File 
Interventions <- read_excel("data/Interventions_One_Federal_source.xlsx",skip = 16, na = "Not Available", sheet = 1)%>% 
  select(1:12) %>% 
  select( -6, -7, -8,-12) %>% 
  select(1, 4,2,3,5,6,7,8)
##### Reorder Interventions Here
Interventions <-Interventions %>% arrange(Order)


#apply(TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)], 2, function(row) as.numeric(row)) -> TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)]



EndUseAllocation %>% 
  select(-5, -7, -11, -12,-14,-15) %>% 
  select(1,2,9,3:8) -> EndUseAllocation

EndUseAllocation %>% 
  gather(key = "Load", value = value, 4:9) %>% 
  filter(value != 0 ) %>% 
  spread(key=use, value=value, fill = 0) -> testfit1

length(unique(EndUseAllocation$Building))
unique(EndUseAllocation$Building)

# Joining Interventions Here To Savings

right_join(testfit1, Interventions, by = "Building") %>% 
  na.omit() -> Savings


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

for(i in 1:length(Savings$Load)) { 
  if(Savings$Savings[i] == "Base" & Savings$Load[i] == "Elect_kWH" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Base Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Base[i]
    }else{ 
    
  if(Savings$Savings[i] == "Base" & Savings$Load[i] == "Steam_Mlb" & Savings$`Intervention Name`[i] != "Electrification") {Savings$`Base Loads`[i] * Savings$`Change in Steam Consumption, kLbs`[i] -> Savings$Saved_Base[i]
    }else{ 
  
  if(Savings$Savings[i] == "Cooling" & Savings$Load[i] == "Elect_kWH" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Cooling[i]
  }else{
    
  if(Savings$Savings[i] == "Cooling" & Savings$Load[i] == "Steam_Mlb" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Cooling[i]
  }else{
  
  if(Savings$Savings[i] == "Heating" & Savings$Load[i] == "Elect_kWH" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Heating[i]
  }else{
  if(Savings$Savings[i] == "Heating" & Savings$Load[i] == "Steam_Mlb" & Savings$`Intervention Name`[i] != "Electrification" )  {Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Heating[i]
  }else{
  if(Savings$Savings[i] == "Heating & Cooling" & Savings$Load[i] == "Elect_kWH" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Heating[i]
    Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Cooling[i]
  }else{
  if(Savings$Savings[i] == "Heating & Cooling" & Savings$Load[i] == "Steam_Mlb" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Heating[i]
      Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Cooling[i]
  }else{
  if(Savings$Savings[i] == "Heating & Cooling & Base" & Savings$Load[i] == "Elect_kWH" & Savings$`Intervention Name`[i] != "Electrification")  {Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Heating[i]
    Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Cooling[i]
    Savings$`Base Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Base[i]
  }else{(Savings$Savings[i] == "Heating & Cooling & Base" & Savings$Load[i] == "Steam_Mlb" & Savings$`Intervention Name`[i] != "Electrification"){Savings$`Heating Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i] -> Savings$Saved_Heating[i]
      Savings$`Cooling Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Cooling[i]
      Savings$`Base Loads`[i] * Savings$`Change in Electricity Consumption Reduction (kWh)`[i]  -> Savings$Saved_Base[i]}
                      }}}}}}}}}}
      
      if (Savings$Load[i] == Savings$Load[i+1] & i< length(Savings$Load))  {Savings$`Base Loads`[i] - Savings$Saved_Base[i] -> Savings$`Base Loads`[i+1]
        if (Savings$Load[i] == Savings$Load[i+1] & i< length(Savings$Load))  {Savings$`Cooling Loads`[i] - Savings$Saved_Cooling[i] -> Savings$`Cooling Loads`[i+1]  
          if (Savings$Load[i] == Savings$Load[i+1] & i< length(Savings$Load))  {Savings$`Heating Loads`[i] - Savings$Saved_Heating[i] -> Savings$`Heating Loads`[i+1]   
          }}}





# how about separate out the electrification measures then perform the reductions in savings and then 
# bringing back the electrification and try to zero it out. 




#  
# Combinations: 

# Load = Steam & Savings = heating  -> steam heat to electric heat
# Use COP to differentiate electric coils, or heat pumps. put COP of electric heat in change in electric and zero out steam heat in heating loads.  

# Load = steam & Savings = Cooling -> steam cooling to electric cooling
#put COP of electric heat in change in electric and zero out steam cooling in cooling loads 

# Load = steam & Savings = Base -> steam DHW to electric DHW 
#put COP of electric heat in change in electric and zero out steam loads in base. 

# Load = gas & Savings = Heating -> gas boilers to electric heat
#put COP of electric heat in electric and zero out gas heating

# Load = gas & Savings = Cooling -> gas chillers to electric cooling
#put COP of electric heat in electric and zero out gas cooling 

# Load = gas & Savings = Base -> gas boilers for DHW to electric DHW
#put COP of electric heat in electric and zero out steam 

