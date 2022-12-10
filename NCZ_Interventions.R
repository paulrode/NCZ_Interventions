
# Load packages 

my_packages <- c("tidyverse", "vroom" , "timetk", "janitor" , "glue" , "tsibble" , "tidytext","lubridate", "fable", "tsibbledata", "ggplot2", "forecast", "tseries", "rio", "zoo", "readxl", "tsibbledata", "knitr", "formattable", "scales", "kable", "kableExtra", "dplyr", "gridExtra")   

invisible( lapply(my_packages, require, character.only = TRUE))

#Set up environment 
place <- "Home"  #Where are we working today. 
# place <- "work"
if (place == "Home"){setwd("C:/Users/paulr/Documents/R/NCZ_Interventions")} else {setwd("C:/Users/prode/Documents/R/ElectricSupplyOptions")}
if (!file.exists("data")) { dir.create("data")}
rm(place, my_packages ) #Clean up
options(dplyr.summarise.inform = FALSE)  # Suppress text in Knit printout. 



# Read in data

# Import first sheet
TSUS_EPA_DATA_Long_ALL <- data.frame("Month", )
TSUS_EPA_DATA_SHEETS <-excel_sheets("data/Energy Star_Energy Use by Calendar Month_US Properties.xlsx")
x <- 1
"TSUS_EPA_DATA" <- read_excel("data/Energy Star_Energy Use by Calendar Month_US Properties.xlsx",skip = 5, na = "Not Available", sheet = x)
apply(TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)], 2, function(row) as.numeric(row)) -> TSUS_EPA_DATA[2:ncol(TSUS_EPA_DATA)]
gather(TSUS_EPA_DATA, key = "CarbonSource", value = "Value", -Month) %>% 
mutate(Building = TSUS_EPA_DATA_SHEETS[x])-> TSUS_EPA_DATA_LONG
TSUS_EPA_DATA_LONG$Month <-  my(TSUS_EPA_DATA_LONG$Month)
TSUS_EPA_DATA_Long_ALL <- TSUS_EPA_DATA_LONG
# Get rest of sheets in






Comp520a <- Comp520a[complete.cases(Comp520a),] 

Calpine_Shaped_Block  <- read_excel("C:/Users/paulr/Documents/R/ElectricSupplyOptions/Calpine Pricing 11.1.2022.xlsx", sheet = "NY", range = "B3:K10" )

Calpine_Seasonal_Blocks  <- read_excel("C:/Users/paulr/Documents/R/ElectricSupplyOptions/Calpine Pricing 11.1.2022.xlsx", sheet = "NY", range = "B13:K20" )

Comp520 %>%
  group_by(y = year(TimeStamp), m = month(TimeStamp),d = day(TimeStamp), h = hour(TimeStamp)) %>% 
  summarise( "Elect" = sum(Elect)) %>% 
  mutate(TimeStamp = paste(y, m, d, sep = '-')) %>% 
  mutate(TimeStamp = paste( TimeStamp, h, sep = " ")) %>% 
  mutate(TimeStamp = paste( TimeStamp, ':00:00', sep = "")) %>% 
  mutate(TimeStamp = ymd_hms(TimeStamp)) -> Comp520

Comp520[c(5:6)] -> Comp520

rm(Comp520a, Comp520b, Comp520c ) #Clean up

inner_join(Comp520, NYSISO_data, by = "TimeStamp") %>% 
  na.omit() -> ISOCOMP_data
ISOCOMP_data <- unique(ISOCOMP_data)
ISOCOMP_data %>% 
  select(c(2,1,6)) -> ISOCOMP_data
colnames(ISOCOMP_data) <- c("TimeStamp", "Energy", "LBMP")



# Put scenario here

ISOCOMP_data %>% 
  mutate('UnitCost50_50' = (0.5 * LBMP) + (0.5 * 83.13)) %>% 
  mutate('UnitCostFixed' = 81.80 ) %>%
  mutate('HourlyCostFixed' = (Energy * UnitCostFixed / 1000) ) %>% 
  mutate('HourlyCost50_50' = (Energy * UnitCost50_50 / 1000 ) ) %>% 
  group_by(month(TimeStamp)) %>% 
  summarise('EnergyIndexCost' = sum(LBMP), 'Monthly_kWh' = sum(Energy), 'EnergyCost50_50' = sum(HourlyCost50_50), 'EnergyCostFixed' = sum(HourlyCostFixed)) %>% 
  mutate('EnergyIndexUnit' = EnergyIndexCost / Monthly_kWh,  'EnergyUnit50_50' = EnergyCost50_50 / Monthly_kWh, 'EnergyUnitFixed' = EnergyCostFixed / Monthly_kWh) -> M_ISOCOMP_data


M_ISOCOMP_data %>% 
  summarise('CostIndex' = sum(EnergyIndexCost), 'Cost50_50' = sum(EnergyCost50_50), 'CostFixed' = sum(EnergyCostFixed)) -> Annual_Cost_Estimates


```

The electric energy market in New York include energy and ancillaries with the primary market by volumn and value being the Locational Based Marginal Price (LBMP) Hourly Day Ahead market. Other markets are the Spot, Installed Capacity (ICAP) and Congestion.

Electric market participants are in wholesale electric markets to manage risk of short term price spikes. Figures 1 and 2 below show the hourly LBMP going back to 2009 and a focus on the last few months. The blue line is a linear model to show the overall trend direction, which has been decreasing for the last decade. In 2020 we see the trend changing with more upward volatility as Figure 2 shows better. For reference the 2014 polar vortex induced price spike can be see in Figure 1.

Winter and summer peaks can be seen in Figure 3 which shows the average monthly prie per MWH. Also as expected the lowest average montly costs is during the swing seasons spring and fall.

<u>Figure 1</u>   Plot of hourly day ahead LBMP over time. The blue line shows a linear model and is used to show the general trend over the past decade. However the last 18 months has seen an increase. If this is lasting is yet to be seen.  

<u>Figure 2</u> This plot show the most reasent few months hourly day ahead LBMP plot. Demonstrating that the declining market has changed direction. 

<u>Figure 3</u> This plot show the average cost per month and show us where the highest and lowest priced months are for informing when to exercise energy efficiency exercises or when to puchase fixed price blocks. 

<u>Figure 4</u> The box chart show more accurately the pricing distribution each month, interesting to see that the 2014 polar vortex did not effect the mean pricing that much. 

<u>Figure 4a</u> The box chart show more accurately the pricing distribution each month, interesting to see that the 2014 polar vortex did not effect the mean pricing that much. 


<u>Figure 6</u> This bar chart show the means in a more readeable scale. These are the same means shown in the box charts. 

<u>Figure 7</u> This chart Show the pricing distributions for each year, again demonstrating the general trend of pricing. Note that declining markets favor variable strategies, and increasing strategies favor fixed stratiges. 

<u>Figure 8</u> This chart show the horuly day ahead pricing superimposed with the hourly consumption data from 520 Madison with its electric cooling plant and ice storage system. 

<u>Figure 9</u>  The most important chart in the set, this bar chart show a comparison of the 3 strategies under consideration fixed, 50/50 which meand 50% of each day's consumption will be purchased on the variable index - the LBMP, the remaining 50% off a fixed cost block contract, and finaly a full variable or index contract. We see that the lowest cost strategy is the variable and the highest cost is the full fixed contract. 

<u>Figure 10</u> This chart shows the annual performance of the three strategies for 2023. 

<u>Figure 11</u> This chart show the effect on annual performance of the three strategies given a 2014 like polar vortex. 

<u>Figure 12</u> This chart show the effect on annual performance of the three strategies given a recession induced declining energy market. 

<u>Table 1</u> Comparision  of the projected spend for 2023 compaired to a year with a 2014 like polar vortex, or a 2009 like recession. 




```{r Market and Building Plots, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}
# Plot data 


#Figure 1: LBMP trend over time
NYSISO_data %>% 
  ggplot(aes(x = `TimeStamp`, y = `DAM Zonal LBMP`)) +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x, show.legend = TRUE, inherit.aes = TRUE, ) +
  scale_x_datetime() +
  theme_classic() +
  labs(x = "Date (Year)", y = "Day Ahead Hourly LBMP ($/MWH)", title = "Figure 1:" ~underline("NYSISO LBMP 2009 - Present"), caption = "ISO Market data from: https://www.nyiso.com/custom-reports" ) +
  geom_segment(aes(x = as.POSIXct("2020-05-29 07:00:00"), y = 5, xend = as.POSIXct("2020-05-29 07:00:00"), yend =300 ),  linewidth = 100, linejoin =  "round")  +
  geom_segment(aes(x = as.POSIXct("2022-11-12 07:00:00"), y = 5, xend = as.POSIXct("2022-11-12 07:00:00"), yend =300 ),  linewidth = 100, linejoin =  "round") +
  geom_segment(aes(x = as.POSIXct("2020-05-29 07:00:00"), y = 300, xend = as.POSIXct("2022-11-12 07:00:00"), yend =300 ),  linewidth = 100, linejoin =  "round") +
  geom_segment(aes(x = as.POSIXct("2020-05-29 07:00:00"), y = 5, xend = as.POSIXct("2022-11-12 07:00:00"), yend =5 ),  linewidth = 100, linejoin = "round") 




#Figure 2: LBMP trend over time
NYSISO_data %>% 
  filter(year(TimeStamp) > 2020) %>%
  ggplot(aes(x = `TimeStamp`, y = `DAM Zonal LBMP`)) +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x, show.legend = TRUE, inherit.aes = TRUE, ) +
  scale_x_datetime(breaks = date_breaks("3 months")) +
  theme_classic() +
  labs(x = "Date (Year)", y = "Day Ahead Hourly LBMP ($/MWH)", title = "Figure 2:" ~underline("NYSISO LBMP Zooming in on last 24 months"), caption = "ISO Market data from: https://www.nyiso.com/custom-reports" ) 



#    FIGURE 3 - LBMP Monthly Average Costs 
#NYSISO_data %>% 
#  mutate('Year' = year(TimeStamp), 'Month' = month(TimeStamp)) %>% 
#  group_by(Year, Month) %>% 
#  mutate('Year-Month' = paste(as.character(Year),as.character(Month), sep = '-')) %>% 
#  summarise(LBMP_Avr = mean(`DAM Zonal LBMP`))  -> NYSISO_data_a
#NYSISO_data_a %>% 
#  mutate('ym' = ym(paste(Year, Month, sep = '-'))) -> NYSISO_data_a
#NYSISO_data_a %>%  
# ggplot(aes(x = ym , y = LBMP_Avr)) +
#  geom_col() +
#  stat_smooth(method = "lm", formula = y ~ x, show.legend = TRUE, inherit.aes = TRUE, ) +
#  scale_x_date(labels = date_format("%Y"), breaks = "1 year",   minor_breaks = waiver()) +
#  theme(axis.text.x = element_text(angle=60, vjust = 0.5)) +
#  labs(x = "Date (Year - Month)", y = "Monthly Average Day Ahead Hourly LBMP ($/MWH)", title = "Figure 3:" ~underline("Monthly Average Costs LBMP"), caption = "ISO Market data from: https://www.nyiso.com/custom-reports" )
#rm(NYSISO_data_a)
```

Figure 3 shows the distribution of LBMP by month over all years 2009 to present. Figure 4 shows the mean vales by month over the same time period. The takeaway here is the priminany of the winter months January and Fenuary and the summer July and August as triditionally peak cost months.

```{r, Average LBMP Plots figures-side, fig.show="hold", out.width="50%",  echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

par(mfrow=c(2,2))

#Table 3
NYSISO_data %>% 
  mutate('Month' =as.factor(month(TimeStamp))) %>% 
  ggplot(aes(x = Month, y = `DAM Zonal LBMP`)) +
  geom_boxplot( coef = 1.0) +
  scale_x_discrete(labels=month.abb) +
  labs(x = "Date (Month)", y = "Average LBMP ($/MWH)", title = "Figure 3:" ~underline("Box and Whisker Historical Average LBMP ($/MWH)") )


#Table 4  
NYSISO_data %>% 
  filter(hour(TimeStamp) <=20 & hour(TimeStamp) >= 7 ) %>% 
  mutate('Month' =as.factor(month(TimeStamp))) %>% 
  ggplot(aes(x = Month, y = `DAM Zonal LBMP`)) +
  geom_boxplot( coef = 1.0) +
  scale_x_discrete(labels=month.abb) +
  labs(x = "Date (Month)", y = "Average LBMP ($/MWH)", title = "Figure 4:" ~underline("Box and Whisker Historical Average LBMP over BUILDING BUSINESS HOURS ($/MWH)") )
#Look at historical average monthly pricing  


#Table 5
NYSISO_data %>% 
  mutate('Month' =as.factor(month(TimeStamp))) %>% 
  ggplot(aes(x = Month, y = `DAM Zonal LBMP`)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE, fill = "grey") +
  scale_x_discrete(labels=month.abb) +
  theme_classic() +
  labs(x = "Date (Month)", y = "Average LBMP ($/MWH)", title = "Figure 5:" ~underline("Historical Average LBMP ($/MWH)") )


# Table 6 Yearly averages 
NYSISO_data %>% 
  filter(hour(TimeStamp) <=20 & hour(TimeStamp) >= 7 ) %>% 
  mutate('Month' =as.factor(month(TimeStamp))) %>% 
  ggplot(aes(x = Month, y = `DAM Zonal LBMP`)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE, fill = "grey") +
  scale_x_discrete(labels=month.abb) +
  theme_classic() +
  labs(x = "Date (Month)", y = "Average LBMP ($/MWH)", title = "Figure 6:" ~underline("Business Hours Average LBMP ($/MWH)") )

```

Figure six shows the annual distribution of LBMP for each year 2009 to present. We see the polar vortex had little influance on the mean cost for 2014 but did infact have very high short terms costs.

```{r Building Plots, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

##Figure 7 Yearly averages 
NYSISO_data %>% 
  mutate('Year' =as.factor(year(TimeStamp))) %>% 
  ggplot(aes(x = Year, y = `DAM Zonal LBMP`)) +
  geom_boxplot( coef = 1.0) +
  scale_x_discrete() +
  labs(x = "Year", y = "Annual Average LBMP ($/MWH)", title = "Figure 7:" ~underline("Box and Whisker Average Annual LBMP ($/MWH)") ) -> Fugire7


```


```{r Data Production, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

#Table 8
ISOCOMP_data %>% 
  mutate(LBMP = LBMP *4)  %>% 
  gather("Item", "Value", -TimeStamp) %>% 
  ggplot(aes(x = `TimeStamp`, y = Value)) +
  geom_line(aes(color = Item)) +
  scale_color_manual(values=c('grey','orange'), name = "Plot", labels = c("Comsumption", "xLBMP")) +
  labs(x = "TimeStamp", y = "Hourly LBMP ($/MWH) & Comsumption (kWh)", title = "Figure 8:" ~underline("Relationship of Comsumption to LBMP"), caption = "ISO Market data from: https://www.nyiso.com/custom-reports & Aquacore Database")


#Figure 9 Comaprision of strategies by month. 
M_ISOCOMP_data[, -c(3,6,7,8)] %>% 
  gather(key= 'Strategy', value = 'Value', -`month(TimeStamp)`) -> MonthlyComparision
MonthlyComparision$Strategy <- factor(MonthlyComparision$Strategy)
MonthlyComparision$`month(TimeStamp)` <- month.abb[MonthlyComparision$`month(TimeStamp)`]

MonthlyComparision %>% 
  ggplot(aes(x = `month(TimeStamp)`, y = Value)) +
  geom_col(aes(fill = Strategy), position = "dodge") +
  theme_classic() +
  scale_fill_discrete(name = "Strategy", labels = c("50 / 50", "Fixed", "Index")) +
  labs(x = "Month", y = "Monthly Cost($)", title = "Figure 9:" ~underline("Comparision of Strategies by month")) -> Figure9
```  

```{r Annual Summeries, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}



Annual_Cost_Estimates %>% 
  gather(key = 'Strategy', value = 'Value' ) -> Annual_Cost_Estimates
gsub('Cost*', '', Annual_Cost_Estimates$Strategy) -> Annual_Cost_Estimates$Strategy
Annual_Cost_Estimates$Strategy <- factor(Annual_Cost_Estimates$Strategy, levels = Annual_Cost_Estimates$Strategy[order(Annual_Cost_Estimates$Value, decreasing = TRUE)]) 

Annual_Cost_Estimates %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  theme_classic() +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 10:" ~underline("Comparision of Strategies")) -> Figure10

#Plot for side by side 
Annual_Cost_Estimates %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle=60, vjust = 0.5) ) +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 10:" ~underline("2023")) -> Figure10a


# Work to make a table of the strategies 



# What is this for????
M_ISOCOMP_data$`month(TimeStamp)` <-  month.name[M_ISOCOMP_data$`month(TimeStamp)`]
M_ISOCOMP_data %>% 
  select(1,6,7,8) %>% 
  adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "Mean") -> M_ISOCOMP_data
M_ISOCOMP_data[13,c(2:4)] <- M_ISOCOMP_data[13,c(2:4)] / 12






kable(Annual_Cost_Estimates, format = 'html',
      col.names = c("Strategy", "Annual Estimate"),
      digits = 0,
      align = 'll',
      caption = "Table 1: Annual Estimates of spend by strategy") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> Figure10aTable 







kable(M_ISOCOMP_data, format = 'html',  
      col.names = c("Month", "Index", "Energy", "Fixed"),
      digits = 3, 
      align = 'lccr',
      caption = "Table 2: Monthly Unit Cost of Electricy Supply ($/kWH)") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> MonthUnitCost


```

Sensitivity with a polar vortex similar to the 2014 spike. 

```{r Polar Vortex, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

#Make more analysis to 2014 the polar vortex year. 
R <- 1
PV <- 1.5

ISOCOMP_data_Run <- ISOCOMP_data

# Multiply Jan Feb by PV defined above in this section. 
ISOCOMP_data_Run %>% 
  mutate_at(vars(c('LBMP')), ~ifelse((month(TimeStamp) == 01 | month(TimeStamp) == 02) , LBMP * PV, LBMP )) -> ISOCOMP_data_Run


ISOCOMP_data_Run %>% 
  mutate(LBMP = LBMP * R) %>% 
  mutate('UnitCost50_50' = (0.5 * LBMP) + (0.5 * 83.13)) %>% 
  mutate('UnitCostFixed' = 81.80 ) %>%
  mutate('HourlyCostFixed' = (Energy * UnitCostFixed / 1000) ) %>% 
  mutate('HourlyCost50_50' = (Energy * UnitCost50_50 / 1000 ) ) %>% 
  group_by(month(TimeStamp)) %>% 
  summarise('EnergyIndexCost' = sum(LBMP), 'Monthly_kWh' = sum(Energy), 'EnergyCost50_50' = sum(HourlyCost50_50), 'EnergyCostFixed' = sum(HourlyCostFixed)) %>% 
  mutate('EnergyIndexUnit' = EnergyIndexCost / Monthly_kWh,  'EnergyUnit50_50' = EnergyCost50_50 / Monthly_kWh, 'EnergyUnitFixed' = EnergyCostFixed / Monthly_kWh) -> M_ISOCOMP_data_Run


M_ISOCOMP_data_Run %>% 
  summarise('CostIndex' = sum(EnergyIndexCost), 'Cost50_50' = sum(EnergyCost50_50), 'CostFixed' = sum(EnergyCostFixed)) -> Annual_Cost_Estimates_Run



Annual_Cost_Estimates_Run %>% 
  gather(key = 'Strategy', value = 'Value' ) -> Annual_Cost_Estimates_Run
gsub('Cost*', '', Annual_Cost_Estimates_Run$Strategy) -> Annual_Cost_Estimates_Run$Strategy
Annual_Cost_Estimates_Run$Strategy <- factor(Annual_Cost_Estimates_Run$Strategy, levels = Annual_Cost_Estimates_Run$Strategy[order(Annual_Cost_Estimates_Run$Value, decreasing = TRUE)])

#Figure 11                                         
Annual_Cost_Estimates_Run %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  theme_classic() +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 11:" ~underline("Comparision of Strategies under a Polar Vortex Hit")) -> Figure11


#Figure11 for side by side plots 
Annual_Cost_Estimates_Run %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle=60, vjust = 0.5) ) +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 11:" ~underline("Polar")) -> Figure11a





# Work to make a table of the strategies 

kable(Annual_Cost_Estimates_Run, format = 'html',
      col.names = c("Strategy", "Annual Estimate"),
      digits = 0,
      align = 'll',
      caption = "Table 2: Spend under a Polar Vortex") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> Figure11aTable




M_ISOCOMP_data_Run$`month(TimeStamp)` <-  month.name[M_ISOCOMP_data_Run$`month(TimeStamp)`]
M_ISOCOMP_data_Run %>% 
  select(1,6,7,8) %>% 
  adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "Mean") -> M_ISOCOMP_data_Run
M_ISOCOMP_data_Run[13,c(2:4)] <- M_ISOCOMP_data_Run[13,c(2:4)] / 12


kable(M_ISOCOMP_data_Run, format = 'html',  
      col.names = c("Month", "Index", "50/50", "Fixed"),
      digits = 3, 
      align = 'lccr',
      caption = "Table 4:  Monthly Unit Cost of Electricy Supply ($/kWH)") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> MonthUnitCostVortex


```






```{r Recession, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

#Make more analysis to 2014 the polar vortex year. 
R <- .7
PV <- 1

ISOCOMP_data_Run2 <- ISOCOMP_data

# Multiply Jan Feb by PV defined above in this section. 
ISOCOMP_data_Run2 %>% 
  mutate_at(vars(c('LBMP')), ~ifelse((month(TimeStamp) == 01 | month(TimeStamp) == 02) , LBMP * PV, LBMP )) -> ISOCOMP_data_Run2


ISOCOMP_data_Run2 %>% 
  mutate(LBMP = LBMP * R) %>% 
  mutate('UnitCost50_50' = (0.5 * LBMP) + (0.5 * 83.13)) %>% 
  mutate('UnitCostFixed' = 81.80 ) %>%
  mutate('HourlyCostFixed' = (Energy * UnitCostFixed / 1000) ) %>% 
  mutate('HourlyCost50_50' = (Energy * UnitCost50_50 / 1000 ) ) %>% 
  group_by(month(TimeStamp)) %>% 
  summarise('EnergyIndexCost' = sum(LBMP), 'Monthly_kWh' = sum(Energy), 'EnergyCost50_50' = sum(HourlyCost50_50), 'EnergyCostFixed' = sum(HourlyCostFixed)) %>% 
  mutate('EnergyIndexUnit' = EnergyIndexCost / Monthly_kWh,  'EnergyUnit50_50' = EnergyCost50_50 / Monthly_kWh, 'EnergyUnitFixed' = EnergyCostFixed / Monthly_kWh) -> M_ISOCOMP_data_Run2


M_ISOCOMP_data_Run2 %>% 
  summarise('CostIndex' = sum(EnergyIndexCost), 'Cost50_50' = sum(EnergyCost50_50), 'CostFixed' = sum(EnergyCostFixed)) -> Annual_Cost_Estimates_Run2


Annual_Cost_Estimates_Run2 %>% 
  gather(key = 'Strategy', value = 'Value' ) -> Annual_Cost_Estimates_Run2
gsub('Cost*', '', Annual_Cost_Estimates_Run$Strategy) -> Annual_Cost_Estimates_Run2$Strategy
Annual_Cost_Estimates_Run2$Strategy <- factor(Annual_Cost_Estimates_Run$Strategy, levels = Annual_Cost_Estimates_Run2$Strategy[order(Annual_Cost_Estimates_Run$Value, decreasing = TRUE)])

#Figure 12                                        
Annual_Cost_Estimates_Run2 %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  theme_classic() +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 12:" ~underline("Comparision of strategies under a Recession")) -> Figure12


#For side by side plot 
Annual_Cost_Estimates_Run2 %>% 
  ggplot(aes(x = Strategy, y = Value)) +
  geom_col(aes(fill = Strategy)) +
  theme(axis.text.x = element_text(angle=60, vjust = 0.5) ) +
  guides(fill=FALSE) +
  labs(x = "Strategy", y = "Annual Cost($)", title = "Figure 12:" ~underline("Recession")) -> Figure12a




# Work to make a table of the strategies 

kable(Annual_Cost_Estimates_Run2, format = 'html',
      col.names = c("Strategy", "Annual Estimate"),
      digits = 0,
      align = 'll',
      caption = "Table 3: Spend under a Recession") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> Figure12aTable 




M_ISOCOMP_data_Run2$`month(TimeStamp)` <-  month.name[M_ISOCOMP_data_Run2$`month(TimeStamp)`]
M_ISOCOMP_data_Run2 %>% 
  select(1,6,7,8) %>% 
  adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "Mean") -> M_ISOCOMP_data_Run2
M_ISOCOMP_data_Run2[13,c(2:4)] <- M_ISOCOMP_data_Run2[13,c(2:4)] / 12


kable(M_ISOCOMP_data_Run2, format = 'html',  
      col.names = c("Month", "Index", "50/50", "Fixed"),
      digits = 3, 
      align = 'lccr',
      caption = "Table 4:  Monthly Unit Cost of Electricy Supply ($/kWH)") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) -> MonthUnitCostRecession


```

```{r Comparisions, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

grid.arrange(Figure10a, Figure11a, Figure12a, ncol=3)

left_join(Annual_Cost_Estimates, Annual_Cost_Estimates_Run, by = "Strategy") %>% 
  left_join(Annual_Cost_Estimates_Run2, by = "Strategy") -> AnnualCostComparisionsTable
kable(AnnualCostComparisionsTable, format = 'html',
      col.names = c("Strategy", "2023", "Polar", "Recession"),
      digits = 0,
      align = 'lrrr',
      caption = "Table 1: Spend under various scenarios") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  footnote(c("2023 Electric Supply Options")) 

Figure9


```

