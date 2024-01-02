# Authored by Sarah Katz, Caroline Jung, Tayae Rogers 
# 8 December 2022 
# STAT 260 Final Project 

##### SECTION 1: MERGING AND CLEANING THE DATA ##### -----

### STEP 1: MERGE RAW DATA ### 
#note: Due to empty columns in the raw dataset, we had to manually delete them before reading them into R since we got errors that the dimensions did not match.

# read in data -----
co2 <- read.csv(file.choose(),header=T) # used csv called "CO2_adj_NOemptycolumns"
pop <- read.csv(file.choose(),header=T) # used csv called "WorldPopulationAdjusted_start1990" 
gdp <- read.csv(file.choose(),header=T) # used csv called "GDP_1990_2019" 
income <- read.csv(file.choose(),header=T) # used csv called "CountryIncomeLevel" 

# rename columns ----- 
colnames(co2) <- c("Country.Name","Country.Code","Indicator.Name","Indicator.Code", "c1990","c1991","c1992","c1993", "c1994","c1995","c1996","c1997", "c1998","c1999", "c2000","c2001","c2002","c2003","c2004","c2005","c2006","c2007","c2008","c2009","c2010","c2011","c2012","c2013","c2014","c2015","c2016","c2017","c2018","c2019")
dim(co2)
dim(pop)
head(co2)
head(pop)

#omit 2020 and 2021 columns from population data -----
pop <- pop[,1:34] 

# multiply the matrices of pop and co2 years ----- 
names(pop)
names(co2)
popyrs <- pop[,5:34]
co2yrs <- co2[,5:34]
multyrs <- popyrs * co2yrs
names(multyrs)
newco2 <- cbind(pop[,1:4],multyrs) 

newco2 # NEW CO2 DATASET (not per capita) 

# add CO2 data to GDP data -----
# rename columns (newco2 has same column names for dates as gdp)
colnames(newco2) = c("Country.Name","Country.Code","Indicator.Name","Indicator.Code", "c1990","c1991","c1992","c1993", "c1994","c1995","c1996","c1997", "c1998","c1999", "c2000","c2001","c2002","c2003","c2004","c2005","c2006","c2007","c2008","c2009","c2010","c2011","c2012","c2013","c2014","c2015","c2016","c2017","c2018","c2019")

dim(newco2)
dim(gdp)
names(gdp)
names(newco2)
gdpco <- cbind(gdp, newco2[,5:34]) #adds newCO2 data to GDP data --> new dataset "gdpco"
names(gdpco)

# add income data to gdpco data  -----
#1: find irregularities: income has 265 rows, gdpco has 266
head(income)
dim(income)
income$Country.Code == gdpco$Country.Code
income[111,1]
gdpco[111,1] #111th row "not classified" needs to be deleted to match num of rows with income csv
gdpcoClean <- gdpco[-111,]
income$Country.Code == gdpcoClean$Country.Code #checked: income & gdpcoClean has the same number of rows

#2: merge income data to gdpcoClean
full <- cbind(gdpcoClean, income[,c("Region", "IncomeGroup")])

# FULLY CLEANED DATASET  -----
full 
dim(full)
full <- full[full$IncomeGroup!="",] # all the rows that didn't have income group were not countries (ex: "World")
dim(full)

# write.csv(full,"step1merged.csv", row.names = FALSE)

### STEP 2: REMOVE COUNTRIES BASED ON CRITERIA ### 
# getting rid of rows that are not UN-recognized states in population data for further analysis----- 
pop <- subset(pop, Country.Name!="Channel Islands" &
                Country.Name!="Cayman Islands"& 
                Country.Name!="Gibraltar"& 
                Country.Name!="Guam"& 
                Country.Name!="Faroe Islands"& 
                Country.Name!="St. Martin (French part)"& 
                Country.Name!="New Caledonia"&
                Country.Name!="French Polynesia"&
                Country.Name!="Northern Mariana Islands"&
                Country.Name!="Turks and Caicos Islands"&
                Country.Name!="British Virgin Islands"&
                Country.Name!="Virgin Islands (U.S.)"&
                Country.Name!="Puerto Rico"&
                Country.Name!="American Samoa"&
                Country.Name!="Curacao"&
                Country.Name!="Macao SAR, China"&
                Country.Name!="Hong Kong SAR, China"&
                Country.Name!="Isle of Man"&
                Country.Name!="Sint Maarten (Dutch part)"&
                Country.Name!="Aruba"&
                Country.Name!="Bermuda")

# cutoff/condition for population size -----
# small is the vector of countries to exclude because population size was <50,000 in 1990 
small <- pop[pop$X1990<50000,]
dim(small)
small$Country.Name

# remove rows that are not UN-recognized states ----- 
full2 <- subset(full, Country.Name!="Channel Islands" &
                Country.Name!="Cayman Islands"& 
                Country.Name!="Gibraltar"& 
                Country.Name!="Guam"& 
                Country.Name!="Faroe Islands"& 
                Country.Name!="St. Martin (French part)"& 
                Country.Name!="New Caledonia"&
                Country.Name!="French Polynesia"&
                Country.Name!="Northern Mariana Islands"&
                Country.Name!="Turks and Caicos Islands"&
                Country.Name!="British Virgin Islands"&
                Country.Name!="Virgin Islands (U.S.)"&
                Country.Name!="Puerto Rico"&
                Country.Name!="American Samoa"&
                Country.Name!="Curacao"&
                Country.Name!="Macao SAR, China"&
                Country.Name!="Hong Kong SAR, China"&
                Country.Name!="Isle of Man"&
                Country.Name!="Sint Maarten (Dutch part)"&
                Country.Name!="Aruba"&
                Country.Name!="Bermuda")

# remove countries that had population less than 50,000 in 1990 in population dataset ----- 
full3 <- subset(full2, Country.Name!="St. Kitts and Nevis" &
                   Country.Name!="Liechtenstein"& 
                   Country.Name!="Monaco"& 
                   Country.Name!="Marshall Islands"& 
                   Country.Name!="Nauru"& 
                   Country.Name!="Palau"& 
                   Country.Name!="San Marino"&
                   Country.Name!="Tuvalu") 

# write.csv(full3,"FULLDATAv5.csv", row.names = FALSE)

############################################################################################################################################

#STEP 3: clean and impute missing values----
full4 <- read.csv(file.choose(),header=T) # used csv called "FULLDATAv5" overall
missing <- read.csv(file.choose(),header=T) #use csv called "filledmissingdata"
pop <- read.csv(file.choose(),header=T) #use csv called "WorldPopulationAdjusted_start1990"

#install.packages("ddpcr")
library(ddpcr)
firstmerge <- merge_dfs_overwrite_col(data, missing, bycol="Country.Code") #initial merge with no imputation or cleaning of undefined values

#clean missing dataset  - get rid of populations < 50,000 & also Kosovo since there are no CO2 values
omit <- c("St. Kitts and Nevis","Liechtenstein","Monaco","Marshall Islands","Nauru","Palau","San Marino","Tuvalu","Kosovo") 
new <- subset(firstmerge, !firstmerge$Country.Name %in% omit)

#impute missing values (NA)
imputed <- new #what we will return
sum(is.na(imputed))
# found there were NA's in imputed --> checked for remaining NA's
# for(row in 1:nrow(imputed)){
#   for(col in X1990i:c2019i){ #rows from X1990 to c2019
#     if(is.na(imputed[row, col])){
#       final[row, col] = -2
#     }
#   }
# }
# write.csv(final, "identifyNAs.csv", row.names=FALSE)

#Micronesia CO2: 1990 and 1991 CO2 are imputed w the same value as 1992 since CO2 levels stay the same from 1992-1994
imputed$c1990[new$Country.Code=="FSM"] <- new$c1992[new$Country.Code=="FSM"]
imputed$c1991[new$Country.Code=="FSM"] <- new$c1992[new$Country.Code=="FSM"]

#NKorea GDP: 2018 and 2019 GDP imputed according to linear relationship
diff <- new$X2017[new$Country.Code=="PRK"] - new$X2016[new$Country.Code=="PRK"] 
imputed$X2018[new$Country.Code=="PRK"] <- new$X2017[new$Country.Code=="PRK"] + diff
imputed$X2019[new$Country.Code=="PRK"] <- imputed$X2018[new$Country.Code=="PRK"] + diff

#Mali CO2: 1996 per capita CO2 value (which is 0) is imputed via mean imputation
#per capita CO2 1995: 0.0010432249839865 from World Bank
#per capita CO2 1997: 0.000990651911368751
#per capita CO2 1996: 0
impute1996 <- mean(0.0010432249839865, 0.000990651911368751) #percapita CO2 imputed
pop1996 <- pop$X1996[pop$Country.Name=="Mali"] #population in 1996
imputed$c1996[new$Country.Name=="Mali"] <- impute1996*pop1996 #overall CO2

#Namibia CO2: 1991 CO2 imputed according to linear relationship
diffnam = new$c1992[new$Country.Code=="NAM"] - new$c1991[new$Country.Code=="NAM"] 
imputed$c1990[new$Country.Code=="NAM"] <- new$c1991[new$Country.Code=="NAM"] - diffnam

sum(is.na(imputed)) #this is now 0, so there are no missing values now

#make undefined values (-1) into NA's
final <- imputed

X1990i <- grep("X1990", colnames(firstmerge)) #column index for X1990
c2019i <- grep("c2019", colnames(firstmerge))
for(row in 1:nrow(final)){
  for(col in X1990i:c2019i){ #rows from X1990 to c2019
    if(imputed[row, col]==-1){
      final[row, col] <- NA
    }
  }
}
#write.csv(final, "FULLDATACLEANEDv4.csv", row.names=FALSE)

############################################################################################################################################

#### SECTION: CHANGING DATA TO PER CAPITA -----------

library(tidyverse)

gdpco2 <- read.csv(file.choose(), header=T) # used csv "FULLDATACLEANEDv4"
#pop <- read.csv(file.choose(),header=T) # used csv "WorldPopulationAdjusted_start1990"

# Rename columns (currently, both GDP and pop columns are both 'X1990', etc so renaming the population columns to avoid confusion)
pop <- pop[,1:34]
colnames(pop) <- c("Country.Name","Country.Code","Indicator.Name","Indicator.Code", "pop1990","pop1991","pop1992","pop1993", "pop1994","pop1995","pop1996","pop1997","pop1998",
                   "pop1999","pop2000","pop2001","pop2002","pop2003","pop2004","pop2005","pop2006","pop2007",
                   "pop2008","pop2009","pop2010","pop2011","pop2012","pop2013","pop2014","pop2015","pop2016",
                   "pop2017","pop2018","pop2019")

# For population data, if the country is not in the gdpco2 data, drop that row
# This ensures that the dataframes have the same dimensions, so we can do operations (specifically, division) that uses both datasets
desiredCountries <- pop$Country.Code[(pop$Country.Code %in% gdpco2$Country.Code) == TRUE]
pop <- as_tibble(pop)
popSub <- pop %>%
  filter(Country.Code %in% desiredCountries)

# Reorders columns in gdpco2 dataset
gdpco2 <- gdpco2[,c(1,2,3,4,65,66,5:64)]

# Splitting up gdp and co2 datasets to more easily divide to get per capita
gdpOnly <- gdpco2[,c(7:36)]
co2Only <- gdpco2[,c(37:66)]

# Narrowing population dataset just to population values (no country code, etc)
popOnly <- popSub[,5:34]

# For each dataset, caluclating per capita
gdpPerCap <- gdpOnly/popOnly
co2PerCap <- co2Only/popOnly

# Bind datasets
fullDataPerCap <- cbind(gdpco2[,1:6], gdpPerCap, co2PerCap)

# Write to csv
# write.csv(fullDataPerCap, "/Users/tayaerogers/Desktop/STAT 260/FinalProject/FULLDATACLEANEDv4_perCap.csv")

#### SECTION: CHANGING DATA TO LONG FORMAT -----------
## Changes format of data table to allow running of regression

library(tidyr)
library(dplyr)

df_fulldata <- read.csv(file.choose(), header=T) # used csv "FULLDATACLEANEDv4"

df_fulldata <- as_tibble(df_fulldata)

# Drops variables that are repetitive
df_thin <- df_fulldata %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code)

# Make long version so each row has a country and year and GDP or CO2
df_thin <- df_thin %>%
  pivot_longer(cols=c("X1990":"c2019"), names_to='yearTemp', values_to='valuesTemp')

# Drops rows with NA
df_thin <- df_thin[!is.na(df_thin$valuesTemp),]

## Correcting to one column for year
df_thin$year <- NA

# For GDP columns -----
df_thin$year[df_thin$yearTemp == 'X1990' | df_thin$yearTemp == 'c1990'] <- 1990
df_thin$year[df_thin$yearTemp == 'X1991' | df_thin$yearTemp == 'c1991'] <- 1991
df_thin$year[df_thin$yearTemp == 'X1992' | df_thin$yearTemp == 'c1992'] <- 1992
df_thin$year[df_thin$yearTemp == 'X1993' | df_thin$yearTemp == 'c1993'] <- 1993
df_thin$year[df_thin$yearTemp == 'X1994' | df_thin$yearTemp == 'c1994'] <- 1994
df_thin$year[df_thin$yearTemp == 'X1995' | df_thin$yearTemp == 'c1995'] <- 1995
df_thin$year[df_thin$yearTemp == 'X1996' | df_thin$yearTemp == 'c1996'] <- 1996
df_thin$year[df_thin$yearTemp == 'X1997' | df_thin$yearTemp == 'c1997'] <- 1997
df_thin$year[df_thin$yearTemp == 'X1998' | df_thin$yearTemp == 'c1998'] <- 1998
df_thin$year[df_thin$yearTemp == 'X1999' | df_thin$yearTemp == 'c1999'] <- 1999
df_thin$year[df_thin$yearTemp == 'X2000' | df_thin$yearTemp == 'c2000'] <- 2000
df_thin$year[df_thin$yearTemp == 'X2001' | df_thin$yearTemp == 'c2001'] <- 2001
df_thin$year[df_thin$yearTemp == 'X2002' | df_thin$yearTemp == 'c2002'] <- 2002
df_thin$year[df_thin$yearTemp == 'X2003' | df_thin$yearTemp == 'c2003'] <- 2003
df_thin$year[df_thin$yearTemp == 'X2004' | df_thin$yearTemp == 'c2004'] <- 2004
df_thin$year[df_thin$yearTemp == 'X2005' | df_thin$yearTemp == 'c2005'] <- 2005
df_thin$year[df_thin$yearTemp == 'X2006' | df_thin$yearTemp == 'c2006'] <- 2006
df_thin$year[df_thin$yearTemp == 'X2007' | df_thin$yearTemp == 'c2007'] <- 2007
df_thin$year[df_thin$yearTemp == 'X2008' | df_thin$yearTemp == 'c2008'] <- 2008
df_thin$year[df_thin$yearTemp == 'X2009' | df_thin$yearTemp == 'c2009'] <- 2009
df_thin$year[df_thin$yearTemp == 'X2010' | df_thin$yearTemp == 'c2010'] <- 2010
df_thin$year[df_thin$yearTemp == 'X2011' | df_thin$yearTemp == 'c2011'] <- 2011
df_thin$year[df_thin$yearTemp == 'X2012' | df_thin$yearTemp == 'c2012'] <- 2012
df_thin$year[df_thin$yearTemp == 'X2013' | df_thin$yearTemp == 'c2013'] <- 2013
df_thin$year[df_thin$yearTemp == 'X2014' | df_thin$yearTemp == 'c2014'] <- 2014
df_thin$year[df_thin$yearTemp == 'X2015' | df_thin$yearTemp == 'c2015'] <- 2015
df_thin$year[df_thin$yearTemp == 'X2016' | df_thin$yearTemp == 'c2016'] <- 2016
df_thin$year[df_thin$yearTemp == 'X2017' | df_thin$yearTemp == 'c2017'] <- 2017
df_thin$year[df_thin$yearTemp == 'X2018' | df_thin$yearTemp == 'c2018'] <- 2018
df_thin$year[df_thin$yearTemp == 'X2019' | df_thin$yearTemp == 'c2019'] <- 2019

## Correcting to one column per country and year

# New variables for GDP and CO2
df_thin['GDP'] <- NA
df_thin['CO2'] <- NA

# Moving GDP values from temporary values variable to GDP variable
df_thin$GDP[df_thin$yearTemp == 'X1990'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1990']
df_thin$GDP[df_thin$yearTemp == 'X1991'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1991']
df_thin$GDP[df_thin$yearTemp == 'X1992'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1992']
df_thin$GDP[df_thin$yearTemp == 'X1993'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1993']
df_thin$GDP[df_thin$yearTemp == 'X1994'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1994']
df_thin$GDP[df_thin$yearTemp == 'X1995'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1995']
df_thin$GDP[df_thin$yearTemp == 'X1996'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1996']
df_thin$GDP[df_thin$yearTemp == 'X1997'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1997']
df_thin$GDP[df_thin$yearTemp == 'X1998'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1998']
df_thin$GDP[df_thin$yearTemp == 'X1999'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X1999']
df_thin$GDP[df_thin$yearTemp == 'X2000'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2000']
df_thin$GDP[df_thin$yearTemp == 'X2001'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2001']
df_thin$GDP[df_thin$yearTemp == 'X2002'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2002']
df_thin$GDP[df_thin$yearTemp == 'X2003'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2003']
df_thin$GDP[df_thin$yearTemp == 'X2004'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2004']
df_thin$GDP[df_thin$yearTemp == 'X2005'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2005']
df_thin$GDP[df_thin$yearTemp == 'X2006'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2006']
df_thin$GDP[df_thin$yearTemp == 'X2007'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2007']
df_thin$GDP[df_thin$yearTemp == 'X2008'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2008']
df_thin$GDP[df_thin$yearTemp == 'X2009'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2009']
df_thin$GDP[df_thin$yearTemp == 'X2010'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2010']
df_thin$GDP[df_thin$yearTemp == 'X2011'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2011']
df_thin$GDP[df_thin$yearTemp == 'X2012'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2012']
df_thin$GDP[df_thin$yearTemp == 'X2013'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2013']
df_thin$GDP[df_thin$yearTemp == 'X2014'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2014']
df_thin$GDP[df_thin$yearTemp == 'X2015'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2015']
df_thin$GDP[df_thin$yearTemp == 'X2016'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2016']
df_thin$GDP[df_thin$yearTemp == 'X2017'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2017']
df_thin$GDP[df_thin$yearTemp == 'X2018'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2018']
df_thin$GDP[df_thin$yearTemp == 'X2019'] <- df_thin$valuesTemp[df_thin$yearTemp == 'X2019']

# Moving CO2 values from temporary values variable to CO2 variable
df_thin$CO2[df_thin$yearTemp == 'c1990'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1990']
df_thin$CO2[df_thin$yearTemp == 'c1991'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1991']
df_thin$CO2[df_thin$yearTemp == 'c1992'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1992']
df_thin$CO2[df_thin$yearTemp == 'c1993'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1993']
df_thin$CO2[df_thin$yearTemp == 'c1994'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1994']
df_thin$CO2[df_thin$yearTemp == 'c1995'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1995']
df_thin$CO2[df_thin$yearTemp == 'c1996'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1996']
df_thin$CO2[df_thin$yearTemp == 'c1997'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1997']
df_thin$CO2[df_thin$yearTemp == 'c1998'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1998']
df_thin$CO2[df_thin$yearTemp == 'c1999'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c1999']
df_thin$CO2[df_thin$yearTemp == 'c2000'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2000']
df_thin$CO2[df_thin$yearTemp == 'c2001'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2001']
df_thin$CO2[df_thin$yearTemp == 'c2002'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2002']
df_thin$CO2[df_thin$yearTemp == 'c2003'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2003']
df_thin$CO2[df_thin$yearTemp == 'c2004'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2004']
df_thin$CO2[df_thin$yearTemp == 'c2005'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2005']
df_thin$CO2[df_thin$yearTemp == 'c2006'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2006']
df_thin$CO2[df_thin$yearTemp == 'c2007'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2007']
df_thin$CO2[df_thin$yearTemp == 'c2008'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2008']
df_thin$CO2[df_thin$yearTemp == 'c2009'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2009']
df_thin$CO2[df_thin$yearTemp == 'c2010'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2010']
df_thin$CO2[df_thin$yearTemp == 'c2011'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2011']
df_thin$CO2[df_thin$yearTemp == 'c2012'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2012']
df_thin$CO2[df_thin$yearTemp == 'c2013'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2013']
df_thin$CO2[df_thin$yearTemp == 'c2014'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2014']
df_thin$CO2[df_thin$yearTemp == 'c2015'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2015']
df_thin$CO2[df_thin$yearTemp == 'c2016'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2016']
df_thin$CO2[df_thin$yearTemp == 'c2017'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2017']
df_thin$CO2[df_thin$yearTemp == 'c2018'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2018']
df_thin$CO2[df_thin$yearTemp == 'c2019'] <- df_thin$valuesTemp[df_thin$yearTemp == 'c2019']

## Inversing the dataset so there's one country and one year on each row (kinda merging the GDP and CO2 columns) -----

# Assessing current dataset
dim(df_thin)
summary(df_thin$GDP)
summary(df_thin$CO2)
# GDP column has more NAs, so we should add the GDP data to the CO2 data (not vice versa bc would leave out data)

# Subset a dataset of just rows with GDP values
subsetGDP <- df_thin %>%
  filter(startsWith(yearTemp, 'X'))

# Subset a dataset of just rows with CO2 values
subsetCO2 <- df_thin %>%
  filter(startsWith(yearTemp, 'c'))

# Remove extra columns from GDP subset
subsetGDP <- subsetGDP %>%
  select(-yearTemp, -valuesTemp, -CO2)

# Remove extra columns from CO2 subset
subsetCO2 <- subsetCO2 %>%
  select(-yearTemp, -valuesTemp, -GDP)

# Join GDP and CO2 subsets on country and year, so each row represents a country/year combo with both GDP and CO2 data
join <- merge(subsetCO2, subsetGDP, by=c('Country.Name', 'Region', 'IncomeGroup', 'year'), all=TRUE)

# Write to csv
#write.csv(join, "/Users/tayaerogers/Desktop/STAT 260/FinalProject/FULLDATACLEANEDv4_long.csv") 

############################################################################################################################################

##### SECTION 2: VISUALIZING THE DATA ##### ----- 
### 1: Creating Visualizations (GDP and CO2, overall and per capita) Over Time and Color Coded by Region ### 

# Read in data -----
d <- read.csv(file.choose(),header=T) # FULLDATACLEANEDv4 
dpercap <- read.csv(file.choose(),header=T) # FULLDATACLEANEDv4_perCap
names(d) # X = gdp, c = co2 emissions 
dim(d)
names(dpercap)
dim(dpercap)

# GDP (not per cap) max and min values  -----
# max
highestgdp <- d[d$X2019==max(na.omit(d$X2019)),]
highestgdp$Country.Name # United States was the country with the highest GDP in 2019

# min 
lowestgdp <- d[d$X2019==min(na.omit(d$X2019)),]
lowestgdp$Country.Name # Kiribati was the country with the highest GDP in 2019

# GDP (not per cap) visualization  -----
usecolor <-"orange" # the first value is Afghanistan which has region = South Asia, so usecolor starts as orange. 
par(mfrow=c(1,1))
plot(1990:2019, log(d[1,5:34]), type="l", ylim=c(13, 31),col=usecolor, 
     xlab="Year", ylab="Log GDP (in USD)", 
     main="Log Country GDP from 1990 to 2019",  lwd=0.5) 
for (ii in 2:nrow(d)){
  if (d$Region[ii] == "East Asia & Pacific") {
    usecolor <- "green" }
  else if (d$Region[ii] == "Europe & Central Asia") {
    usecolor <- "red"}
  else if (d$Region[ii] == "Latin America & Caribbean") { 
    usecolor <- "blue"}
  else if (d$Region[ii] == "Middle East & North Africa") {
    usecolor <- "purple"} 
  else if (d$Region[ii] == "North America") {
    usecolor <- "turquoise"}
  else if (d$Region[ii] == "South Asia") {
    usecolor <- "orange"}
  else if (d$Region[ii] == "Sub-Saharan Africa") {
    usecolor <- "magenta"}
  if (d$Country.Name[ii] == "United States" | d$Country.Name[ii] == "Kiribati") {
    points(1990:2019, log(d[ii,5:34]), type="l", col=usecolor, lwd=2)}
  else {
    points(1990:2019, log(d[ii,5:34]), type="l", col=usecolor, lwd=0.5)} # 5:34
}
legend(2005, 17.5, legend=c("East Asia & Pacific", "Europe & Central Asia",
                            "Latin America & Caribbean", "Middle East & North Africa",
                            "North America","South Asia","Sub-Saharan Africa"), fill=c("green","red", "blue","purple","turquoise","orange","magenta"))

text(x=2015,y=31,"United States",col="turquoise")
text(x=2015,y=18.5,"Kiribati",col="green")

# CO2 (not per cap) max and min values -----
#max
highestco2 <- d[d$c2019==(max(na.omit(d$c2019))),]
highestco2$Country.Name # China was the country with the highest CO2 emissions in 2019

# min  
lowestco2 <- d[d$c2019==(min(na.omit(d$c2019))),]
lowestco2$Country.Name # Kiribati was the country with the highest CO2 emissions in 2019

# CO2 (not per cap) visualization  -----
par(mfrow=c(1,1))
plot(1990:2019, log(d[1,35:64]), type="l", ylim=c(5, 24),col=usecolor, 
     xlab="Year", ylab="Log CO2 Emissions (in Metric Tons)", 
     main="Log Country CO2 Emissions from 1990 to 2019", lwd=0.5) 
for (ii in 2:nrow(d)){
  if (d$Region[ii] == "East Asia & Pacific") {
    usecolor <- "green" }
  else if (d$Region[ii] == "Europe & Central Asia") {
    usecolor <- "red"}
  else if (d$Region[ii] == "Latin America & Caribbean") { 
    usecolor <- "blue"}
  else if (d$Region[ii] == "Middle East & North Africa") {
    usecolor <- "purple"} 
  else if (d$Region[ii] == "North America") {
    usecolor <- "turquoise"}
  else if (d$Region[ii] == "South Asia") {
    usecolor <- "orange"}
  else if (d$Region[ii] == "Sub-Saharan Africa") {
    usecolor <- "magenta"}
  if (d$Country.Name[ii] == "China" | d$Country.Name[ii] == "Kiribati") {
    points(1990:2019, log(d[ii,35:64]), type="l", col=usecolor, lwd=2)}
  else {
    points(1990:2019, log(d[ii,35:64]), type="l", col=usecolor, lwd=0.5)} 
}
legend(2004, 9.75, legend=c("East Asia & Pacific", "Europe & Central Asia",
                            "Latin America & Caribbean", "Middle East & North Africa",
                            "North America","South Asia","Sub-Saharan Africa"), fill=c("green","red", "blue","purple","turquoise","orange","magenta"))

text(x=2017,y=23.5,"China",col="green")
text(x=2017,y=10.5,"Kiribati",col="green")

# GDP per capita max and min values -----
# max
highestgdppercap <- dpercap[dpercap$X2019==max(na.omit(dpercap$X2019)),]
highestgdppercap$Country.Name # Luxembourg was the country with the highest GDP per capita in 2019 

# min 
lowestgdppercap <- dpercap[dpercap$X2019==min(na.omit(dpercap$X2019)),]
lowestgdppercap$Country.Name # Burundi was the country with the highest GDP per capita in 2019

# GDP per capita visualization -----
par(mfrow=c(1,1))
plot(1990:2019, log(dpercap[1,8:37]), type="l", ylim=c(2,12),col=usecolor, 
     xlab="Year", ylab="Log GDP Per Capita (in USD)", 
     main="Log Country GDP Per Capita from 1990 to 2019",  lwd=0.5) 
for (ii in 2:nrow(dpercap)){
  if (dpercap$Region[ii] == "East Asia & Pacific") {
    usecolor <- "green" }
  else if (dpercap$Region[ii] == "Europe & Central Asia") {
    usecolor <- "red"}
  else if (dpercap$Region[ii] == "Latin America & Caribbean") { 
    usecolor <- "blue"}
  else if (dpercap$Region[ii] == "Middle East & North Africa") {
    usecolor <- "purple"} 
  else if (dpercap$Region[ii] == "North America") {
    usecolor <- "turquoise"}
  else if (dpercap$Region[ii] == "South Asia") {
    usecolor <- "orange"}
  else if (dpercap$Region[ii] == "Sub-Saharan Africa") {
    usecolor <- "magenta"}
  if (dpercap$Country.Name[ii] == "Luxembourg" | dpercap$Country.Name[ii] == "Burundi") {
    points(1990:2019, log(dpercap[ii,8:37]), type="l", col=usecolor, lwd=2)}
  else {
    points(1990:2019, log(dpercap[ii,8:37]), type="l", col=usecolor, lwd=0.5)} 
}
legend(2009, 4.5, legend=c("East Asia & Pacific", "Europe & Central Asia",
                           "Latin America & Caribbean", "Middle East & North Africa",
                           "North America","South Asia","Sub-Saharan Africa"), fill=c("green","red", "blue","purple","turquoise","orange","magenta"))

text(x=2016,y=12,"Luxembourg",col="red") 
text(x=2016,y=5.25,"Burundi",col="magenta") 

# CO2 per capita max and min values  -----
# max 
highestco2percap <- dpercap[dpercap$c2019==(max(na.omit(dpercap$c2019))),]
highestco2percap$Country.Name # Qatar was the country with the highest CO2 emissions per capita in 2019

#min 
lowestco2percap <- dpercap[dpercap$c2019==(min(na.omit(dpercap$c2019))),]
lowestco2percap$Country.Name # Congo, Dem. Rep. was the country with the highest CO2 emissions per capita in 2019

# CO2 per capita visualization -----
par(mfrow=c(1,1))
plot(1990:2019, log(dpercap[1,38:67]), type="l", ylim=c(-7.75, 4.5),col=usecolor, 
     xlab="Year", ylab="Log CO2 Emissions Per Capita (in Metric Tons)", 
     main="Log Country CO2 Emissions Per Capita from 1990 to 2019", lwd=0.5) # 35:64 # c(5, 25),
for (ii in 2:nrow(dpercap)){
  if (dpercap$Region[ii] == "East Asia & Pacific") {
    usecolor <- "green" }
  else if (dpercap$Region[ii] == "Europe & Central Asia") {
    usecolor <- "red"}
  else if (dpercap$Region[ii] == "Latin America & Caribbean") { 
    usecolor <- "blue"}
  else if (dpercap$Region[ii] == "Middle East & North Africa") {
    usecolor <- "purple"} 
  else if (dpercap$Region[ii] == "North America") {
    usecolor <- "turquoise"}
  else if (dpercap$Region[ii] == "South Asia") {
    usecolor <- "orange"}
  else if (dpercap$Region[ii] == "Sub-Saharan Africa") {
    usecolor <- "magenta"}
  if (dpercap$Country.Name[ii] == "Qatar" | dpercap$Country.Name[ii] == "Congo, Dem. Rep.") {
    points(1990:2019, log(dpercap[ii,38:67]), type="l", col=usecolor, lwd=2)}
  else {
    points(1990:2019, log(dpercap[ii,38:67]), type="l", col=usecolor, lwd=0.5)} 
}
legend(2008, -4.5, legend=c("East Asia & Pacific", "Europe & Central Asia",
                            "Latin America & Caribbean", "Middle East & North Africa",
                            "North America","South Asia","Sub-Saharan Africa"), fill=c("green","red", "blue","purple","turquoise","orange","magenta"))

text(x=2017,y=3.75,"Qatar",col="purple") 
text(x=2017,y=-4,"Congo, Dem. Rep.",col="magenta")














############################################################################################################################################

# 2: For each plot, focus on only one year and plot CO2 vs GDP for all countries. This is showing overall CO2 and GDP (not per capita)
data <- read.csv(file.choose(),header=T) #use final merged data (wide) "FULLDATACLEANEDv3"

#for each year
#1. subset data for GDP, CO2, Region for the specific year
#2. sort subsetted data to take care of overlapping points when plotting
#3. plot

df1990 <- data[,c(1,2,grep("X1990", colnames(data)), grep("c1990", colnames(data)), grep("Region", colnames(data)))]
sort1990 <- df1990[order(df1990$X1990, df1990$c1990),]
g1990 <- ggplot(sort1990, aes(x=log(X1990), y=log(c1990), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) +
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="1990") + xlim(15,32) + ylim(10,23)


df1995 <- data[,c(1,2,grep("X1995", colnames(data)), grep("c1995", colnames(data)), grep("Region", colnames(data)))]
sort1995 <- df1995[order(df1995$X1995, df1995$c1995),]
g1995 <- ggplot(sort1995, aes(x=log(X1995), y=log(c1995), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="1995") + xlim(15,32) + ylim(10,23)

df2000 <- data[,c(1,2,grep("X2000", colnames(data)), grep("c2000", colnames(data)), grep("Region", colnames(data)))]
sort2000 <- df2000[order(df2000$X2000, df2000$c2000),]
g2000 <- ggplot(sort2000, aes(x=log(X2000), y=log(c2000), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2000") + xlim(15,32) + ylim(10,23)

df2005 <- data[,c(1,2,grep("X2005", colnames(data)), grep("c2005", colnames(data)), grep("Region", colnames(data)))]
sort2005 <- df2005[order(df2005$X2005, df2005$c2005),]
g2005 <- ggplot(sort2005, aes(x=log(X2005), y=log(c2005), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) +
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2005") + xlim(15,32) + ylim(10,23)

df2010 <- data[,c(1,2,grep("X2010", colnames(data)), grep("c2010", colnames(data)), grep("Region", colnames(data)))]
sort2010 <- df2010[order(df2010$X2010, df2010$c2010),]
g2010 <- ggplot(sort2010, aes(x=log(X2010), y=log(c2010), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2010") + xlim(15,32) + ylim(10,23)

df2015 <- data[,c(1,2,grep("X2015", colnames(data)), grep("c2015", colnames(data)), grep("Region", colnames(data)))]
sort2015 <- df2015[order(df2015$X2015, df2015$c2015),]
g2015 <- ggplot(sort2015, aes(x=log(X2015), y=log(c2015), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2015") + xlim(15,32) + ylim(10,23)

df2019 <- data[,c(1,2,grep("X2019", colnames(data)), grep("c2019", colnames(data)), grep("Region", colnames(data)))]
sort2019 <- df2019[order(df2019$X2019, df2019$c2019),]
g2019 <- ggplot(sort2019, aes(x=log(X2019), y=log(c2019), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=FALSE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2019") + xlim(15,32) + ylim(10,23)

forlegend <- ggplot(sort2019, aes(x=log(X2019), y=log(c2019), color=Region)) + 
  geom_point(cex=1.5, alpha=0.3, show.legend=TRUE) + 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", title="2019") + xlim(15,32) + ylim(10,23)
leg <- get_legend(forlegend)
legend <- as_ggplot(leg)

#install.packages("ggpubr")
library("ggpubr")
overall <- ggarrange(g1990, g1995, g2000, g2005, g2010, g2015, g2019, legend,
                    ncol=4, nrow=2)

############################################################################################################################################

# 3: Regression Lines Plot  -----
#for overall data
slopes = ggplot(sort1990, aes(x=log(X1990), y=log(c1990))) + #just need a plot
  geom_point(cex=0, alpha=0) + xlim(15,32) + ylim(10,23)+ 
  labs(x="Log of GDP (in USD)", y="Log of CO2 Emissions (metric tons)", 
       title="Regression Lines for Log CO2 vs GDP 
       Totals Every 5 Years") +
  geom_abline(slope=0.40541, intercept=6.821, linewidth=0.3) + #1990
  geom_abline(slope=0.388705, intercept=7.2355, linewidth=0.3) + #1995
  geom_abline(slope=0.372, intercept=7.65, linewidth=0.3) + #2000
  geom_abline(slope=0.355295, intercept=8.0645, linewidth=0.3) + #2005
  geom_abline(slope=0.33859, intercept=8.479, linewidth=0.3) + #2010
  geom_abline(slope=0.321885, intercept=8.8935, linewidth=0.3) + #2015
  geom_abline(slope=0.308521, intercept=9.2251, linewidth=0.3) #2019

############################################################################################################################################

##### SECTION 3: ANALYZING THE DATA ##### ----- 

library(tidyr)
library(dplyr)

d2 <- read.csv(file.choose(), header=T) # used csv "FULLDATACLEANEDv2_inverted_120822"

# Double-checking dataset
head(d2)

# Interaction
modelInteract <- lm(logCO2~Country.Name+logGDP*year, data=d2)
summary(modelInteract)
# p-value log(GDP): 0
# p-value year: 0.00972
# p-value log(GDP)*interaction: 0
# R-squared: 0.9824
# Adjusted R-squared: 0.9818

# No interaction
modelNoInteract <- lm(log(CO2)~Country.Name+log(GDP)+year, data=d2)
summary(modelNoInteract)
# p-value log(GDP): 0
# p-value year: 0.011257
# R-squared: 0.9817
# Adjusted R-squared: 0.981

BIC(modelInteract)
# 5040.068

BIC(modelNoInteract)
# 5253.177

# Determining a contry with ~median CO2 value to use as an estimate in simplified regression
d_temp <- read.csv(file.choose(), header=T) # used csv "FULLDATACLEANEDv4"

d_temp2 <- d_temp[order(d_temp$c2005),]
d_temp2[93:94,] # Identifies North Macedonia and Bolivia as two middle countries
# Want to choose a country that has a relatively median emissions level throughout,
# so checking other years

d_temp3 <- d_temp[order(d_temp$c1990),]
d_temp3[89:98,] # Both are present here

d_temp4 <- d_temp[order(d_temp$c2019),]
d_temp4[89:98,] # Neither are in here
d_temp4[80:110,] # Bolivia in here (a little over median, but close enough)





