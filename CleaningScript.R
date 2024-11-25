##Cleaning Cross-Country Cross-Wave data set (Grand Merge

library(haven)
library(dplyr)
library(survey)
library(tidyr)


##Open base (decided to use the Stata files instead of the SPSS)
Data<-read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.dta")

LAPOP<-Data

## Countries in the dataset


table(LAPOP$pais)

labels <- attr(LAPOP$pais, "labels")

#Some countries are not labeled, like Venezuela (16), Guyana (24)

if (!"Venezuela" %in% names(labels)) labels["Venezuela"] <- 16
if (!"Guyana" %in% names(labels)) labels["Guyana"] <- 24

labels <- labels[!is.na(labels)]

labels <- labels[order(labels)]

## Rename 
LAPOP$pais <- factor(LAPOP$pais, levels = unique(LAPOP$pais), labels = names(labels))

print(table(LAPOP$pais))  


# Keep only Latin American Countries (non Caribbean and not US or Canada)
countries_to_keep <- c("México", "Guatemala", "El Salvador", "Honduras", "Nicaragua", 
                       "Costa Rica", "Panamá", "Colombia", "Ecuador", "Bolivia", 
                       "Perú", "Paraguay", "Chile", "Uruguay", "Brasil", 
                       "Venezuela", "Argentina", "República Dominicana", "Haití")

LAPOP <- LAPOP %>% filter(pais %in% countries_to_keep)
table(LAPOP$pais)

LAPOP$pais <- droplevels(LAPOP$pais)

table(LAPOP$pais)
## There are a lot of variables that are only NA's, I am droping them for ease of analysis

LAPOP <- LAPOP %>%
  select(where(~ !all(is.na(.))))


save(LAPOP, file = "LAPOP.RData")

##___________________________________________________________________________##
## Second step - Very there are no missings in Key survey variables 


##There were some missing values in the sampling unit
## I decided to check by country 

sum(is.na(LAPOP$upm))
sum(is.na(LAPOP$estratopri))
sum(is.na(LAPOP$weight1500))

missing_values_by_country_year <- LAPOP %>%
  group_by(pais, year) %>%
  summarise(missing_upm = sum(is.na(upm)), .groups = 'drop') %>%
  arrange(desc(missing_upm))

print(missing_values_by_country_year)
# the missing values for upm are in three countries, which I will check

#First Ecuador
## I downloaded the Ecuador Data for 2008 from LAPOP to check the issue before 

LAPOP_Ecuador2008 <- LAPOP %>% filter(pais == "Ecuador" & year == 2008)
Ecuador2008 <- read_dta("Ecuador2008.dta") 

sum(is.na(LAPOP_Ecuador2008$upm))
sum(is.na(Ecuador2008$upm))
View(LAPOP_Ecuador2008)
View(Ecuador2008)


# The issue seems to be in the up coded as 88, that was re coded as missing in the merged data
### Fixing the issue with UPM

Ecuador2008_relevant <- Ecuador2008 %>% select(idnum, upm)


LAPOP_Ecuador2008 <- LAPOP_Ecuador2008 %>%
  left_join(Ecuador2008_relevant, by = "idnum", suffix = c("_LAPOP", "_Ecuador2008"))

# Update the UPM values where they were missing
LAPOP_Ecuador2008 <- LAPOP_Ecuador2008 %>%
  mutate(upm = if_else(is.na(upm_LAPOP), upm_Ecuador2008, upm_LAPOP)) %>%
  select(-upm_LAPOP, -upm_Ecuador2008)  # Drop the extra UPM columns

# Verify that the UPM values are corrected
sum(is.na(LAPOP_Ecuador2008$upm))  

# Reintegrate the corrected Ecuador 2008 data back into the LAPOP dataset
LAPOP <- LAPOP %>%
  filter(!(pais == "Ecuador" & year == 2008)) %>%
  bind_rows(LAPOP_Ecuador2008)


##__________________________________##
## The other case with an issue was Chile 2010
## I also downloaded the data for Chile 2010 from LAPOP to check the issue

LAPOP_Chile2010 <- LAPOP %>% filter(pais == "Chile" & year == 2010)
Chile2010 <- read_dta("Chile2010.dta") 

sum(is.na(LAPOP_Chile2010$upm))
sum(is.na(Chile2010$upm))
View(LAPOP_Chile2010)
View(Chile2010)

# In the case of Chile 88, 98 and 99 are read as missing
### Fixing the issue with UPM

Chile2010_relevant <- Chile2010 %>% select(idnum, upm)


LAPOP_Chile2010 <- LAPOP_Chile2010 %>%
  left_join(Chile2010_relevant, by = "idnum", suffix = c("_LAPOP", "_Chile2010"))

# Update the UPM values where they were missing
LAPOP_Chile2010 <- LAPOP_Chile2010 %>%
  mutate(upm = if_else(is.na(upm_LAPOP), upm_Chile2010, upm_LAPOP)) %>%
  select(-upm_LAPOP, -upm_Chile2010)  # Drop the extra UPM columns

# Verify that the UPM values are corrected
sum(is.na(LAPOP_Chile2010$upm))  

# Reintegrate the corrected Chile 2010 data back into the LAPOP dataset
LAPOP <- LAPOP %>%
  filter(!(pais == "Chile" & year == 2010)) %>%
  bind_rows(LAPOP_Chile2010)

##_____________________________##
## Finally, the last case with an issue was Dominican Republic for 2004
## I dowloaded the Dominican Republic 2004 data set from LAPOP to check the issue

RD2004 <- read_sav("RD2004.sav") 
RD_2004<- LAPOP %>%
  filter(pais == "República Dominicana" & year == 2004)
sum(is.na(RD2004$upm))
sum(is.na(RD_2004$upm))
View(RD2004)
View(RD_2004)

# The case of Dominican Republic is different, because in 2004 they did not include a Unidad Primaria de muestro in the variables
# I looked instead at the merged data of 2004, and found that Republica Dominicana was not included here
# Also, for the main merged data, the observations from Dominican Republic in 2004 do not have an idnum either which would make the matching of the upm impossible
# So I decided to drop the observations from Republica Dominica in the year 2004

# Filter out the observations from República Dominicana in 2004
LAPOP <- LAPOP %>%
  filter(!(pais == "República Dominicana" & year == 2004))

##________________________##

# As a final step I will check whether this fixes the issue with the missing data in UPM (PSUs)

missing_values_by_country_year <- LAPOP %>%
  group_by(pais, year) %>%
  summarise(missing_upm = sum(is.na(upm)), .groups = 'drop') %>%
  arrange(desc(missing_upm))

print(missing_values_by_country_year)



# _______________________##
#_________________________##

#Now, I want to check the missing values for the other key variables of the design, the strata and the weights 
# The webpage suggest using the estratopri variable as strata; but for cross country comparison the documentation recommends the strata variable

missing_values_by_country_year <- LAPOP %>%
  group_by(pais, year) %>%
  summarise(missing_upm = sum(is.na(strata)), .groups = 'drop') %>%
  arrange(desc(missing_upm))

print(missing_values_by_country_year)


missing_values_by_country_year <- LAPOP %>%
  group_by(pais, year) %>%
  summarise(missing_upm = sum(is.na(weight1500)), .groups = 'drop') %>%
  arrange(desc(missing_upm))

print(missing_values_by_country_year)
## no missing values

##Now, I will create a country/year identifier for ease of analysis

LAPOP<-LAPOP %>%
  mutate(country_year = paste(pais, year, sep = "_"))

LAPOP<-LAPOP%>%
  select(country_year, everything())


save(LAPOP, file = "LAPOP.RData")