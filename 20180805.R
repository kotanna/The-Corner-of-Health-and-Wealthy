# Summer Practicum: The Corner of Healthy and Wealthy
# Last updated: August 5, 2018 by Anna M. Kot (11:45 PM)

#### Import relevant libraries ####
library(alluvial);library(caret);library(corrplot)
library(countrycode);library(factoextra);library(FactoMineR)
library(NbClust);library(readxl);library(RColorBrewer)
library(scatterplot3d);library(stringr);library(scales)
library(dplyr);library(ggfortify)

gapminder_data <- NULL
par(mar = c(1,1,1,1))

# hyperparameters
num_of_components <- 2
dropCountries <- c('')
dropCols <- c('invest', 'military_exp', 'agriculture', 'fdi', 'health_spending', 'co2_emissions')

# set up function to collect yearly data (backwards-looking)
collectYears <- function(df, starting_index, num_years){
  for(i in num_years){
    new_col <- df[,c(starting_index)]
    new_col <- as.numeric(apply(df, 1, function(x){ifelse(is.na(unlist(x[starting_index])), unlist(x[starting_index-i]), unlist(x[starting_index]))}))
    return(new_col)
  }
}

# enter path to folder containing all datasets
path <- '/Users/annakot/Desktop/M.S. Analytics/01. Summer 2018/Data 803/Summer Practicum/DataFiles/'

# collect and clean years_in_school
male_school <- read_xlsx(paste0(path, 'Years in school men 25-34.xlsx'))
female_school <- read_xlsx(paste0(path, 'Years in school women 25-34.xlsx'))

m_cols <- c()
f_cols <- c()
avg_cols <- c()

for(i in 1970:2009){
  m_title <- paste0('m', i)
  m_cols <- c(m_cols, m_title)
  f_title <- paste0('f', i)
  f_cols <- c(f_cols, f_title)
  avg_col <- paste0('avg_', i)
  avg_cols <- c(avg_cols, avg_col)
}

colnames(male_school)[2:41] <- m_cols
colnames(female_school)[2:41] <- f_cols
rm(i, m_cols, m_title, f_cols, f_title)

# merge the male and female dataset into years in school dataframe
years_in_school <- merge(male_school,female_school, by = "Row Labels")
years_in_school <- cbind(years_in_school, as.data.frame(matrix(data = NA, nrow = 175, ncol = 40)))

# convert the years in school for each country based on gender ratio
additional_col_index <- 81

for(i in 1970:2009){
  additional_col_index <- additional_col_index + 1
  
  m_col_title <- paste0('m', i)
  f_col_title <- paste0('f', i)
  
  m_col_num <- (which(colnames(years_in_school) == m_col_title))
  f_col_num <- (which(colnames(years_in_school) == f_col_title))
  
  temp_data <- years_in_school[,c(m_col_num, f_col_num)]
  
  years_in_school[additional_col_index] <- rowMeans(temp_data)
  
}

colnames(years_in_school)[1] <- 'country'
colnames(years_in_school)[82:121] <- avg_cols

# convert the country column into the correct country name for the merge 
years_in_school$country <- str_replace(years_in_school$country,"Central African Rep.", "Central African Republic")
years_in_school$country <- str_replace(years_in_school$country,"Czech Rep.", "Czech Republic")
years_in_school$country <- str_replace(years_in_school$country,"Dominican Rep.", "Dominican Republic")
years_in_school$country <- str_replace(years_in_school$country, "Korea, Rep.", "South Korea")
years_in_school$country <- str_replace(years_in_school$country,"Kyrgyzstan","Kyrgyz Republic")
years_in_school$country <- str_replace(years_in_school$country,"Laos","Lao")
years_in_school$country <- str_replace(years_in_school$country, "Saint Lucia", "St. Lucia")
years_in_school$country <- str_replace(years_in_school$country, "Yemen, Rep.", "Yemen")

rm(male_school, female_school, temp_data)

years_in_school$data10 <- collectYears(years_in_school, which(colnames(years_in_school) == 'avg_2009'), 4)
years_in_school$data00 <- collectYears(years_in_school, which(colnames(years_in_school) == 'avg_2000'), 5)
years_in_school$data90 <- collectYears(years_in_school, which(colnames(years_in_school) == 'avg_1990'), 5)
years_in_school$data80 <- collectYears(years_in_school, which(colnames(years_in_school) == 'avg_1980'), 5)
years_in_school$data70 <- collectYears(years_in_school, which(colnames(years_in_school) == 'avg_1970'), 5)


# collect data from gapminder sources
# read the xlsx for the gdp data
# collect available data for either 2011, 2010, 2009, 2008, or 2007
gdp <- read_xlsx(paste0(path, '/GDPpercapitaconstant2000US.xlsx'))

# collecting data over from 2010 to 1970
gdp$data10 <- collectYears(gdp, which(colnames(gdp) == '2010'), 5)
gdp$data00 <- collectYears(gdp, which(colnames(gdp) == '2000'), 5)
gdp$data90 <- collectYears(gdp, which(colnames(gdp) == '1990'), 5)
gdp$data80 <- collectYears(gdp, which(colnames(gdp) == '1980'), 5)
gdp$data70 <- collectYears(gdp, which(colnames(gdp) == '1970'), 5)

colnames(gdp)[1] <- 'country'

# read the xlsx for the food_consumption data 
# name the first column as country
food_consumption <- read_xlsx(paste0(path, '/indicator food_consumption.xlsx')); 
colnames(food_consumption)[1] <- 'country'

# convert the country column into the correct country name for the merge 
food_consumption$country <- str_replace(food_consumption$country, "Czech Rep.", "Czech Republic")
food_consumption$country <- str_replace(food_consumption$country, "Central African Rep.", "Central African Republic")
food_consumption$country <- str_replace(food_consumption$country, "Dominican Rep.", "Dominican Republic")
food_consumption$country <- str_replace(food_consumption$country, "Kyrgyzstan", "Kyrgyz Republic")
food_consumption$country <- str_replace(food_consumption$country, "Laos", "Lao")
food_consumption$country <- str_replace(food_consumption$country, "Saint Lucia", "St. Lucia")
food_consumption$country <- str_replace(food_consumption$country, "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines")
food_consumption$country <- str_replace(food_consumption$country, "Yemen, Rep.", "Yemen")
food_consumption$country <- str_replace(food_consumption$country, "Cook Islands", "Cook Is")
food_consumption$country <- str_replace(food_consumption$country, "Korea, Dem. Rep.", "North Korea")
food_consumption$country <- str_replace(food_consumption$country, "Korea, Rep.", "South Korea")
food_consumption$country <- str_replace(food_consumption$country, "Korea, United", "United Korea (former)")
food_consumption$country <- str_replace(food_consumption$country, "Saint Barth?lemy", "St. Barth?lemy")
food_consumption$country <- str_replace(food_consumption$country, "Saint Helena", "St. Helena")
food_consumption$country <- str_replace(food_consumption$country, "Saint Kitts and Nevis", "St. Kitts and Nevis")
food_consumption$country <- str_replace(food_consumption$country, "Saint Martin", "St. Martin")
food_consumption$country <- str_replace(food_consumption$country, "Saint-Pierre-et-Miquelon", "St.-Pierre-et-Miquelon")

# collecting data over from 2010 to 1970
food_consumption$data10 <- collectYears(food_consumption, which(colnames(food_consumption) == '2007.0'), 5)
food_consumption$data00 <- collectYears(food_consumption, which(colnames(food_consumption) == '2000.0'), 5)
food_consumption$data90 <- collectYears(food_consumption, which(colnames(food_consumption) == '1990.0'), 5)
food_consumption$data80 <- collectYears(food_consumption, which(colnames(food_consumption) == '1980.0'), 5)
food_consumption$data70 <- collectYears(food_consumption, which(colnames(food_consumption) == '1970.0'), 5)

# read the xlsx for the infant_mortality data
# name the first column as country
infant_mortality <- read_xlsx(paste0(path, '/indicator gapminder infant_mortality.xlsx')); 
infant_mortality <- infant_mortality[,c(1, 137:217)]
colnames(infant_mortality)[1] <- 'country'
infant_mortality <- cbind(infant_mortality[1], apply(infant_mortality[,c(2:ncol(infant_mortality))], 2, function(x){str_replace(x, "-", "")}))
colnames(infant_mortality)[1] <- 'country'

# WE GOT TO HERE. HAVE TO REPLACE ALL BLANKS WITH NA. #

# collecting data over from 2010 to 1970
infant_mortality$data10 <- collectYears(infant_mortality, which(colnames(infant_mortality) == '2010.0'), 5)
infant_mortality$data00 <- collectYears(infant_mortality, which(colnames(infant_mortality) == '2000.0'), 5)
infant_mortality$data90 <- collectYears(infant_mortality, which(colnames(infant_mortality) == '1990.0'), 5)
infant_mortality$data80 <- collectYears(infant_mortality, which(colnames(infant_mortality) == '1980.0'), 5)
infant_mortality$data70 <- collectYears(infant_mortality, which(colnames(infant_mortality) == '1970.0'), 5)


# read the xlsx for the life_expectancy data 
# name the first column as country 
life_expectancy <- read_xlsx(paste0(path, '/indicator life_expectancy_at_birth.xlsx')); 
colnames(life_expectancy)[1] <- 'country'

# collecting data over from 2010 to 1970
life_expectancy$data10 <- collectYears(life_expectancy, which(colnames(life_expectancy) == '2010.0'), 5)
life_expectancy$data00 <- collectYears(life_expectancy, which(colnames(life_expectancy) == '2000.0'), 5)
life_expectancy$data90 <- collectYears(life_expectancy, which(colnames(life_expectancy) == '1990.0'), 5)
life_expectancy$data80 <- collectYears(life_expectancy, which(colnames(life_expectancy) == '1980.0'), 5)
life_expectancy$data70 <- collectYears(life_expectancy, which(colnames(life_expectancy) == '1970.0'), 5)


# read the xlsx for the internet_users data
# name the first column as country
internet_users <- read_xlsx(paste0(path, '/Internet user per 100.xlsx')); 
colnames(internet_users)[1] <- 'country'

# collecting data over from 2010 to 1970
internet_users$data10 <- collectYears(internet_users, which(colnames(internet_users) == '2010'), 5)
internet_users$data00 <- collectYears(internet_users, which(colnames(internet_users) == '2000'), 5)
internet_users$data90 <- collectYears(internet_users, which(colnames(internet_users) == '1990'), 5)

# these years do not exist
#internet_users$data80 <- collectYears(internet_users, which(colnames(internet_users) == '1980'), 5)
#internet_users$data70 <- collectYears(internet_users, which(colnames(internet_users) == '1970'), 5)


# read the xlsx for the agriculture data
# name the first column as country
agriculture <- read_xlsx(paste0(path, '/agriculture.xlsx')); 
colnames(agriculture)[1] <- 'country'

# collecting data over from 2010 to 1970
agriculture$data10 <- collectYears(agriculture, which(colnames(agriculture) == '2010'), 5)
agriculture$data00 <- collectYears(agriculture, which(colnames(agriculture) == '2000'), 5)
agriculture$data90 <- collectYears(agriculture, which(colnames(agriculture) == '1990'), 5)
agriculture$data80 <- collectYears(agriculture, which(colnames(agriculture) == '1980'), 5)
agriculture$data70 <- collectYears(agriculture, which(colnames(agriculture) == '1970'), 5)


# read the xlsx for the cell phone data
# name the first column as country
cell_phone <- read_xlsx(paste0(path, '/cell phones (per 100 people).xlsx')); 
colnames(cell_phone)[1] <- 'country'

# collecting data over from 2010 to 1970
cell_phone$data10 <- collectYears(cell_phone, which(colnames(cell_phone) == '2010'), 5)
cell_phone$data00 <- collectYears(cell_phone, which(colnames(cell_phone) == '2000'), 5)
cell_phone$data90 <- collectYears(cell_phone, which(colnames(cell_phone) == '1990'), 5)
cell_phone$data80 <- collectYears(cell_phone, which(colnames(cell_phone) == '1980'), 5)
cell_phone$data70 <- collectYears(cell_phone, which(colnames(cell_phone) == '1970'), 5)


# read the xlsx for the child mortality
# name the first column as country
child_mortality <- read_xlsx(paste0(path, '/child mortality.xlsx')); 
colnames(child_mortality)[1] <- 'country'

# collecting data over from 2010 to 1970
child_mortality$data10 <- collectYears(child_mortality, which(colnames(child_mortality) == '2010.0'), 5)
child_mortality$data00 <- collectYears(child_mortality, which(colnames(child_mortality) == '2000.0'), 5)
child_mortality$data90 <- collectYears(child_mortality, which(colnames(child_mortality) == '1990.0'), 5)
child_mortality$data80 <- collectYears(child_mortality, which(colnames(child_mortality) == '1980.0'), 5)
child_mortality$data70 <- collectYears(child_mortality, which(colnames(child_mortality) == '1970.0'), 5)

# read the xlsx for the children per women
# name the first column as country
children_per_women <- read_xlsx(paste0(path, '/children per woman (total fertility).xlsx')); 
colnames(children_per_women)[1] <- 'country'

# collecting data over from 2010 to 1970
children_per_women$data10 <- collectYears(children_per_women, which(colnames(children_per_women) == '2010.0'), 5)
children_per_women$data00 <- collectYears(children_per_women, which(colnames(children_per_women) == '2000.0'), 5)
children_per_women$data90 <- collectYears(children_per_women, which(colnames(children_per_women) == '1990.0'), 5)
children_per_women$data80 <- collectYears(children_per_women, which(colnames(children_per_women) == '1980.0'), 5)
children_per_women$data70 <- collectYears(children_per_women, which(colnames(children_per_women) == '1970.0'), 5)

# read the xlsx for the CO2 emissions
# name the first column as country
co2_emissions <- read_xlsx(paste0(path, '/cumulative CO2 emissions (tonnes).xlsx')); 
colnames(co2_emissions)[1] <- 'country'

# collecting data over from 2010 to 1970
co2_emissions$data10 <- collectYears(co2_emissions, which(colnames(co2_emissions) == '2010.0'), 5)
co2_emissions$data00 <- collectYears(co2_emissions, which(colnames(co2_emissions) == '2000.0'), 5)
co2_emissions$data90 <- collectYears(co2_emissions, which(colnames(co2_emissions) == '1990.0'), 5)
co2_emissions$data80 <- collectYears(co2_emissions, which(colnames(co2_emissions) == '1980.0'), 5)
co2_emissions$data70 <- collectYears(co2_emissions, which(colnames(co2_emissions) == '1970.0'), 5)

# read the xlsx for the fdi 
# name the first column as country
fdi<- read_xlsx(paste0(path, '/foreign direct investment, net inflows (% of GDP).xlsx')); 
colnames(fdi)[1] <- 'country'

fdi$country <- str_replace(fdi$country, "Central African Rep.", "Central African Republic")
fdi$country <- str_replace(fdi$country, "Czech Rep.", "Czech Republic")
fdi$country <- str_replace(fdi$country, "Dominican Rep.", "Dominican Republic")
fdi$country <- str_replace(fdi$country, "Korea, Dem. Rep.", "North Korea")
fdi$country <- str_replace(fdi$country, "Korea, Rep.", "South Korea")
fdi$country <- str_replace(fdi$country, "Laos", "Lao")
fdi$country <- str_replace(fdi$country, "Saint Kitts and Nevis", "St. Kitts and Nevis")
fdi$country <- str_replace(fdi$country, "Saint Lucia", "St. Lucia")
fdi$country <- str_replace(fdi$country, "Saint Martin", "St. Martin")
fdi$country <- str_replace(fdi$country, "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines")
fdi$country <- str_replace(fdi$country, "Yemen, Rep.", "Yemen")

# collecting data over from 2010 to 1970
# the data is a % of GDP
fdi$data10 <- collectYears(fdi, which(colnames(fdi) == '2010'), 5)
fdi$data00 <- collectYears(fdi, which(colnames(fdi) == '2000'), 5)
fdi$data90 <- collectYears(fdi, which(colnames(fdi) == '1990'), 5)
fdi$data80 <- collectYears(fdi, which(colnames(fdi) == '1980'), 5)

# data does not exist for 1970s
#fdi$data70 <- collectYears(fdi, which(colnames(fdi) == '1970'), 5)


# read the xlsx for the invest 
# name the first column as country
invest<- read_xlsx(paste0(path, '/investments (% of GDP).xlsx')); 
colnames(invest)[1] <- 'country'

invest$country <- str_replace(invest$country, "Central African Rep.", "Central African Republic")
invest$country <- str_replace(invest$country, "Czech Rep.", "Czech Republic")
invest$country <- str_replace(invest$country, "Dominican Rep.", "Dominican Republic")
invest$country <- str_replace(invest$country, "Korea, Dem. Rep.", "North Korea")
invest$country <- str_replace(invest$country, "Korea, Rep.", "South Korea")
invest$country <- str_replace(invest$country, "Laos", "Lao")
invest$country <- str_replace(invest$country, "Saint Kitts and Nevis", "St. Kitts and Nevis")
invest$country <- str_replace(invest$country, "Saint Lucia", "St. Lucia")
invest$country <- str_replace(invest$country, "Saint Martin", "St. Martin")
invest$country <- str_replace(invest$country, "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines")
invest$country <- str_replace(invest$country, "Yemen, Rep.", "Yemen")

# collecting data over from 2010 to 1970
# the data is a % of GDP
invest$data10 <- collectYears(invest, which(colnames(invest) == '2010'), 5)
invest$data00 <- collectYears(invest, which(colnames(invest) == '2000'), 5)
invest$data90 <- collectYears(invest, which(colnames(invest) == '1990'), 5)
invest$data80 <- collectYears(invest, which(colnames(invest) == '1980'), 5)
invest$data70 <- collectYears(invest, which(colnames(invest) == '1970'), 5)

# read the xlsx for the military expenditure 
# name the first column as country
military_exp<- read_xlsx(paste0(path, '/military expenditure (% of GDP).xlsx')); 
colnames(military_exp)[1] <- 'country'

# collecting data over from 2010 to 1970
# the data is a % of GDP
military_exp$data10 <- collectYears(military_exp, which(colnames(military_exp) == '2010'), 5)
military_exp$data00 <- collectYears(military_exp, which(colnames(military_exp) == '2000'), 5)
military_exp$data90 <- collectYears(military_exp, which(colnames(military_exp) == '1990'), 5)

# data does not exist for 1980 to 1970
#military_exp$data80 <- collectYears(military_exp, which(colnames(military_exp) == '1980'), 5)
#military_exp$data70 <- collectYears(military_exp, which(colnames(military_exp) == '1970'), 5)


# read the xlsx for the population growth
# name the first column as country
population_growth<- read_xlsx(paste0(path, '/population growth (annual 5).xlsx')); 
colnames(population_growth)[1] <- 'country'

# collecting data over from 2010 to 1970
# the data is a % of GDP
population_growth$data10 <- collectYears(population_growth, which(colnames(population_growth) == '2010'), 5)
population_growth$data00 <- collectYears(population_growth, which(colnames(population_growth) == '2000'), 5)
population_growth$data90 <- collectYears(population_growth, which(colnames(population_growth) == '1990'), 5)
population_growth$data80 <- collectYears(population_growth, which(colnames(population_growth) == '1980'), 5)
population_growth$data70 <- collectYears(population_growth, which(colnames(population_growth) == '1970'), 5)


# read the xlsx for the total health spending per person 
# name the first column as country
health_spending<- read_xlsx(paste0(path, '/total health spending per person (US$).xlsx')); 
colnames(health_spending)[1] <- 'country'

# collecting data over from 2010 to 1970
# the data is in US dollars
health_spending$data10 <- collectYears(health_spending, which(colnames(health_spending) == '2010.0'), 5)
health_spending$data00 <- collectYears(health_spending, which(colnames(health_spending) == '2000.0'), 5)

# data does not exist for 1990 to 1970
#health_spending$data90 <- collectYears(health_spending, which(colnames(health_spending) == '1990.0'), 5)
#health_spending$data80 <- collectYears(health_spending, which(colnames(health_spending) == '1980.0'), 5)
#health_spending$data70 <- collectYears(health_spending, which(colnames(health_spending) == '1970.0'), 5)


# read the xlsx for the total health spending per person 
# name the first column as country
urban_pop<- read_xlsx(paste0(path, '/urban population (% of total).xlsx')); 
colnames(urban_pop)[1] <- 'country'

# collecting data over from 2010 to 1970
# the data is in US dollars
urban_pop$data10 <- collectYears(urban_pop, which(colnames(urban_pop) == '2010'), 5)
urban_pop$data00 <- collectYears(urban_pop, which(colnames(urban_pop) == '2000'), 5)
urban_pop$data90 <- collectYears(urban_pop, which(colnames(urban_pop) == '1990'), 5)
urban_pop$data80 <- collectYears(urban_pop, which(colnames(urban_pop) == '1980'), 5)
urban_pop$data70 <- collectYears(urban_pop, which(colnames(urban_pop) == '1970'), 5)

#### Merging data sets #####

# create list of countries
# name the first column as country 
gapminder_countries <- as.data.frame(internet_users[,c(1)])
colnames(gapminder_countries)[1] <- 'country'

# merge with years_in_school
test <- merge(gapminder_countries, years_in_school[,c(1, which(colnames(years_in_school) == 'data10'):which(colnames(years_in_school) == 'data70'))], all.x = T)
df_name <- 'years_in_school'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with gdp
test <- merge(test, gdp[,c(1, which(colnames(gdp) == 'data10'):which(colnames(gdp) == 'data70'))], all.x = T)
df_name <- 'gdp'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with food consumption
test <- merge(test, food_consumption[,c(1, which(colnames(food_consumption) == 'data10'):which(colnames(food_consumption) == 'data70'))], all.x = T)
df_name <- 'food_consumption'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with infant mortality
test <- merge(test, infant_mortality[,c(1, which(colnames(infant_mortality) == 'data10'):which(colnames(infant_mortality) == 'data70'))], all.x = T)
df_name <- 'infant_mortality'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with life expectancy
test <- merge(test, life_expectancy[,c(1, which(colnames(life_expectancy) == 'data10'):which(colnames(life_expectancy) == 'data70'))], all.x = T)
df_name <- 'life_expectancy'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with internet users
test <- merge(test, internet_users[,c(1, which(colnames(internet_users) == 'data10'):which(colnames(internet_users) == 'data90'))], all.x = T)
df_name <- 'internet_users'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')

# merge with agriculture
test <- merge(test, agriculture[,c(1, which(colnames(agriculture) == 'data10'):which(colnames(agriculture) == 'data70'))], all.x = T)
df_name <- 'agriculture'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with cell_phone
test <- merge(test, cell_phone[,c(1, which(colnames(cell_phone) == 'data10'):which(colnames(cell_phone) == 'data90'))], all.x = T)
df_name <- 'cell_phone'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')

# merge with child_mortality
test <- merge(test, child_mortality[,c(1, which(colnames(child_mortality) == 'data10'):which(colnames(child_mortality) == 'data70'))], all.x = T)
df_name <- 'child_mortality'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with children per woman
test <- merge(test, children_per_women[,c(1, which(colnames(children_per_women) == 'data10'):which(colnames(children_per_women) == 'data70'))], all.x = T)
df_name <- 'children_per_women'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with CO2
test <- merge(test, co2_emissions[,c(1, which(colnames(co2_emissions) == 'data10'):which(colnames(co2_emissions) == 'data70'))], all.x = T)
df_name <- 'co2_emissions'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with fdi
test <- merge(test, fdi[,c(1, which(colnames(fdi) == 'data10'):which(colnames(fdi) == 'data80'))], all.x = T)
df_name <- 'fdi'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')

# merge with invest
test <- merge(test, invest[,c(1, which(colnames(invest) == 'data10'):which(colnames(invest) == 'data70'))], all.x = T)
df_name <- 'invest'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with miltary exp
test <- merge(test, military_exp[,c(1, which(colnames(military_exp) == 'data10'):which(colnames(military_exp) == 'data90'))], all.x = T)
df_name <- 'military_exp'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')

# merge with population_growth
test <- merge(test, population_growth[,c(1, which(colnames(population_growth) == 'data10'):which(colnames(population_growth) == 'data70'))], all.x = T)
df_name <- 'population_growth'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# merge with health spending
test <- merge(test, health_spending[,c(1, which(colnames(health_spending) == 'data10'):which(colnames(health_spending) == 'data00'))], all.x = T)
df_name <- 'health_spending'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')

# merge with urban pop
test <- merge(test, urban_pop[,c(1, which(colnames(urban_pop) == 'data10'):which(colnames(urban_pop) == 'data70'))], all.x = T)
df_name <- 'urban_pop'
colnames(test)[which(colnames(test) == 'data10')] <- paste0(df_name, '_data_10')
colnames(test)[which(colnames(test) == 'data00')] <- paste0(df_name, '_data_00')
colnames(test)[which(colnames(test) == 'data90')] <- paste0(df_name, '_data_90')
colnames(test)[which(colnames(test) == 'data80')] <- paste0(df_name, '_data_80')
colnames(test)[which(colnames(test) == 'data70')] <- paste0(df_name, '_data_70')

# drop countries that we want to exclude - from hyperparameter
test <- test[!(test$country %in% dropCountries),]

# split test df into decade-by-decade test dfs
test10 <- test[,c(1, which(grepl(pattern = '10$', x = colnames(test))))]
test00 <- test[,c(1, which(grepl(pattern = '00$', x = colnames(test))))]
test90 <- test[,c(1, which(grepl(pattern = '90$', x = colnames(test))))]
test80 <- test[,c(1, which(grepl(pattern = '80$', x = colnames(test))))]
test70 <- test[,c(1, which(grepl(pattern = '70$', x = colnames(test))))]

# drop columns with too many NAs - from hyperparameter
dropCols10 <-  paste0(dropCols, "_data_", "10")
test10 <- test10[complete.cases(test10[, !names(test10) %in% dropCols10]), ]
test10 <- test10[, !names(test10) %in% dropCols10]

dropCols00 <- paste0(dropCols, "_data_", "00")
test00 <- test00[complete.cases(test00[, !names(test00) %in% dropCols00]), ]
test00 <- test00[, !names(test00) %in% dropCols00]

dropCols90 <- paste0(dropCols, "_data_", "90")
test90 <- test90[complete.cases(test90[, !names(test90) %in% dropCols90]), ]
test90 <- test90[, !names(test90) %in% dropCols90]

dropCols80 <- paste0(dropCols, "_data_", "80")
test80 <- test80[complete.cases(test80[, !names(test80) %in% dropCols80]), ]
test80 <- test80[, !names(test80) %in% dropCols80]

dropCols70 <- paste0(dropCols, "_data_", "70")
test70 <- test70[complete.cases(test70[, !names(test70) %in% dropCols70]), ]
test70 <- test70[, !names(test70) %in% dropCols70]

test_merged_10 <- test10
test_merged_00 <- merge(test10, test00, by = "country")
test_merged_90 <- merge(test_merged_00, test90, by = "country")
test_merged_80 <- merge(test_merged_90, test80, by = "country")
test_merged_70 <- merge(test_merged_80, test70, by = "country")

test10$year <- 2010
test00$year <- 2000
test90$year <- 1990

countries_to_keep <- unique(test_merged_90$country)
test10 <- filter(test10, country %in% countries_to_keep)
test00 <- filter(test00, country %in% countries_to_keep)
test90 <- filter(test90, country %in% countries_to_keep)

colnames(test10)[2:ncol(test10)] <- str_replace(names(test10)[2:ncol(test10)], "_data_\\d\\d$", "")
colnames(test00)[2:ncol(test00)] <- str_replace(names(test00)[2:ncol(test00)], "_data_\\d\\d$", "")
colnames(test90)[2:ncol(test90)] <- str_replace(names(test90)[2:ncol(test90)], "_data_\\d\\d$", "")

unscaled10 <- test10
unscaled00 <- test00
unscaled90 <- test90

scaling <- preProcess(x = test10[, !names(test10) %in% c('country', 'year')], method = c('center', 'scale'))
test10 <- predict(scaling, test10)
test00 <- predict(scaling, test00)
test90 <- predict(scaling, test90)

gapminder_data <- rbind(test10, test00, test90)
num_countries <- unname(table(gapminder_data$year)[1])

names.var <- ("Variable" = c("Country", "Years in school", "GDP", "Food consumption", "Infant mortality","Life expectancy","Internet users","Cell phone users","Child mortality","Children per women","Population growth","Urban population", "Year"))
colnames(gapminder_data) <- names.var
colnames(unscaled10) <- names.var
colnames(unscaled00) <- names.var
colnames(unscaled90) <- names.var

#### Compute correlation matrix ####
col.UNH <- colorRampPalette(c("#cb4d0b", "#f77a05", "#FFCC64", "#FFFFD4", "#ffffff", "#DBDEFF", "#7F8DFF", "#0044bb", "#001d52"))

corr10 <- round(cor(unscaled10[,2:12]),2)
#corr00 <- round(cor(unscaled00[,2:12]),2)
#corr90 <- round(cor(unscaled90[,2:12]),2)

#2010 (saved to working directory)
#png('corr10.png', width = 1920, height = 1280, units="px", res=NA)
#corrplot(corr10, addgrid.col= "#003591", bg = "#FBFCFF", type = "lower", order = "hclust", tl.col = "#003591", tl.srt = 45, col = col.UNH(100))
#dev.off()

# #2000 (saved to working directory)
# tiff('corr00.jpeg', units="in", width=6, height=6, res=1200)
# corrplot(corr00, addgrid.col= "#003591", bg = "#FBFCFF", type = "lower", order = "hclust", tl.col = "#003591", tl.srt = 45, col = col.UNH(100))
# dev.off()
# 
# #1990 (saved to working directory)
# tiff('corr90.jpeg', units="in", width=6, height=6, res=1200)
# corrplot(corr90, addgrid.col= "#003591", bg = "#FBFCFF", type = "lower", order = "hclust", tl.col = "#003591", tl.srt = 45, col = col.UNH(100))
# dev.off()

#### Compute PCA ####

# Proceed with principal components using
# covariance matrix on isolated year range (2010).
pca.2010 <- PCA(gapminder_data[1:num_countries,2:(which(names(gapminder_data)=='Year')-1)], scale = FALSE, graph = FALSE)

# Visualize eigenvalues using a (scree plot), and show the
# percentage of variances explained by each principal component of 2010.
# Eigenvalues measure the amount of variation retained by each principal component
#tiff('plot_scree.jpeg', units="in", width=6, height=6, res=1200)
# plot.scree <- fviz_screeplot(pca.2010,
#                               linecolor = "#001d52",
#                               ncp = 5,
#                               barcolor = NA,
#                               barfil = c(
#                                 "#f77a05",
#                                 "#f77a05",
#                                 "#0044bb",
#                                 "#0044bb",
#                                 "#0044bb"),
#                                 # "#001d52",
#                                 # "#001d52",
#                                 # "#001d52",
#                                 # "#001d52",
#                                 # "#001d52"),
#                               sort = "DESC",
#                               addlabels = F,
#                               ylim = c(0, 70),
#                               xlab = "",
#                               ylab = "",
#                               yticks.by = 10,
#                               title = "") + theme_minimal()
# 
# plot.scree[["data"]][["dim"]] <- c("PC1","PC2","PC3","PC4","PC5")
# plot.scree
#dev.off()

# 2 components explain ~80% variance.
summary(pca.2010)

# # Function to isolate as many PCs as we ask
# isolatePC <- function(pca_object, num_components){
#   PClist <- matrix(nrow = ncol(pca_object$rotation), ncol = 0)
#   
#   for(i in 1:num_components){
#     PClist <- cbind(PClist, unname(pca_object$rotation[1:nrow(pca_object$rotation),i]))
#   }
#   
#   PClist <- as.data.frame(PClist)
#   
#   for(i in 1:num_components){
#     colName <- paste0('PC', i)
#     colnames(PClist)[i] <- colName
#   }
#   
#   for(i in 1:nrow(pca_object$rotation)){
#     rowName <- rownames(pca_object$rotation)[i]
#     rownames(PClist)[i] <- rowName
#   }
#   
#   return(as.data.frame(t(PClist)))
# }

# Plot of total contribution of variables to PC1 & PC2
# pca.2010$var$cos2[,1] <- pca.2010$var$cos2[,1] * 100
# pca.2010$var$cos2[,2] <- pca.2010$var$cos2[,2] * 100
# pca.2010$var$cos2[,1] <- pca.2010$var$cos2[,1] + pca.2010$var$cos2[,2]
# 
# contrib.pcaTotal <- fviz_cos2(pca.2010, color = "#0044bb", fill = "#0044bb", choice = "var", axes = 1, top = 11, title = "") + theme_minimal()
# 
# ggpubr::ggpar(contrib.pcaTotal,
#               xlab = "", ylab = "",
#               ggtheme = theme_minimal(), palette = "jco",
#               yticks.by = 10, ticks = FALSE,
#               x.text.angle = 45
# )

# Isolate n principal components as respective vectors.
#PC_df <- isolatePC(pca_object = pca.2010, num_components = num_of_components)
PC_df <- (pca.2010$svd$V[,1:2])
PC_df <- t(PC_df)
rownames(PC_df) <- c("PC1", "PC2")
colnames(PC_df) <- c("Years in school", "GDP", "Food consumption", "Infant mortality","Life expectancy","Internet users","Cell phone users","Child mortality","Children per women","Population growth","Urban population")

# Calculate coordinates using principal components from 2010.
PC_coordinates <- as.data.frame(matrix(nrow = nrow(gapminder_data), ncol = 0))

for(comp in 1:nrow(PC_df)){
  
  mult_PC <- rep(0, nrow(gapminder_data))
  
  for(i in which(colnames(gapminder_data)=='Years in school'):(which(colnames(gapminder_data)=='Year')-1)){
    
    feature <- colnames(gapminder_data[i])
    PC_col_index <- which(colnames(PC_df)==feature)
    
    mult_PC <- mult_PC + (gapminder_data[,c(i)] * PC_df[comp, feature])
  }
  
  PC_coordinates <- cbind(PC_coordinates, mult_PC)
  colnames(PC_coordinates)[ncol(PC_coordinates)] <- paste0('PC', comp)
}

gapminder_data <- cbind(gapminder_data, PC_coordinates)

# Retain the calculation of the first two principal components for each 
# year range denoting their coordinates.

# 2010
GM2010 <- gapminder_data[gapminder_data$Year==2010,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(GM2010) <- GM2010$Country
GM2010$Country <- NULL

# 2000
GM2000 <- gapminder_data[gapminder_data$Year==2000,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(GM2000) <- GM2000$Country
GM2000$Country <- NULL

# 1990
GM1990 <- gapminder_data[gapminder_data$Year==1990,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(GM1990) <- GM1990$Country
GM1990$Country <- NULL

# Plot and visualize the first two principal components for each 
# year range denoting their coordinates.

# 2010
#plot(GM2010, pch=16, col=rgb(0,0,0,0.5), main = '2010')
# 2000
#plot(GM2000, pch=16, col=rgb(0,0,0,0.5), main = '2000')
# 1990
#plot(GM1990, pch=16, col=rgb(0,0,0,0.5), main = '1990')

# Impute coordinates for each year into separate PCA analysises
# for the first n principal components.
pca.2010 <- PCA(gapminder_data[1:num_countries,2:(which(names(gapminder_data)=='Year')-1)], scale = FALSE, graph = FALSE, ncp = 2)
pca.2000 <- pca.2010
pca.1990 <- pca.2010

# 2000
GM2000 <- as.matrix(GM2000)
for(i in 1:num_countries){
  for(j in 1:num_of_components){
    pca.2000$ind$coord[i,j] <- GM2000[i,j]
  }
}

# 1990
GM1990 <- as.matrix(GM1990)
for(i in 1:num_countries){
  for(j in 1:num_of_components){
    pca.1990$ind$coord[i,j] <- GM1990[i,j]
  }
}

#### Access PCA results ####
# Eigenvalues for the first two principal components
eig.val <- get_eigenvalue(pca.2010)
eig.val <- eig.val[(1:num_of_components), ]
eig.val

#### Isolate PCA coordinates ####

# 2010
rm2010 <- gapminder_data[gapminder_data$Year==2010,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(pca.2010$ind$coord) <- rm2010$Country

names.var <- ("Variable" = c("Years in school", "GDP", "Food consumption", "Infant mortality","Life expectancy","Internet users","Cell phone users","Child mortality","Children per women","Population growth","Urban population"))
rownames(pca.2010$svd$V) <- names.var

# 2000
rm2000 <- gapminder_data[gapminder_data$Year==2000,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(pca.2000$ind$coord) <- rm2000$Country

# 1990
rm1990 <- gapminder_data[gapminder_data$Year==1990,c(1,which(colnames(gapminder_data)=='PC1'):ncol(gapminder_data))]
rownames(pca.1990$ind$coord) <- rm1990$Country

#### Determine best K ####
# 
# 2010
# Elbow method
# fviz_nbclust(GM2010, kmeans, method = "wss", linecolor="#0044bb") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "", title="")

# # Silhouette method
# fviz_nbclust(GM2010, kmeans, method = "silhouette",linecolor="#0044bb")+
#   labs(subtitle = "", title="")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(GM2010, kmeans, nstart = 25,  method = "gap_stat", nboot = 500,linecolor="#0044bb")+
#   labs(subtitle = "", title="")
# 
# # 2000
# # Elbow method
# fviz_nbclust(GM2000, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# # Silhouette method
# fviz_nbclust(GM2000, kmeans, method = "silhouette")+
#   labs(subtitle = "Silhouette method")
# 
# # Gap statistic
# # nboot = 50 to keep the function speedy. 
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(123)
# #fviz_nbclust(GM2000, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
#   labs(subtitle = "Gap statistic method")
# 
# # 1990
# # Elbow method
# fviz_nbclust(GM1990, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "", title="")
# 
# # Silhouette method
# fviz_nbclust(GM1990, kmeans, method = "silhouette")+
#   labs(subtitle = "", title="")
# 
# # Gap statistic
# # nboot = 50 to keep the function speedy.
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(GM1990, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
#   labs(subtitle = "Gap statistic method")

#### k-Means 10 ####

# k = 3
set.seed(1)
k_3 <- kmeans(GM2010, 3, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# Plot
set.seed(1)
autoplot(kmeans(GM2010, 3, nstart=25, iter.max=1000), data = GM2010, legendLabs = "Cluster")

col.UNH <- colorRampPalette(c("#cb4d0b", "#f77a05", "#FFCC64", "#FFFFD4", "#ffffff", "#DBDEFF", "#7F8DFF", "#0044bb", "#001d52"))


# fviz_cluster(k_3, data = GM2010,
#              palette = c("#cb4d0b", "#f77a05", "#FFCC64"), ellipse.type = "",
#              ggtheme = theme_minimal(), xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", main="", geom="point",
#              xlim = c(-3,3), ylim = c(-4,4)
# )

fviz_cluster(k_7_10, data = GM2010,
             palette = c("#cb4d0b", "#f77a05", "#FFCC64", "#7F8DFF","#0044bb","#001d52","#DBDEFF"), ellipse.type = "",
             ggtheme = theme_minimal(), xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", main="", geom="point",
             xlim = c(-2.5,2.5), ylim = c(-2.5,2.5)
)


fviz_cluster(k_7_10, data = GM2010,
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid",
             ggtheme = theme_minimal(), xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", main="", geom="point"
)

# # k = 4
# set.seed(1)
# k_4 <- kmeans(GM2010, 4, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# autoplot(k_4, data = GM2010)
# 
# k = 5
set.seed(1)
k_5 <- kmeans(GM2010, 5, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# autoplot(k_5, data = GM2010)
# 
# # k = 6
# set.seed(1)
# k_6 <- kmeans(GM2010, 6, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# autoplot(k_6, data = GM2010)

# k = 7
set.seed(1)
k_7_10 <- kmeans(GM2010, 7, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# # k = 9
# set.seed(1)
# k_9 <- kmeans(GM2010, 9, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))

gapminder_2010 <- filter(gapminder_data,Year==2010)

# input clusters into the the gapminder_data frame as a factor format
# gapminder_2010$k_3 <- as.factor(k_3$cluster)
# gapminder_2010$k_4 <- as.factor(k_4$cluster)
gapminder_2010$k_5 <- as.factor(k_5$cluster)
# gapminder_2010$k_6 <- as.factor(k_6$cluster)
gapminder_2010$k_7 <- as.factor(k_7_10$cluster)
# gapminder_2010$k_9 <- as.factor(k_9$cluster)

#### k-Means 00 ####

# # k = 3
# set.seed(1)
# k_3 <- kmeans(GM2000, 3, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_3, data = GM2000)
# 
# # k = 4
# set.seed(1)
# k_4 <- kmeans(GM2000, 4, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_3, data = GM2000)
# 
# # k = 5
# set.seed(1)
# k_5 <- kmeans(GM2000, 5, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_4, data = GM2000)
# 
# # k = 6
# set.seed(1)
# k_6 <- kmeans(GM2000, 6, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_6, data = GM2000)

# k = 7
set.seed(1)
k_7_00 <- kmeans(GM2000, 7, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# Plot
#set.seed(1)
#autoplot(k_6, data = GM2000)

# # k = 8
# set.seed(1)
# k_8 <- kmeans(GM2000, 8, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))

# Plot
#set.seed(1)
#autoplot(k_6, data = GM2000)

gapminder_2000 <- filter(gapminder_data,Year==2000)

# input clusters into the the gapminder_data frame as a factor format
# gapminder_2000$k_3 <- as.factor(k_3$cluster)
# gapminder_2000$k_4 <- as.factor(k_4$cluster)
# gapminder_2000$k_5 <- as.factor(k_5$cluster)
# gapminder_2000$k_6 <- as.factor(k_6$cluster)
gapminder_2000$k_7 <- as.factor(k_7_00$cluster)
# gapminder_2000$k_8 <- as.factor(k_8$cluster)

#### k-Means 90 ####

# k = 3
set.seed(1)
k_3 <- kmeans(GM1990, 3, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_3, data = GM1990, frame = TRUE)
# 
# # k = 4
# set.seed(1)
# k_4 <- kmeans(GM1990, 4, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_4, data = GM1990)
# 
# k = 5
set.seed(1)
k_5 <- kmeans(GM1990, 5, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# # Plot
# set.seed(1)
# #autoplot(k_4, data = GM1990)
# 
# # k = 6
# set.seed(1)
# k_6 <- kmeans(GM1990, 6, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
#autoplot(k_6, data = GM1990)

# k = 7
set.seed(1)
k_7 <- kmeans(GM1990, 7, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# Plot
#set.seed(1)
#autoplot(k_7_90, data = GM1990)

# # k = 8
# set.seed(1)
# k_8 <- kmeans(GM1990, 8, nstart=25, iter.max=1000)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# 
# # Plot
# set.seed(1)
# #autoplot(k_6, data = GM1990)

gapminder_1990 <- filter(gapminder_data,Year==1990)

# Input clusters into the the gapminder_data frame as a factor format
# gapminder_1990$k_3 <- as.factor(k_3$cluster)
# # gapminder_1990$k_4 <- as.factor(k_4$cluster)
# gapminder_1990$k_5 <- as.factor(k_5$cluster)
# gapminder_1990$k_6 <- as.factor(k_6$cluster)
gapminder_1990$k_7 <- as.factor(k_7$cluster)
#gapminder_1990$k_8 <- as.factor(k_8$cluster)

gapminder_data <- rbind(gapminder_2010, gapminder_2000, gapminder_1990)

#### Determine tiers ####

# Create Index
gapminder_data$Country_index <- paste0(gapminder_data$Year,as.numeric(gapminder_data$k_7))

gapminder_1990$Country_index <- paste0(gapminder_1990$Year,as.numeric(gapminder_1990$k_7))
gapminder_2000$Country_index <- paste0(gapminder_2000$Year,as.numeric(gapminder_2000$k_7))
gapminder_2010$Country_index <- paste0(gapminder_2010$Year,as.numeric(gapminder_2010$k_7))

# Create Tier columns
gapminder_data$Tier <- ""
gapminder_data$Tier <- as.integer(gapminder_data$Tier)

gapminder_1990$Tier <- ""
gapminder_1990$Tier <- as.integer(gapminder_1990$Tier)

gapminder_2000$Tier <- ""
gapminder_2000$Tier <- as.integer(gapminder_2000$Tier)

gapminder_2010$Tier <- ""
gapminder_2010$Tier <- as.integer(gapminder_2010$Tier)

# ifelse statement
gapminder_data$Tier <- ifelse(gapminder_data$Country_index %in% c(19903,20005,20105),7,
                         ifelse(gapminder_data$Country_index %in% c(19904,20001,20102),6,
                                ifelse(gapminder_data$Country_index %in% c(19907,20007,20103),5,
                                       ifelse(gapminder_data$Country_index %in% c(19906,20002,20104),4,
                                              ifelse(gapminder_data$Country_index %in% c(19905,20006,20101),3,
                                                     ifelse(gapminder_data$Country_index %in% c(19901,20004,20107),2,1))))))
# ifelse statement
gapminder_2010$Tier <- ifelse(gapminder_2010$Country_index %in% c(19903,20005,20105),7,
                              ifelse(gapminder_2010$Country_index %in% c(19904,20001,20102),6,
                                     ifelse(gapminder_2010$Country_index %in% c(19907,20007,20103),5,
                                            ifelse(gapminder_2010$Country_index %in% c(19906,20002,20104),4,
                                                   ifelse(gapminder_2010$Country_index %in% c(19905,20006,20101),3,
                                                          ifelse(gapminder_2010$Country_index %in% c(19901,20004,20107),2,1))))))
# ifelse statement
gapminder_2000$Tier <- ifelse(gapminder_2000$Country_index %in% c(19903,20005,20105),7,
                              ifelse(gapminder_2000$Country_index %in% c(19904,20001,20102),6,
                                     ifelse(gapminder_2000$Country_index %in% c(19907,20007,20103),5,
                                            ifelse(gapminder_2000$Country_index %in% c(19906,20002,20104),4,
                                                   ifelse(gapminder_2000$Country_index %in% c(19905,20006,20101),3,
                                                          ifelse(gapminder_2000$Country_index %in% c(19901,20004,20107),2,1))))))
# ifelse statement
gapminder_1990$Tier <- ifelse(gapminder_1990$Country_index %in% c(19903,20005,20105),7,
                              ifelse(gapminder_1990$Country_index %in% c(19904,20001,20102),6,
                                     ifelse(gapminder_1990$Country_index %in% c(19907,20007,20103),5,
                                            ifelse(gapminder_1990$Country_index %in% c(19906,20002,20104),4,
                                                   ifelse(gapminder_1990$Country_index %in% c(19905,20006,20101),3,
                                                          ifelse(gapminder_1990$Country_index %in% c(19901,20004,20107),2,1))))))

#### Country plots ####

# # Plot 2010 country coordinates
# fviz_pca_ind(pca.2010,
#              col.ind = gapminder_1990$k_3,
#              repel = FALSE, title="", xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", label="", legend.title="Clusters"
# )
# 
# fviz_pca_ind(pca.2010,
#              col.ind = gapminder_1990$k_5,
#              repel = FALSE, title="", xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", label="", legend.title="Clusters"
# )
# 
# fviz_pca_ind(pca.2010,
#              col.ind = gapminder_1990$k_7,
#              repel = FALSE, title="", xlab="PC1 (71.4%)", ylab="PC2 (8.0%)", label="", legend.title="Clusters"
# )

# Plot 2000 country coordinates
# fviz_pca_ind(pca.2000,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = FALSE     # Avoid text overlapping
# )

# Plot 1990 country coordinates
# fviz_pca_ind(pca.1990,
#              #col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = FALSE     # Avoid text overlapping
# )

#### Feature plots ####

# Plot 2010 features
# fviz_pca_var(pca.2010,
#              col.var = "cos2", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

# Plot 2000 features
# fviz_pca_var(pca.2000,
#              #col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

# Plot 2000 features
# fviz_pca_var(pca.1990,
#              #col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
  
#### Biplots ####
# fviz_pca_biplot(pca.2010,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = gapminder_2010$k_7,
#                 repel = FALSE, 
#                 title = "2010",
#                 xlim = c(-6,6),
#                 ylin = c(-3,3)
# )
# 
# fviz_pca_biplot(pca.2000, repel = FALSE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = gapminder_2000$k_7,
#                 title = "2000",
#                 xlim = c(-6,6),
#                 ylin = c(-3,3)
# )
# #
# fviz_pca_biplot(pca.1990, repel = FALSE,
#                 col.var = "#2E9FDF",
#                 col.ind="#0044bb", label = "",
#                 title = "",
#                 xlim = c(-8,8),
#                 ylin = c(-3,3), xlab="PC1 (71.4%)", ylab="PC2 8.0%)"
# )


fviz_pca_ind(pca.1990,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE, title="", xlab="PC1 (71.4%)", ylab="PC2 8.0%)",
             xlim=c(-10,6), ylim=c(-2.5,4), labels=FALSE
)

# Descriptive statistics
# three_countries <- gapminder_1990 %>%
#   select(Country,"Infant mortality", "Years in school",GDP) %>%
#   filter(Country == "United States"| Country == "Cuba"| Country == "Chad" )

##### Alluvial Diagram #####
# alluvialDF <- data.frame("1990s"=gapminder_1990$Tier,
#                          "2000s"=gapminder_2000$Tier,
#                          "2010s"=gapminder_2010$Tier)
# alluvialDF$Count <- 1
# 
# View(alluvialDF)
# 
# alluvialDF_2 <- alluvialDF %>%
#   group_by(X1990s,X2000s,X2010s) %>%
#   summarise(sum(Count))
# 
# col.UNH <- colorRampPalette(c("#cb4d0b", "#f77a05", "#FFCC64", "#FFFFD4", "#ffffff", "#DBDEFF", "#7F8DFF", "#0044bb", "#001d52"))
# 
# alluvial_diag <- alluvial(alluvialDF_2[,1:3], freq = alluvialDF_2$`sum(Count)`,
#          col = ifelse(alluvialDF_2$X1990s==1,"#cb4d0b",
#                       ifelse(alluvialDF_2$X1990s==2,"#FFFFD4",
#                              ifelse(alluvialDF_2$X1990s==3,"#ffffff",
#                                     ifelse(alluvialDF_2$X1990s==4,"#DBDEFF",
#                                            ifelse(alluvialDF_2$X1990s==5,"#7F8DFF",
#                                                   ifelse(alluvialDF_2$X1990s==6,"#0044bb",
#                                                          "#001d52")))))))

#### Cluster sizes ####
#sort(table(k_7$clust))

#### Create 3-d plane ####
# s3d <- scatterplot3d(gapminder_data[2:4], highlight.3d=TRUE, col.axis = "#003591", col.grid = "#003591", col.lab = "#003591")
# 
# plane <- lm(gapminder_data$`Years in school` ~ gapminder_data$`Food consumption` + gapminder_data$GDP)
# s3d$plane3d(plane)



################## Scene 1 ################################

fviz_pca_var(pca.1990, repel = TRUE,
             col.var = "#2E9FDF",
             title = "",
             xlim = c(-1,1),
             ylin = c(-3,3),
             label = "var",
             select.var = list(name = c("Population growth","Children per women"))
)
             #select.var = c("Population growth","Children per women","Child mortality","Infant mortality")
             #select_vars("Population Growth","Children Per Women","Child mortality","Infant mortality"))
             
fviz_pca_biplot(pca.1990, repel = TRUE,
                             col.var = "#2E9FDF", # Variables color
                             habillage = gapminder_1990$k_3,
                             title = "Directions & Magnitudes of Features",
                             xlim = c(-8,8),
                             ylin = c(-3,3),
                             label = "var",
                             addEllipses = TRUE,
                             ellipse.level = 0.95,
                             labelsize = 5.0,
                             ellipse.type = "convex")
             
# #p + scale_color_manual(values=c("green", "blue", "red"))
#              
# ## Options to change colors below.
# # p + scale_color_brewer(palette="Dark2") +
# #   theme_minimal()
# # p + scale_color_manual(values=c("green", "blue", "red"))
#              
# ################## Scene 2 ################################
#              
# fviz_pca_ind(pca.1990, repel = TRUE,
#                           col.var = "#2E9FDF",
#                           habillage = gapminder_1990$k_3,
#                           title = "",
#                           xlim = c(-8,8),
#                           ylin = c(-3,3),
#                           label = "var",
#                           addEllipses = TRUE,
#                           ellipse.level = 0.95,
#                           labelsize = 5.0,
#                           ellipse.type = "convex"
#              # select.ind = list(name = c("United States","Cuba","Chad"))
# )
#              
# ################## Scene 4 ################################
#              
# fviz_pca_biplot(pca.1990, repel = TRUE,
#                              col.var = "#2E9FDF", # Variables color
#                              habillage = gapminder_1990$continent,
#                              title = "Directions & Magnitudes of Features",
#                              xlim = c(-10,10),
#                              ylin = c(-3,3),
#                              label = "var",
#                              invisible = "var",
#                              labelsize = 5.0
#              )
#              
# ################## Scene 5 ################################
#              library(ggplot2)
#              
#              ggplot(gapminder_data, aes(PC1, PC2))+geom_point()
             
             
             # library(devtools)
             # library(RCurl)
             # library(httr)
             # set_config( config( ssl_verifypeer = 0L ) )
             # devtools::install_github("thomasp85/gganimate")
             # 
             # install.packages("devtools")
             # library(devtools)
             # install_github("thomasp85/gganimate")
             # # install_github("wrengels/HWxtest", subdir="pkg")
             # 
             # library(ggplot2)
             # library(gganimate)
             # library(gapminder)
             # 
             # ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
             #   geom_point(alpha = 0.7, show.legend = FALSE) +
             #   scale_colour_manual(values = country_colors) +
             #   scale_size(range = c(2, 12)) +
             #   scale_x_log10() +
             #   facet_wrap(~continent) +
             #   # Here comes the gganimate specific bits
             #   labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
             #   transition_time(year) +
             #   ease_aes('linear')
             
######################## Alluvial Diagram ################################
             
             library(alluvial)
             
             alluvialDF <- data.frame("1990s"=gapminder_1990$Tier,
                                      "2000s"=gapminder_2000$Tier,
                                      "2010s"=gapminder_2010$Tier)
             
             # Rename Columns
             colnames(alluvialDF)[colnames(alluvialDF)=="X1990s"] <- "1990s"
             colnames(alluvialDF)[colnames(alluvialDF)=="X2000s"] <- "2000s"
             colnames(alluvialDF)[colnames(alluvialDF)=="X2010s"] <- "2010s"
             
             alluvialDF$Count <- 1
             
             View(alluvialDF)
             
             alluvialDF_2 <- alluvialDF %>%
               group_by(`1990s`,`2000s`,`2010s`) %>%
               summarise(sum(Count))
             
             col.UNH <- colorRampPalette(c("#cb4d0b", "#f77a05", "#FFCC64", "#FFFFD4", "#ffffff", "#DBDEFF", "#7F8DFF", "#0044bb", "#001d52"))
             
             alluvial(alluvialDF_2[,1:3], freq = alluvialDF_2$`sum(Count)`,
                      col = ifelse(alluvialDF_2$`1990s`==1,"#cb4d0b",
                                   ifelse(alluvialDF_2$`1990s`==2,"#FFFFD4",
                                          ifelse(alluvialDF_2$`1990s`==3,"#ffffff",
                                                 ifelse(alluvialDF_2$`1990s`==4,"#DBDEFF",
                                                        ifelse(alluvialDF_2$`1990s`==5,"#7F8DFF",
                                                               ifelse(alluvialDF_2$`1990s`==6,"#0044bb",
                                                                      "#001d52")))))))
             
             
             
             
             
             
             # # Plot countries against PCs
             # autoplot(pca.2010, loadings = TRUE, colour = gapminder_2010$k_7, scale=0,
             #          loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3, xlim = c(-6,6), ylim = c(-3,3))
             # 
             # autoplot(pca.2010, loadings = TRUE, colour = gapminder_2010$k_4, scale=0,
             #          loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3, xlim = c(-6,6), ylim = c(-3,3))
             # 
             # autoplot(pca.2010, loadings = TRUE, colour = gapminder_2010$k_5, scale=0,
             #          loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3, xlim = c(-6,6), ylim = c(-3,3))
             # 
             # autoplot(pca.2010, loadings = TRUE, colour = gapminder_2010$k_6, scale=0,
             #          loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3, xlim = c(-6,6), ylim = c(-3,3))
             
             # Cluster plot (convex group) 
             fviz_cluster(object = k_7_10, data = GM2010, ellipse.type = "norm", repel = T, show.clust.cent = T, alpha = 0,xlim = c(-6,6),ylin = c(-3,3))
             #fviz_cluster(object = k_7_00, data = GM2000, ellipse.type = "convex", repel = T, show.clust.cent = T)
             #fviz_cluster(object = k_7_90, data = GM1990, ellipse.type = "convex", repel = T, show.clust.cent = T)
             
             gapminder_data <- rbind(gapminder_2010, gapminder_2000, gapminder_1990)
             
             # Descriptive statistics
             three_countries <- gapminder_1990 %>%
               select(Country,"Infant mortality", "Years in school",GDP) %>%
               filter(Country == "United States"| Country == "Cuba"| Country == "Chad" )
             
             # Create Tier Index
             
             gapminder_data$country_index <- paste0(gapminder_data$Year,as.numeric(gapminder_data$k_7))
             
             gapminder_1990$country_index <- paste0(gapminder_1990$Year,as.numeric(gapminder_1990$k_7))
             gapminder_2000$country_index <- paste0(gapminder_2000$Year,as.numeric(gapminder_2000$k_7))
             gapminder_2010$country_index <- paste0(gapminder_2010$Year,as.numeric(gapminder_2010$k_7))
             
             # Create Tier column 
             
             gapminder_data$Tier <- ""
             
             gapminder_1990$Tier <- ""
             gapminder_2000$Tier <- ""
             gapminder_2010$Tier <- ""
             
             # Create ifelse statement to assign Tiers
             
             gapminder_data$Tier <- ifelse(gapminder_data$country_index %in% c(19903,20005,20105),7,
                                           ifelse(gapminder_data$country_index %in% c(19904,20001,20102),6,
                                                  ifelse(gapminder_data$country_index %in% c(19907,20007,20103),5,
                                                         ifelse(gapminder_data$country_index %in% c(19906,20002,20104),4,
                                                                ifelse(gapminder_data$country_index %in% c(19905,20006,20101),3,
                                                                       ifelse(gapminder_data$country_index %in% c(19901,20004,20107),2,1))))))
             
             
             # Creating Tiers for the Data Frames per year
             
             gapminder_1990$Tier <- ifelse(gapminder_1990$country_index %in% c(19903,20005,20105),7,
                                           ifelse(gapminder_1990$country_index %in% c(19904,20001,20102),6,
                                                  ifelse(gapminder_1990$country_index %in% c(19907,20007,20103),5,
                                                         ifelse(gapminder_1990$country_index %in% c(19906,20002,20104),4,
                                                                ifelse(gapminder_1990$country_index %in% c(19905,20006,20101),3,
                                                                       ifelse(gapminder_1990$country_index %in% c(19901,20004,20107),2,1))))))
             
             gapminder_2000$Tier <- ifelse(gapminder_2000$country_index %in% c(19903,20005,20105),7,
                                           ifelse(gapminder_2000$country_index %in% c(19904,20001,20102),6,
                                                  ifelse(gapminder_2000$country_index %in% c(19907,20007,20103),5,
                                                         ifelse(gapminder_2000$country_index %in% c(19906,20002,20104),4,
                                                                ifelse(gapminder_2000$country_index %in% c(19905,20006,20101),3,
                                                                       ifelse(gapminder_2000$country_index %in% c(19901,20004,20107),2,1))))))
             
             
             gapminder_2010$Tier <- ifelse(gapminder_2010$country_index %in% c(19903,20005,20105),7,
                                           ifelse(gapminder_2010$country_index %in% c(19904,20001,20102),6,
                                                  ifelse(gapminder_2010$country_index %in% c(19907,20007,20103),5,
                                                         ifelse(gapminder_2010$country_index %in% c(19906,20002,20104),4,
                                                                ifelse(gapminder_2010$country_index %in% c(19905,20006,20101),3,
                                                                       ifelse(gapminder_2010$country_index %in% c(19901,20004,20107),2,1))))))
             
             
###############Adding Continent##########################
             
library(countrycode)
=gapminder_data$continent <- countrycode(sourcevar = gapminder_data$Country, origin = "country.name", destination = "continent")
             
             gapminder_1990$continent <- countrycode(sourcevar = gapminder_1990$Country, origin = "country.name", destination = "continent")
             gapminder_2000$continent <- countrycode(sourcevar = gapminder_2000$Country, origin = "country.name", destination = "continent")
             gapminder_2010$continent <- countrycode(sourcevar = gapminder_2010$Country, origin = "country.name", destination = "continent")
             
             ########################### Calculating Variance Per Year ######################
             
             TotalUnscaled <- merge(unscaled90,unscaled00, by=c("country","country"))
             
             TotalUnscaled_AllInclusive <- merge(TotalUnscaled,unscaled10, by=c("country","country"))
             
             # .x columns are the 90s, .y columns are the 00s, and regular column names are the 10s
             
             str(TotalUnscaled_AllInclusive)
             
             # Years in School Variance Columns
             
             TotalUnscaled_AllInclusive$SchoolVariance_90_00 <- ((TotalUnscaled_AllInclusive$years_in_school.y-TotalUnscaled_AllInclusive$years_in_school.x)/TotalUnscaled_AllInclusive$years_in_school.x) 
             TotalUnscaled_AllInclusive$SchoolVariance_00_10 <- ((TotalUnscaled_AllInclusive$years_in_school-TotalUnscaled_AllInclusive$years_in_school.y)/TotalUnscaled_AllInclusive$years_in_school.y) 
             TotalUnscaled_AllInclusive$SchoolVariance_90_10 <- ((TotalUnscaled_AllInclusive$years_in_school-TotalUnscaled_AllInclusive$years_in_school.x)/TotalUnscaled_AllInclusive$years_in_school.x)
             
             # GDP Variance Columns
             
             TotalUnscaled_AllInclusive$GDPVariance_90_00 <- ((TotalUnscaled_AllInclusive$gdp.y-TotalUnscaled_AllInclusive$gdp.x)/TotalUnscaled_AllInclusive$gdp.x) 
             TotalUnscaled_AllInclusive$GDPVariance_00_10 <- ((TotalUnscaled_AllInclusive$gdp-TotalUnscaled_AllInclusive$gdp.y)/TotalUnscaled_AllInclusive$gdp.y) 
             TotalUnscaled_AllInclusive$GDPVariance_90_10 <- ((TotalUnscaled_AllInclusive$gdp-TotalUnscaled_AllInclusive$gdp.x)/TotalUnscaled_AllInclusive$gdp.x)
             
             # Food Consumption Variance Columns
             
             TotalUnscaled_AllInclusive$Food_ConsumptionVariance_90_00 <- ((TotalUnscaled_AllInclusive$food_consumption.y-TotalUnscaled_AllInclusive$food_consumption.x)/TotalUnscaled_AllInclusive$food_consumption.x) 
             TotalUnscaled_AllInclusive$Food_ConsumptionVariance_00_10 <- ((TotalUnscaled_AllInclusive$food_consumption-TotalUnscaled_AllInclusive$food_consumption.y)/TotalUnscaled_AllInclusive$food_consumption.y) 
             TotalUnscaled_AllInclusive$Food_ConsumptionVariance_90_10 <- ((TotalUnscaled_AllInclusive$food_consumption-TotalUnscaled_AllInclusive$food_consumption.x)/TotalUnscaled_AllInclusive$food_consumption.x)
             
             # Infant Mortality Variance Columns
             
             TotalUnscaled_AllInclusive$Infant_MortalityVariance_90_00 <- ((TotalUnscaled_AllInclusive$infant_mortality.y-TotalUnscaled_AllInclusive$infant_mortality.x)/TotalUnscaled_AllInclusive$infant_mortality.x) 
             TotalUnscaled_AllInclusive$Infant_MortalityVariance_00_10 <- ((TotalUnscaled_AllInclusive$infant_mortality-TotalUnscaled_AllInclusive$infant_mortality.y)/TotalUnscaled_AllInclusive$infant_mortality.y) 
             TotalUnscaled_AllInclusive$Infant_MortalityVariance_90_10 <- ((TotalUnscaled_AllInclusive$infant_mortality-TotalUnscaled_AllInclusive$infant_mortality.x)/TotalUnscaled_AllInclusive$infant_mortality.x)
             
             # Life Expectancy Variance Columns
             
             TotalUnscaled_AllInclusive$Life_ExpectancyVariance_90_00 <- ((TotalUnscaled_AllInclusive$life_expectancy.y-TotalUnscaled_AllInclusive$life_expectancy.x)/TotalUnscaled_AllInclusive$life_expectancy.x) 
             TotalUnscaled_AllInclusive$Life_ExpectancyVariance_00_10 <- ((TotalUnscaled_AllInclusive$life_expectancy-TotalUnscaled_AllInclusive$life_expectancy.y)/TotalUnscaled_AllInclusive$life_expectancy.y) 
             TotalUnscaled_AllInclusive$Life_ExpectancyVariance_90_10 <- ((TotalUnscaled_AllInclusive$life_expectancy-TotalUnscaled_AllInclusive$life_expectancy.x)/TotalUnscaled_AllInclusive$life_expectancy.x)

# Internet Users Variance Columns
             
             TotalUnscaled_AllInclusive$internet_usersVariance_90_00 <- ((TotalUnscaled_AllInclusive$internet_users.y-TotalUnscaled_AllInclusive$internet_users.x)/TotalUnscaled_AllInclusive$internet_users.x) 
             TotalUnscaled_AllInclusive$internet_usersVariance_00_10 <- ((TotalUnscaled_AllInclusive$internet_users-TotalUnscaled_AllInclusive$internet_users.y)/TotalUnscaled_AllInclusive$internet_users.y) 
             TotalUnscaled_AllInclusive$internet_usersVariance_90_10 <- ((TotalUnscaled_AllInclusive$internet_users-TotalUnscaled_AllInclusive$internet_users.x)/TotalUnscaled_AllInclusive$internet_users.x)
             
# Cell Phone Variance Columns
             TotalUnscaled_AllInclusive$cell_phoneVariance_90_00 <- ((TotalUnscaled_AllInclusive$cell_phone.y-TotalUnscaled_AllInclusive$cell_phone.x)/TotalUnscaled_AllInclusive$cell_phone.x) 
             TotalUnscaled_AllInclusive$cell_phoneVariance_00_10 <- ((TotalUnscaled_AllInclusive$cell_phone-TotalUnscaled_AllInclusive$cell_phone.y)/TotalUnscaled_AllInclusive$cell_phone.y) 
             TotalUnscaled_AllInclusive$cell_phoneVariance_90_10 <- ((TotalUnscaled_AllInclusive$cell_phone-TotalUnscaled_AllInclusive$cell_phone.x)/TotalUnscaled_AllInclusive$cell_phone.x)
             
# Child Mortality Variance Columns
TotalUnscaled_AllInclusive$Child_MortalityVariance_90_00 <- ((TotalUnscaled_AllInclusive$child_mortality.y-TotalUnscaled_AllInclusive$child_mortality.x)/TotalUnscaled_AllInclusive$child_mortality.x) 
TotalUnscaled_AllInclusive$Child_MortalityVariance_00_10 <- ((TotalUnscaled_AllInclusive$child_mortality-TotalUnscaled_AllInclusive$child_mortality.y)/TotalUnscaled_AllInclusive$child_mortality.y) 
TotalUnscaled_AllInclusive$Child_MortalityVariance_90_10 <- ((TotalUnscaled_AllInclusive$child_mortality-TotalUnscaled_AllInclusive$child_mortality.x)/TotalUnscaled_AllInclusive$child_mortality.x)
             
# Children Per Women Variance Columns
TotalUnscaled_AllInclusive$Children_Per_WomenVariance_90_00 <- ((TotalUnscaled_AllInclusive$children_per_women.y-TotalUnscaled_AllInclusive$children_per_women.x)/TotalUnscaled_AllInclusive$children_per_women.x) 
TotalUnscaled_AllInclusive$Children_Per_WomenVariance_00_10 <- ((TotalUnscaled_AllInclusive$children_per_women-TotalUnscaled_AllInclusive$children_per_women.y)/TotalUnscaled_AllInclusive$children_per_women.y) 
TotalUnscaled_AllInclusive$Children_Per_WomenVariance_90_10 <- ((TotalUnscaled_AllInclusive$children_per_women-TotalUnscaled_AllInclusive$children_per_women.x)/TotalUnscaled_AllInclusive$children_per_women.x)
             
# Population Growth Variance Columns
TotalUnscaled_AllInclusive$Population_GrowthVariance_90_00 <- ((TotalUnscaled_AllInclusive$population_growth.y-TotalUnscaled_AllInclusive$population_growth.x)/TotalUnscaled_AllInclusive$population_growth.x) 
TotalUnscaled_AllInclusive$Population_GrowthVariance_00_10 <- ((TotalUnscaled_AllInclusive$population_growth-TotalUnscaled_AllInclusive$population_growth.y)/TotalUnscaled_AllInclusive$population_growth.y) 
TotalUnscaled_AllInclusive$Population_GrowthVariance_90_10 <- ((TotalUnscaled_AllInclusive$population_growth-TotalUnscaled_AllInclusive$population_growth.x)/TotalUnscaled_AllInclusive$population_growth.x)
             
# Urban Pop Variance Columns
TotalUnscaled_AllInclusive$Urban_PopVariance_90_00 <- ((TotalUnscaled_AllInclusive$urban_pop.y-TotalUnscaled_AllInclusive$urban_pop.x)/TotalUnscaled_AllInclusive$urban_pop.x)
TotalUnscaled_AllInclusive$Urban_PopVariance_00_10 <- ((TotalUnscaled_AllInclusive$urban_pop-TotalUnscaled_AllInclusive$urban_pop.y)/TotalUnscaled_AllInclusive$urban_pop.y)
TotalUnscaled_AllInclusive$Urban_PopVariance_90_10 <- ((TotalUnscaled_AllInclusive$urban_pop-TotalUnscaled_AllInclusive$urban_pop.x)/TotalUnscaled_AllInclusive$urban_pop.x)
             
#### Export dataframes ####
#write.csv(gapminder_data, file = "/Users/annakot/Desktop/gapminder_data.csv", row.names = F)
#write.table(gapminder_data, file = "/Users/annakot/Desktop/gapminder_data.txt", sep = ",", row.names = F)
#write.csv(TotalUnscaled_AllInclusive, file = "D:/Summer Practicum/Total_Unscaled_Variances.csv", row.names = F)
             
library(alluvial)

alluvialDF <- data.frame("1990s"=gapminder_1990$Tier,
                         "2000s"=gapminder_2000$Tier,
                         "2010s"=gapminder_2010$Tier)

# Rename Columns
colnames(alluvialDF)[colnames(alluvialDF)=="X1990s"] <- "1990s"
colnames(alluvialDF)[colnames(alluvialDF)=="X2000s"] <- "2000s"
colnames(alluvialDF)[colnames(alluvialDF)=="X2010s"] <- "2010s"

alluvialDF$Count <- 1

View(alluvialDF)

alluvialDF_2 <- alluvialDF %>%
  group_by(`1990s`,`2000s`,`2010s`) %>%
  summarise(sum(Count))

View(alluvialDF_2)

col.UNH <- colorRampPalette(c("#cb4d0b", "#f77a05", "#FFCC64", "#FFFFD4", "#ffffff", "#DBDEFF", "#7F8DFF", "#0044bb", "#001d52"))


# Create Order for each column of Alluvial
ord <- list(with(alluvialDF_2, order('1990s')), NULL, NULL)


alluvial(alluvialDF_2[,1:3], freq = alluvialDF_2$`sum(Count)`,
         # ordering = ord,
         col = ifelse(alluvialDF_2$`1990s`==1,"#cb4d0b",
                      ifelse(alluvialDF_2$`1990s`==2,"#FFFFD4",
                             ifelse(alluvialDF_2$`1990s`==3,"#ffffff",
                                    ifelse(alluvialDF_2$`1990s`==4,"#DBDEFF",
                                           ifelse(alluvialDF_2$`1990s`==5,"#7F8DFF",
                                                  ifelse(alluvialDF_2$`1990s`==6,"#0044bb",
                                                         "#001d52")))))))


alluvial(alluvialDF_2[,1:3], freq = alluvialDF_2$`sum(Count)`,axis_labels = c("1990","2000","2010"),alpha = .75,
         col = ifelse(alluvialDF_2$`1990s`==1,"#f77a05",
                      ifelse(alluvialDF_2$`1990s`==2,"#5c6874",
                             ifelse(alluvialDF_2$`1990s`==3,"#003591",
                                    ifelse(alluvialDF_2$`1990s`==4,"#cb4d0b",
                                           ifelse(alluvialDF_2$`1990s`==5,"#0044bb",
                                                  ifelse(alluvialDF_2$`1990s`==6,"#A3A9AC",
                                                         "#001d52")))))))
