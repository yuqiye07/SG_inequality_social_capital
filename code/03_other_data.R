
#-------------------------------------------------------------------------------
# Script purpose: clean multiple datasets needed in this study.
# Sources for the raw data are detailed in the README file.
#-------------------------------------------------------------------------------

# load libraries
library(dplyr)
library(tidycensus) 
library(sf) 
library(stringr)
library(tidyr)


# get census data using api

variables <- c("B19083_001E" = "gini",# Gini coefficient
               "S1901_C01_012E" = "income", # median income
               "B01003_001E" = "population", # total population
               "DP02_0093PE" = "foreign", # percentage of foreign born residents
               "S0101_C01_033E" = "sex_ratio",# males per 100 females
               "S0101_C01_032E" = "age", # median age
               
               # households% of different income groups
               'S1901_C01_002E' = 'less_10k',
               'S1901_C01_003E' = '10k_15k',
               'S1901_C01_004E' = '15k_25k',
               'S1901_C01_009E' = '100k_150k',
               'S1901_C01_010E' = "150k_200k",
               'S1901_C01_011E' = "200k_more"
)

## get census data for predefined variables
data <- get_acs(geography = "county",
                variables = names(variables),
                year = 2019)

data <- data[,c("GEOID","NAME","variable",'estimate')]
data$variable <- paste0(data$variable,"E")

## long data to wide data
census <- pivot_wider(data, names_from = variable, values_from = estimate)

## replace variable codes with names
names(census) <- stringr::str_replace_all(names(census), variables)


## get the area for counties to calculate population density
tmp_geojson <- tempfile(fileext = ".geojson")
download.file(
  "https://raw.githubusercontent.com/holtzy/The-Python-Graph-Gallery/master/static/data/US-counties.geojson",
  tmp_geojson
)
geo_file <- read_sf(tmp_geojson)
geo_file <- geo_file[,c("id","CENSUSAREA")]

census <- merge(census,
                geo_file,
                by.x="GEOID",
                by.y="id")

### calculate population per square kms
census$pop_density <- census$population/census$CENSUSAREA  

## calculate households% earning $100k+ and 25k-
census$income_25k_less <- census$less_10k+census$`10k_15k`+census$`15k_25k`
census$income_100k_more <- census$`100k_150k`+census$`150k_200k`+census$`200k_more`

## proportion of female residents
census$female <- 100/(census$sex_ratio+100) 

## log income
census$log_income <- log(census$income)


## remove unnecessary columns
census <- census %>%
  select(-c(population, 
            sex_ratio, 
            CENSUSAREA,
            geometry,
            less_10k,
            `10k_15k`,
            `15k_25k`,
            `100k_150k`,
            `150k_200k`,
            `200k_more`
            ))

census <- census[complete.cases(census), ]#remove rows with NAs
census$state <- substr(census$GEOID, 1, 2) # get FIPS code for state


# JEC index data
jec_raw <- rio::import_list('jec_social_capital_Index.xlsx') # raw data sources can be found in the README file
jec_raw <- jec_raw$`County Index` # select sheet
colnames(jec_raw) <- as.character(unlist(jec_raw[1, ])) #rename columns
jec_raw = jec_raw[-1,] # remove first row


df_jec <- df_jec[, c("FIPS Code", #select columns
                     "County-Level Index",
                    "Family Unity",
                    "Community Health",
                    "Institutional Health",
                    "Collective Efficacy")]
# remove rows with empty values
df_jec <- jec_raw[complete.cases(jec_raw[, "County-Level Index"]), ]


colnames(df_jec) <- c("home_county", # rename columns
                      "jec_index",
                      "family_unity",
                      "community_health",
                      "institutional_health",
                      "collective_efficacy"
)

# convert to numeric columns 
df_jec$jec_index <- as.numeric(df_jec$jec_index)
df_jec$family_unity <- as.numeric(df_jec$family_unity)
df_jec$community_health <- as.numeric(df_jec$community_health)
df_jec$institutional_health <- as.numeric(df_jec$institutional_health)
df_jec$collective_efficacy <- as.numeric(df_jec$collective_efficacy)


# Chetty's index 
chetty <- read.csv("social_capital_raw.csv") # raw data sources can be found in the README file

chetty <- chetty %>% # select columns
  select(c(county,
           ec_county,
           clustering_county,
           support_ratio_county,
           volunteering_rate_county,
           civic_organizations_county))

## convert FIPS code from numeric to 5 digits character
chetty$county <- sprintf("%05d", chetty$county)

names(chetty) <- c("home_county", # rename columns
                   "ec",
                   "clustering",
                   "support_ratio",
                   "volunteering_rate",
                   "civic_organizations")

## read penn state index data
penn <- readxl::read_excel("raw_data/penn_social_capital.xlsx")
penn <- penn[,c("FIPS","sk2014")]
## convert FIPS code from numeric to 5 digits character
penn$FIPS <- sprintf("%05d", penn$FIPS)
penn <- penn %>% 
  rename("home_county" = "FIPS")

## merge penn state index with other components in Chetty's index
chetty <- merge(chetty,
                penn,
                by="home_county")

chetty <- chetty[complete.cases(chetty), ]



# Ortega data
ortega <- read.csv("ortega_parameter_alpha_gamma_county.csv") # raw data sources can be found in the README file

ortega <- ortega[,c("COUNTY","alpha","gamma")] # select columns

## no FIPS code in Ortega data. merge with census data based on state and county 
## names to obtain FIPS.

## edit county names in the census data to align with the Ortega data.
edit_name <- function(name) {
  ## split the state and county name on comma
  parts <- str_split(name, ", ")[[1]]
  if(length(parts) < 2) return(NA)  # Return NA if the input is not properly formatted
  
  county <- parts[1]
  state <- parts[2]
  
  ## Replace "St." with "St" 
  county <- gsub("St\\.", "St", county)
  
  ## If the state is Alaska, take only the first word of the county
  if(state %in% c("Alaska",'AK')) {
    county <- str_split(county, " ")[[1]][1]
    ##  if county name includes "-", take the part before "-"
    if(str_detect(county, "-")){
      county <- str_split(county, "-")[[1]][1]
    }
  }
  
  ## create the new name by pasting county and state
  new_name <- paste(county, state, sep = ", ")
  return(new_name)
}

## Apply the function over the NAME column of the census data frame
census$NAME_new <- sapply(census$NAME, edit_name)
  
## merge ortega and census data to get FIPS code for ortega
ortega <- merge(ortega, census[,c("GEOID","NAME_new")],
                 by.x="COUNTY",
                 by.y="NAME_new")










