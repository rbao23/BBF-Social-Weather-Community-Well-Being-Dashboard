
library(tigris)
library(sf)

# load libaries
#library(shiny)
library(shinythemes)
library(dplyr)
library(RPostgreSQL)
library(magrittr)
library(tidyverse)
library(leaflet)

#library(shinydashboard)

################## Getting data from the database e#############################################

# script to connect
source("H:/ruihab/Documents/BBF-Social-Weather-Dashboard/code_base/dbconnect.R")
source("H:/ruihab/Documents/BBF-Social-Weather-Dashboard/code_base/pgpass.R")

# create a connection to the postgresql database
# note that "con" will be used later in each connection to the database
con <- connectdb(host = "bffsw.csde.washington.edu", dbname = "ruihab_sb")

# test
dbListTables(conn = con) 


# get the required tables from the sql database 
social_index_dataset = dbGetQuery(con, "SELECT * from public.tbl_dataset_info
                                        right JOIN public.tbl_social_weather_dataset using(dataset_id)
                                        RIGHT JOIN public.tbl_geography using(geo_id)")

# dataframe wrangling
social_index_dataset<-social_index_dataset %>% mutate(sex = case_when(startsWith(social_index_dataset$variable,"Female") ~ "Female", 
                                                                      startsWith(social_index_dataset$variable,"Male")   ~ "Male", TRUE ~ "All"))
social_index_dataset <- social_index_dataset %>%
  mutate(age = case_when(
    (social_index_dataset$dataset_id == 12 &(endsWith(social_index_dataset$variable, "years_percent insured estimate") | endsWith(social_index_dataset$variable,"older_percent insured estimate")))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),TRUE ~ 'All'))

social_index_dataset <- social_index_dataset %>%
  mutate(race = case_when(
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"prison_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-16),
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"jail_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),
    (social_index_dataset$dataset_id == 11 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-15),
    (social_index_dataset$dataset_id == 12 & social_index_dataset$sex == 'All'& social_index_dataset$age == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),
    (social_index_dataset$dataset_id == 15 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),TRUE ~ 'All'))

social_index_dataset$race[social_index_dataset$race=='Total']  <- "All" 
social_index_dataset$race[social_index_dataset$race=='Aapi']  <- "Asian Americans and Pacific Islanders"
social_index_dataset$subdomain <- ifelse(is.null(social_index_dataset$subdomain), 'N/A', social_index_dataset$subdomain )

# disconnect database
dbDisconnect(con) 


wa_tracts <-tracts(state = "WA", county = c('King', 'Pierce','Yakima'))
md_tracts <-tracts(state = 'MD', county = c('Baltimore county', 'Baltimore city','Prince George','Montgomery'))
                        
test_tracts = union(wa_tracts[,c("GEOID","geometry")],md_tracts[,c("GEOID","geometry")])

wa_counties <- counties(state = 'WA', cb = TRUE, resolution = '20m')
md_counties <- counties(state = 'MD', cb = TRUE, resolution = '20m')
test_counties <- union(wa_counties[,c("GEOID","geometry")],md_counties[,c("GEOID","geometry")])

allstates <- states(cb=TRUE)
test_area <- bind_rows(test_tracts,test_counties,allstates)

print(mapdata_merged)

social_index_dataset$geo_id <- substr(social_index_dataset$geo_id,3,nchar(social_index_dataset$geo_id))
names(social_index_dataset)[1] <- 'GEOID'
mapdata_merged <- dplyr::left_join(social_index_dataset,test_area[,c("GEOID","geometry")], "GEOID", "GEOID")%>% drop_na("geometry")

mapdata_merged_sf <-st_as_sf(mapdata_merged)


pal_fun <- colorQuantile("YlOrRd", NULL, n = 6)
p_popup <- paste0("<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name), collapse = "\n",
                  "<strong> Total estimate </strong>", unique(mapdata_merged_sf$value))

leaflet(mapdata_merged_sf) %>%
  addProviderTiles(provider = "CartoDB") %>% 
  addPolygons(
    stroke = FALSE,
    fillColor = ~pal_fun(as.numeric(value)), # set fill color with function from above and value
    fillOpacity = 0.5, 
    smoothFactor = 0.2)


#setView(lng = -93.85, lat = 37.45, zoom = 4)
