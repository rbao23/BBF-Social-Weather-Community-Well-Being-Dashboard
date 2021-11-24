#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# load libaries
library(shiny)
library(shinythemes)
library(dplyr)
library(RPostgreSQL)
library(magrittr)
library(tidyverse)
library(leaflet)
library(tigris)
#options(tigris_use_cache = TRUE)

library(sf)
library(classInt)
library(shinydashboard)

library(RColorBrewer)
library(viridis)
library(ggplot2)

library(DT)
library(shinycssloaders)


#library(shinydashboard)

################## Getting data from the database e#############################################

# script to connect
source("code_base/dbconnect.R")
source("code_base/pgpass.R")

# create a connection to the postgresql database
# note that "con" will be used later in each connection to the database
con <- connectdb(host = "bffsw.csde.washington.edu", dbname = "ruihab_sb")

# test
dbListTables(conn = con) 


# get the required tables from the sql database 
social_index_dataset = dbGetQuery(con, "SELECT * from public.tbl_social_weather_dataset
                                        LEFT JOIN public.tbl_dataset_info using(dataset_id)
                                        LEFT JOIN public.tbl_geography using(geo_id)")
tbl_age = dbReadTable(con,"tbl_Age")
tbl_population =  dbReadTable(con,"tbl_Population")
tbl_lifeexpectancy =  dbReadTable(con,"tbl_LifeExpectancy")
tbl_race = dbReadTable(con,"tbl_Race")
tbl_geography =  dbReadTable(con,"tbl_geography")

tbl_age_geo = dplyr::left_join(tbl_age,tbl_geography, "geo_id", "geo_id")
tbl_pop_geo = dplyr::left_join(tbl_population,tbl_geography, "geo_id", "geo_id")
tbl_le_geo = dplyr::left_join(tbl_lifeexpectancy,tbl_geography, "geo_id", "geo_id")
tbl_race_geo = dplyr::left_join(tbl_race,tbl_geography, "geo_id", "geo_id")

# disconnect database
dbDisconnect(con) 

# dataframe wrangling
social_index_dataset<-social_index_dataset %>% mutate(sex = case_when(startsWith(social_index_dataset$variable,"Female") ~ "Female", 
                                                                      startsWith(social_index_dataset$variable,"Male")   ~ "Male", TRUE ~ "All"))
# add age column
social_index_dataset <- social_index_dataset %>%
  mutate(age = case_when(
    (social_index_dataset$dataset_id == 12 &(endsWith(social_index_dataset$variable, "years_percent insured estimate") | endsWith(social_index_dataset$variable,"older_percent insured estimate")))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),TRUE ~ 'All'))

# add race column
social_index_dataset <- social_index_dataset %>%
  mutate(race = case_when(
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"prison_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-16),
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"jail_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),
    (social_index_dataset$dataset_id == 11 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-15),
    (social_index_dataset$dataset_id == 12 & social_index_dataset$sex == 'All'& social_index_dataset$age == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),
    (social_index_dataset$dataset_id == 15 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),TRUE ~ 'All'))

# change variable column
social_index_dataset <- social_index_dataset %>%
  mutate(variables = case_when(
    (social_index_dataset$dataset_id == 8 & endsWith(social_index_dataset$variable,"prison_pop_rate"))  ~ "Prison Population Rate",
    (social_index_dataset$dataset_id == 8 & endsWith(social_index_dataset$variable,"jail_pop_rate"))  ~ "Jail Population Rate",
    (social_index_dataset$dataset_id == 11)  ~  "Native Analysis Value",
    (social_index_dataset$dataset_id == 12)  ~"Percent Insured Estimate",
    (social_index_dataset$dataset_id == 15)  ~"Percent Voted",
    TRUE ~social_index_dataset$variable))

social_index_dataset$race[social_index_dataset$race=='Total']  <- "All" 
social_index_dataset$race[social_index_dataset$race=='Aapi']  <- "Asian Americans and Pacific Islanders"
social_index_dataset$subdomain <- ifelse(is.na(social_index_dataset$subdomain), 'N/A', social_index_dataset$subdomain)
social_index_dataset$value[is.na(social_index_dataset$value)] <- '0'
social_index_dataset <- social_index_dataset %>%
  mutate(value = case_when(endsWith(social_index_dataset$value,"%") ~substr(social_index_dataset$value,0,nchar(social_index_dataset$value)-1),TRUE ~social_index_dataset$value))

social_index_dataset_copy <- social_index_dataset
# clean up non numeric values
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE20']  <- "10"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='PS']  <- "60"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE5']  <- "2.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE95']  <- "97.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LT50']  <- "25"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE50']  <- "75"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE10']  <- "5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE90']  <- "95"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE80']  <- "90"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE99']  <- "99.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE1']  <- "0.5"

social_index_dataset$value <- apply(social_index_dataset[c("dataset_id", "value")], 1, function(df) {
  if (as.numeric(df["dataset_id"]) == 4 & is.na(as.numeric(df["value"]))) {
    splitted = strsplit(df["value"],"-")
    toString((as.numeric(splitted[[1]][2]) + as.numeric(splitted[[1]][1]) /2))
  } else {
    df["value"]
  }
})

social_index_dataset$value[social_index_dataset$dataset_id == 4 & is.na(as.numeric(social_index_dataset$value))] <- toString((as.numeric(strsplit(social_index_dataset$value,"-")[[1]][2]) + as.numeric(strsplit(social_index_dataset$value,"-")[[1]][1]))/2)





##############################shape file##############################
# get tract shapefile
wa_tracts <-tracts(state = "WA", county = c('King', 'Pierce','Yakima'))
md_tracts <-tracts(state = 'MD', county = c('Baltimore county', 'Baltimore city','Prince George','Montgomery'))
test_tracts = union(wa_tracts,md_tracts)
# get county shapefile
wa_counties <- counties(state = 'WA')
md_counties <- counties(state = 'MD')
test_counties <- union(wa_counties[,c("GEOID","geometry")],md_counties[,c("GEOID","geometry")])
#get state shapefile
test_states <- filter(states(),STUSPS == 'WA' | STUSPS == 'MD')
# get zcta shapefile
test_zcta<-zctas(starts_with = c("98", "99","20","21"))
test_area <- bind_rows(test_tracts,test_counties,test_states)

names(test_zcta)[1] <- 'GEOID'
test = union(test_area[,c("GEOID","geometry")],test_zcta[,c("GEOID","geometry")])




############################################### ui.R ##################################################

body <-navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                  title = "Social Weather Community Well-Being Dashboard",
                  tabPanel("Interactive Map",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Select Map Dataset"),
                               selectInput("domain","Domain:",choices=sort(unique(social_index_dataset$domain)),selected = NULL),
                               selectInput("subdomain", "Subdomain:", choices=NULL,selected = NULL),
                               selectInput("indicator", "Indicator:", choices=NULL,selected = NULL),
                               selectInput("variable_name", "Variable:", choices=NULL,selected = NULL),
                               selectInput("sex", "Sex:",choices=NULL,selected = NULL),
                               selectInput("race", "Race:",choices=NULL),
                               selectInput("age", "Age:",choices=NULL),
                               selectInput("geo_level", "Geographic Scale:",choices=NULL),
                               selectInput("year", "Year:",choices=NULL),
                               #sliderInput("year_slider", "Select Year", value =1990, min = 1990, max=2021, step=1,ticks = FALSE, animate=TRUE)
                               h3("Select Location"),
                               selectInput("line_state", "State:",choices=c("Washington","Maryland")),
                               selectInput("linelocation", "Location:",choices=NULL)
                             ),
                             mainPanel(
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               ),
                               tabsetPanel(
                                 id = "panels",
                                 tabPanel("Map View", verbatimTextOutput("mapview"),
                                          fluidRow(column(11, wellPanel(tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar{
                                                     background: #48C9B0;
                                                     border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                                        #sliderInput("yearslider", "Select Mapping Year", value =1990, min = 1990, max=2021, step=1,ticks = FALSE, animate=TRUE),
                                                                        fluidRow(column(width = 12, div(id = "mymap", leaflet::leafletOutput("mymap", height = "65vh"))%>% withSpinner(color="#0dc5c1"))), 
                                                                        fluidRow(column(width = 12, " ", style='padding:3px;')),
                                                                        plotOutput("lineplot", height = "250px"), 
                                                                        fluidRow(column(width = 12, "Welcome to the Social Weather Map! Use the left panel to filter data, 
                                                  and click on the map for additional details. Please note that data are not currently
                                                  available for every county in every year, and estimates will change as we process more data.", 
                                                                                        style='font-family:Avenir, Helvetica;font-size:16;text-align:center'))
                                          )))),
                                 tabPanel("Map Data", verbatimTextOutput("viewdata"),
                                          h2("Selected Social Weather Index Data Table"),
                                          DT::dataTableOutput("mytable")),
                                 tabPanel("Profile View", verbatimTextOutput("locprofview"),
                                          textOutput("text"),
                                          tags$head(tags$style("#text{color: Black;
                                          font-size: 25px;
                                          font-style: Bold;}")
                                          ),
                                          fluidRow(column(6, sliderInput("yearperiod", "Race Year Slider", value =1990, min = 1990, max=2021, sep = "",step=1,animate=animationOptions(
                                            interval = 1000,
                                            loop = TRUE,
                                            playButton = NULL,
                                            pauseButton = NULL)),
                                            plotOutput("raceplot")),
                                            column(6, sliderInput("yearperiod2", "Age Year Slider", value =1990, min = 1990, max=2021, sep = "",step=1,animate=animationOptions(
                                              interval = 1000,
                                              loop = TRUE,
                                              playButton = NULL,
                                              pauseButton = NULL)),
                                              plotOutput("ageplot"))),
                                          fluidRow(column(6, plotOutput("popplot")),
                                                   column(6, plotOutput("leplot")))
                                 ),
                                 tabPanel("Profile Data", verbatimTextOutput("Profdata"),
                                          textOutput("demogeotext"),
                                          tags$head(tags$style("#demogeotext{color: Black;
                                          font-size: 25px;
                                          font-style: Bold;}")
                                          ),
                                          selectInput("selectdemo", "Select demographic:", choices=c("Age","Population","Life Expectancy","Race")),
                                          DT::dataTableOutput("profiledata"),
                                          DT::dataTableOutput("allprofiledata"))
                                 
                               ))))
)

############################################### server.R ##################################################

# Define server logic required to draw a histogram
server <- function(input,output,session) {
  domain <- reactive({
    print("domain changed")
    #req(input$domain)
    print(input$domain)
    filter(social_index_dataset, social_index_dataset$domain==input$domain)
  })
  
  observeEvent(domain(), {
    print("observe domain change,return subdomain choices")
    print(input$domain)
    print("update subdomain choices")
    choices = sort(unique(domain()$subdomain))
    updateSelectInput(session=session,"subdomain",choices = choices)
    print(choices)
  })
  
  subdomain <- reactive({
    print("subdomain changed")
    req(input$subdomain)
    print(input$subdomain)
    filter(domain(), subdomain == input$subdomain)
  })
  
  observeEvent(subdomain(),{
    #req(input$subdomain)
    print("observe subdomain change,return indicator choices")
    print(input$subdomain)
    print("update indicator choices")
    choices = sort(unique(subdomain()$indicator))
    updateSelectInput(session,"indicator",choices = choices)
    print(choices)
  })
  
  indicator <- reactive({
    print("indicator changed")
    req(input$indicator)
    print(input$indicator)
    filter(subdomain(), indicator == input$indicator)
  })
  observeEvent(indicator(),{
    #req(input$indicator)
    print("observe indicator change,return variable names choices")
    print(input$indicator)
    print("update variable name choices")
    choices = sort(unique(indicator()$variables))
    updateSelectInput(session,"variable_name",choices = choices)
    print(choices)
  })
  variable_name <- reactive({
    print("variable_name changed")
    req(input$variable_name)
    print(input$variable_name)
    filter(indicator(), variables== input$variable_name)
  })
  observeEvent(variable_name(),{
    #req(input$variable_name)
    print("observe variable_name change,return sex choices")
    print(input$variable_name)
    print("update sex choices")
    choices = sort(unique(variable_name()$sex))
    updateSelectInput(session,"sex",choices = choices)
    print(choices)
  })
  
  ###
  sex <- reactive({
    print("sex changed")
    req(input$sex)
    print(input$sex)
    filter(variable_name(), sex == input$sex)
  })
  observeEvent(sex(), {
    #req(input$sex)
    print("observe sex change,return race choices")
    print(input$sex)
    print("update race choices")
    choices = sort(unique(sex()$race))
    updateSelectInput(session,"race",choices = choices)
    print(choices)
  })
  race <- reactive({
    print("race changed")
    req(input$race)
    print(input$race)
    filter(sex(), race == input$race)
  })
  observeEvent(race(),{
    #req(input$race)
    print("observe race change,return age choices")
    print(input$race)
    print("update age choices")
    choices = sort(unique(race()$age))
    updateSelectInput(session,"age",choices = choices)
    print(choices)
  })
  age <- reactive({
    print("age changed")
    req(input$age)
    print(input$age)
    filter(race(), age == input$age)
  })
  observeEvent(age(),{
    #req(input$age)
    print("observe age change,return geo_level choices")
    print(input$age)
    print("update geo_level choices")
    choices = sort(unique(age()$geo_level))
    updateSelectInput(session,"geo_level",choices = choices)
    print(choices)
  })
  geo_level <- reactive({
    print("geo_level changed")
    req(input$geo_level)
    print(input$geo_level)
    filter(age(), geo_level == input$geo_level)
  })
  observeEvent({geo_level()},{
    #req(input$geo_level)
    print("observe geo_level change,return year choices")
    print(input$geo_level)
    print("update year choices")
    choices = sort(unique(geo_level()$year))
    updateSelectInput(session,"year",choices = choices)
    print(choices)
  })
  
  year <- reactive({
    print("year changed")
    req(input$year)
    print(input$year)
    filter(geo_level(), year == input$year)
  })
  
  line_state <- reactive({
    print("linestate changed")
    print(input$line_state)
    if(input$line_state == "Washington"){
      filter(geo_level(), grepl("Washington|WA",geo_name))
    }
    else{
      filter(geo_level(), grepl("Maryland|MD",geo_name))
    }
  })
  
  observeEvent({line_state()},{
    updateSelectInput(session,"linelocation",choices = sort(unique(line_state()$geo_name)))
  })
  
  linelocation <- reactive({
    print("location changed")
    req(input$linelocation)
    print(input$linelocation)
    filter(line_state(), geo_name == input$linelocation)
  })
  
  #observeEvent({geo_level()},{
  #print("observe geo_level")
  #print(geo_level())
  #print(geo_level()$year)
  #choices = sort(unique(geo_level()$year))
  #print("choices")
  #print(choices)
  #updateSliderInput(session,'year_slider', value=range(choices),
  #                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  # })
  
  #year_slider <- reactive({
  # req(input$year_slider)
  #print("year changed")
  #print(input$year_slider)
  #filter(geo_level(), year == input$year_slider)
  #})
  
  #observeEvent({
  #geo_level()
  #year()},{
  #updateSliderInput(session,'yearslider', value=unique(year()$year),
  #                   min = min(as.numeric(age()$year)), max = max(as.numeric(age()$year)), step = 1)
  # }) 
  
  output$mymap <- renderLeaflet({
    print("observe geo level change, change map")
    req(input$year)
    f_data <- year()
    f_data$geo_id = substr(f_data$geo_id,3,nchar(f_data$geo_id))
    names(f_data)[1] <- 'GEOID'
    mapdata_merged <- dplyr::left_join(f_data,test[,c("GEOID","geometry")], "GEOID", "GEOID")
    if (unique(mapdata_merged$dataset_id == 4)){
      new_filtered <- aggregate(as.numeric(value) ~ GEOID, data = mapdata_merged, FUN = mean)
      names(new_filtered)[1] <- 'GEOID'
      names(new_filtered)[2] <- 'value'
      new_merged <- dplyr::inner_join(new_filtered,test[,c("GEOID","geometry")], "GEOID","GEOID")
      mapdata_merged_sf <- new_merged
    }
    # transfer to spatial dataset
    mapdata_merged_sf <-st_as_sf(mapdata_merged)
    pal_fun_num <- colorNumeric("Blues", NULL, n =7)
    print("for pop up")
    print(mapdata_merged_sf)
    #pal_fun_cat <- colorFactor("YlOrRd", NULL, n =7)
    
    ##################### add pop up for data description #############################
    if (unique(mapdata_merged_sf$variables) == "Percent Voted"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Percent of householder living alone"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Percent Insured Estimate"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Food insecurity rate"){
      p_popup <- paste0("<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate: </strong>", unique(mapdata_merged_sf$value),"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Metro area cost-burdened households (%)"){
      p_popup <- paste0("<strong> Metro Area: </strong>", mapdata_merged_sf$metro_area,"<br/>",
                        "<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate: </strong>", unique(mapdata_merged_sf$value),"%","<br/>",
                        "<strong> Data Description: </strong>","County in the same metro area share the same data point")
    }
    else if (unique(mapdata_merged_sf$variables) == "Total number of household with internet subscription"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      print(total_state)
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"(",round(as.numeric(mapdata_merged_sf$value)/total_state*100,2),"%)")
    }
    else if (unique(mapdata_merged_sf$variables) == "Children aged 3-4 enrolled in school (percent)"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Adjusted cohort graduation rate"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      print(total_state)
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%","<br/>",
                        "<strong> Data Description: </strong>","Adjusted cohort graduation rate is the number of students who graduate in four years or less with a  regular high school diploma divided by the number of students who form the adjusted-cohort.","<br/>","<br/>",
                        mapdata_merged_sf$value, "% is average school graduation rate in ", mapdata_merged_sf$geo_name,", please click 'Map Data' page for details of graduation rate for each school.")
    }
    else if (unique(mapdata_merged_sf$variables) == "National risk index score"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", round(as.numeric(mapdata_merged_sf$value),2),"<br/>",
                        "<strong> Data Description: </strong>","<br/>",
                        "The National Risk Index (NRI) is an online tool to help illustrate the nation's communities most 
                        at risk of natural hazards. It leverages authoritative nationwide datasets and multiplies values for exposure, hazard frequency, and historic
                        loss ratios to derive Expected Annual Loss for 18 natural hazards; and it combines this metric with Social Vulnerability and Community Resilience
                        data to generate a unitless, normalized Risk Index score for every census tract and county in the United States.","<br/>","<br/>",
                        "The NRI incorporates data for the following natural hazards: Avalanche, Coastal Flooding, Cold Wave, Drought, Earthquake, Hail, Heat Wave,
                        Hurricane, Ice Storm, Landslide, Lightning, Riverine Flooding, Strong Wind, Tornado, Tsunami, Volcanic Activity, Wildfire, and Winter Weather.")
    }
    
    else if (unique(mapdata_merged_sf$variables) == "County-level real gdp"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      print(total_state)
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", as.numeric(mapdata_merged_sf$value),"(",round(as.numeric(mapdata_merged_sf$value)/total_state*100,2),"%)")
    }
    else if (unique(mapdata_merged_sf$variables) == "Gdp percent change from preceding period"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", as.numeric(mapdata_merged_sf$value),"%")
    }
    else if (unique(mapdata_merged_sf$variables) == "Hudper100"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>",round(as.numeric(mapdata_merged_sf$value),2),"<br/>",
                        "<strong> Data Description: </strong>","Hudper100 is HUD units per 100 ELI renters (=(HUD/Total)*100)")
    }
    else if (unique(mapdata_merged_sf$variables) == "Usdaper100"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>","USDAper100","<br/>",
                        "<strong> Total estimate: </strong>",round(as.numeric(mapdata_merged_sf$value),2),"<br/>",
                        "<strong> Data Description: </strong>","USDAper100 is USDA units per 100 ELI renters (=(USDA/Total)*100)")
    }
    else if (unique(mapdata_merged_sf$variables) == "Native Analysis Value"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"<br/>",
                        "<strong> Data Description: </strong>","Native Analysis Value of Emergency department visit rate per 1000 beneficiaries")
    }
    else if (unique(mapdata_merged_sf$variables) == "Jail Population Rate" | unique(mapdata_merged_sf$variables) == "Prison Population Rate"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value," per 100,000 residents age 15-64")
    }
    else if (unique(mapdata_merged_sf$variables) == "Top-to-bottom ratio"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"<br/>",
                        "<strong> Data Description: </strong>","Ratio of top 1% income to bottom 99% income for all U.S. counties")
    }
    else{
      p_popup <- paste0("<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate </strong>", round(as.numeric(mapdata_merged_sf$value),2))
    }
    req(input$linelocation)
    req(input$geo_level)
    req(input$line_state)
    print("Req")
    print(input$linelocation)
    print(input$geo_level)
    print(input$line_state)
    
    if (input$line_state=="Washington"){
      lng = -120.740135
      lat = 47
    }
    else{
      lng = -76.61
      lat = 39.3
    }
    if(input$line_state =="Washington"){
      if(input$geo_level == "State"){
        zoom = 5.5
      }
      else if (input$geo_level == 'County' | input$geo_level == 'Tract'){
        zoom = 7.5
      }
      else{
        zoom=6.5
      }
    }
    else if(input$line_state == "Maryland"){
      if(input$geo_level == 'State'){
        zoom=6.5
      }
      else if (input$geo_level == 'ZCTA'){
        zoom = 7.5
      }
      else{
        zoom = 8.5
      }
    }
    leaflet(mapdata_merged_sf) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      addPolygons(
        color = "black",
        weight = 1,
        stroke = TRUE, 
        smoothFactor = 1, 
        fillOpacity = 0.55,
        fillColor = ~pal_fun_num(as.numeric(value)), # set fill color with function from above and value
        popup = p_popup,
        label = ~lapply(paste0(geo_name, "<br>", unique(mapdata_merged_sf$variables), ": ",round(as.numeric(value),2)),HTML),
        highlightOptions = highlightOptions(weight = 2,
                                            color = "white",
                                            fillOpacity = 0.7, 
                                            opacity =1,
                                            bringToFront = T)) %>% 
      addLegend("bottomright",  # location
                pal = pal_fun_num,     # palette function
                values = ~as.numeric(value),
                title = unique(mapdata_merged_sf$variables)) %>% # legend title 
      setView(lng = lng, lat = lat, zoom = zoom)
  })
  
  output$lineplot <- renderPlot({
    lineloc<-linelocation()
    geo_level_data <-geo_level()
    if (unique(geo_level_data$variables) == "Adjusted cohort graduation rate"){
      print("lineloc_data after filter")
      lineloc <- aggregate(as.numeric(lineloc$value), list(lineloc$year,lineloc$variables,lineloc$geo_name),FUN = mean)
      names(lineloc)[1] <- 'year'
      names(lineloc)[2] <- 'variables'
      names(lineloc)[3] <- 'geo_name'
      names(lineloc)[4] <- 'value'
      geo_level_data <- aggregate(as.numeric(geo_level_data$value), list(geo_level_data$geo_level,geo_level_data$year,geo_level_data$geo_name), FUN=mean)
      names(geo_level_data)[1] <- 'geo_level'
      names(geo_level_data)[2] <- 'year'
      names(geo_level_data)[3] <- 'geo_name'
      names(geo_level_data)[4] <- 'value'
      geo_level_data<-unique(geo_level_data)
    }
    
    US_values <- aggregate(as.numeric(geo_level_data$value), list(geo_level_data$year), FUN=mean)
    names(US_values)[1] <- 'year'
    names(US_values)[2] <- 'value'
    
    
    plotdata = merge(lineloc[,c("year","variables","value")], US_values[,c("year","value")], by="year")
    names(plotdata)[3] <- unique(lineloc$geo_name)
    names(plotdata)[4] <- 'Average'
    datamelted <- reshape2::melt(plotdata[,c("year", unique(lineloc$geo_name),"Average")], id.var='year')
    ggplot(datamelted, aes(x=year, y=as.numeric(value), col=variable,group=variable)) + geom_line()+geom_point(size=2)+
      ggtitle(paste0(unique(lineloc$variables), " Trend Line Comparison"))
  })
  
  output$mytable = DT::renderDataTable({
    x<-year()
    names(x)[5] <- 'values'
    names(x)[11] <- 'geo_names'
    names(x)[12] <- 'geo_levels'
    names(x)[13]<- "metro_areas"
    names(social_index_dataset_copy)[17] <- 'variables1'
    y <- dplyr::left_join(x,social_index_dataset_copy,by=c("dataset_id"="dataset_id","domain"="domain","subdomain"="subdomain",
                                                           "indicator"="indicator","variables"="variables1","year"="year","sex"="sex","race" ="race","age"="age",
                                                           "geo_id"="geo_id","NCESSCH" ="NCESSCH","SCHNAM"="SCHNAM","metro_areas"="metro_area"))
    y<- y[,c("domain","subdomain","indicator","variables","sex","race","age","year","value","geo_name","NCESSCH","SCHNAM")]
    datatable(y,
              rownames = F,
              extensions = 'Buttons',
              options = list(dom = 'Bt', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             scrollX = TRUE),
              colnames=c("domain","subdomain","indicator","variables","sex","race","age","year","value","geo_name","NCESSCH","SCHNAM"))
    
    #options = list(pageLength=50, scrollX='400px')
  })
  
  
  #######################location profile view########################
  output$text <- renderText({ 
    paste0(input$linelocation," Demographics:")
  })
  
  racedata <- reactive({
    filter(tbl_race_geo,geo_name == input$linelocation)
  })
  
  observeEvent(racedata(),{
    print("update year slider")
    print(racedata())
    choices = sort(unique(racedata()$year))
    print("choices")
    print(choices)
    updateSliderInput(session,'yearperiod', value=range(choices),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  yearperiod <- reactive({
    print("year changed")
    print(input$yearperiod)
    filter(filter(racedata(), year == input$yearperiod),label != "estimate_total_population")
  })
  
  
  output$raceplot <- renderPlot({
    print("raceplot")
    racedataset <- yearperiod()
    
    pie<-ggplot(racedataset, aes(x = "", y=as.numeric(estimate), fill = factor(label))) + 
      geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="Class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of Race")
    
    pie + coord_polar(theta = "y", start=0)+
      scale_fill_viridis(discrete = TRUE)+
      theme(panel.background = element_blank())
  })
  
  ############age
  agedata <- reactive({
    filter(tbl_age_geo,geo_name == input$linelocation)
  })
  
  observeEvent(agedata(),{
    print("update year slider")
    print(agedata())
    choices = sort(unique(agedata()$year))
    print("choices")
    print(choices)
    updateSliderInput(session,'yearperiod2', value=range(choices),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  yearperiod2 <- reactive({
    print("year changed")
    print(input$yearperiod2)
    filter(agedata(), year == input$yearperiod2)
  })
  
  output$ageplot <- renderPlot({
    print("ageplot")
    agedataset <- yearperiod2()
    agedataset$label <- factor(agedataset$label, levels = agedataset$label)
    print(agedataset)
    ggplot(agedataset, aes(x = label, y =as.numeric(estimate),fill=as.numeric(estimate)))+
      coord_flip() +
      scale_fill_continuous(type = "viridis",name = "Number of People")+theme(legend.position="right")+
      geom_bar(position="stack",stat="identity")+
      xlab("Age Group") +
      ylab("Number of People")+
      ggtitle("Bar Chart of Age")+
      theme(panel.background = element_blank())
  })
  
  
  ############pop
  popdata <- reactive({
    filter(tbl_pop_geo,geo_name == input$linelocation)
  })
  
  output$popplot <- renderPlot({
    print("poplot")
    popdataset <- popdata()
    print(popdataset)
    ggplot(popdataset, aes(x=as.factor(year), y = as.numeric(estimate),fill= as.numeric(estimate)))+
      geom_bar(stat="identity")+scale_fill_continuous(type = "viridis",name = "Number of People")+
      xlab("Year") +
      ylab("Number of People")+
      ggtitle("Bar Chart of Population")+
      theme(panel.background = element_blank())
  })
  
  ############le
  ledata <- reactive({
    filter(tbl_le_geo,geo_name == input$linelocation)
  })
  
  output$leplot <- renderPlot({
    print("poplot")
    ledataset <- ledata()
    print(ledataset)
    ggplot(ledataset, aes(x=year, y = as.numeric(estimate),fill= as.numeric(estimate)))+
      geom_bar(stat="identity")+scale_fill_continuous(type = "viridis",name = "Number of People")+
      xlab("Year") +
      ylab("Number of People")+
      ggtitle("Bar Chart of Life Expectancy")+
      theme(panel.background = element_blank())
  })
  ##############profile data##################
  output$demogeotext <- renderText({ 
    paste0(input$linelocation)
  })
  
  selectdemo <- reactive({
    req(input$selectdemo)
    req(input$linelocation)
    print("select selected demo changed")
    print(input$selectdemo)
    if (input$selectdemo=="Age"){
      filter(tbl_age_geo,geo_name == input$linelocation)
    }
    else if (input$selectdemo=="Race"){
      filter(tbl_race_geo,geo_name == input$linelocation)
    }
    else if (input$selectdemo=="Population"){
      filter(tbl_pop_geo,geo_name == input$linelocation)
    }
    else{
      filter(tbl_le_geo,geo_name == input$linelocation)
      
    }
  })
  
  observeEvent(selectdemo(),{
    print("select demo")
    print(selectdemo())
    output$profiledata = DT::renderDataTable({
      datatable(selectdemo(),
                rownames = F,
                extensions = 'Buttons',
                options = list(dom = 'Bt', 
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               scrollX = TRUE))
      #colnames=c("domain","subdomain","indicator","variables","sex","race","age","year","value","geo_name","NCESSCH","SCHNAM"))
      
    })
  })
}

shinyApp(body, server)