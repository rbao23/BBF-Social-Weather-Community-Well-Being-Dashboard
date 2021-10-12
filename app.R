#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libaries
#library(shiny)
library(shinythemes)
library(dplyr)
library(RPostgreSQL)
library(magrittr)
library(tidyverse)

#library(shinydashboard)

################## Getting data from the database e#############################################

# script to connect
source("code_base/dbconnect.R")
# password env var
source("code_base/pgpass.R")

# create a connection to the postgresql database
# note that "con" will be used later in each connection to the database
con <- connectDB(host = "bffsw.csde.washington.edu", dbname = "ruihab_sb")

# test
dbListTables(conn = con) 


# get the required tables from the sql database 
social_index_dataset = dbGetQuery(con, "SELECT * from public.tbl_dataset_info
                                        inner JOIN public.tbl_social_weather_dataset using(dataset_id)")

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
social_index_dataset$subdomain[social_index_dataset$subdomain=='/N']  <- "N/A"

# disconnect database
dbDisconnect(con) 

############################################### ui.R ##################################################

body<-navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 title = "Social Weather Community Well-Being Dashboard",
                 tabPanel("Comparion Map",h2("Select Dataset"),
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("domain","Domain:",choices=sort(unique(social_index_dataset$domain))),
                                  selectInput("subdomain", "Subdomain:", choices=NULL),
                                  selectInput("indicator", "Indicator:", choices=NULL),
                                  selectInput("sex", "Sex:",choices=NULL),
                                  selectInput("race", "Race:",choices=NULL),
                                  selectInput("age", "Age:",choices=NULL),
                                  selectInput("year", "Year:",choices=NULL)
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("US Map View", verbatimTextOutput("mapview"),
                                               fluidRow(column(11, wellPanel(tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar{
                                                     background: #48C9B0;
                                                     border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                                             sliderInput("one", "Select Mapping Year", value =1990, min = 1990, max=2021, step=1,animate=TRUE,ticks=TRUE),
                                                                             leafletOutput("mymap"),
                                                                             p(),
                                                                             actionButton("recalc", "New points")
                                               )))),
                                      tabPanel("State Profile View", verbatimTextOutput("summary")),
                                      tabPanel("County Profile View", tableOutput("table1"),
                                               fluidRow(column(8, wellPanel(
                                                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar{
                                                     background: #48C9B0;
                                                     border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                   sliderInput("two", "Select Mapping Year", value =2021, min = 1990, max=2021, step=1)
                                               )))),
                                      tabPanel("Tract Profile View", tableOutput("table2"),
                                               fluidRow(column(8, wellPanel(
                                                   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar{
                                                     background: #48C9B0;
                                                     border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                   sliderInput("three", "Select Mapping Year", value =2021, min = 1990, max=2021, step=1)
                                               )))),
                                      tabPanel("Zip Code Profile View", tableOutput("table3"),
                                               fluidRow(column(8, wellPanel(
                                                   tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar{
                                                     background: #48C9B0;
                                                     border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                   sliderInput("four", "Select Mapping Year", value =2021, min = 1990, max=2021, step=1)
                                               ))))
                                  )
                              ))), 
                 tabPanel("About this site",h2("About Social Weather Index"))
)

############################################### server.R ##################################################

# Define server logic required to draw a histogram
server <- function(input,output,session) {
    #reactive selectinput
    domain <- reactive({
        print("event domain")
        req(input$domain)
        filter(social_index_dataset, domain == input$domain)
    })
    observeEvent(domain(), {
        print("event subdomain")
        updateSelectInput(session,"subdomain", choices = sort(unique(domain()$subdomain)))
    })
    subdomain <- reactive({
        req(input$subdomain)
        filter(domain(), subdomain == input$subdomain)
    })
    observeEvent(subdomain(), {
        print("event indicator")
        updateSelectInput(session,inputId = "indicator", choices = sort(unique(subdomain()$indicator)))
    })
    indicator <- reactive({
        req(input$indicator)
        filter(subdomain(), indicator == input$indicator)
    })
    observeEvent(indicator(), {
        print("event sex")
        updateSelectInput(session,inputId = "sex", choices = sort(unique(indicator()$sex)))
    })
    sex <- reactive({
        req(input$sex)
        filter(indicator(), sex == input$sex)
    })
    observeEvent(sex(), {
        print("event race")
        updateSelectInput(session,inputId = "race", choices = sort(unique(sex()$race)))
    })
    race <- reactive({
        req(input$race)
        filter(sex(), race == input$race)
    })
    observeEvent(race(), {
        print("event age")
        updateSelectInput(session,inputId = "age", choices = sort(unique(race()$age)))
    })
    
    age <- reactive({
        req(input$age)
        filter(race(), age == input$age)
    })
    
    observeEvent(age(), {
        print("event year")
        updateSelectInput(session,inputId = "year", choices = sort(unique(age()$year)))
    })
    
    year <- reactive({
        req(input$year)
        filter(age(), year == input$year)
    })
    # reactive sliderinput
    observeEvent(year(),{
        updateSliderInput(session, "one", value = input()$year,
                          min = min(age()$year), max = max(age()$year), step = 1)
    })
    
    # Reactive expression to create data frame of all input values ----
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
    
    
    
    
    

}

shinyApp(body, server)
