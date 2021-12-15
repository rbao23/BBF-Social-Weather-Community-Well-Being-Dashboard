# BBF-Social-Weather-Community-Well-Being-Dashboard

This is Shiny web application for Social Weather Community Well-Being Dashboard, used to be embedded in social weather website (https://uw-social-weather.netlify.app/). The app is currently published on rsc.csde.washington.edu and can be accessed via https://rsc.csde.washington.edu/content/038ba60e-b58c-4895-abb1-54507b0d9323/

## File Description
### app_rsc_local.R 
 This app is currently published on rsc.csde.washington.edu and used to integrate into the social weather website.
 Note: this app reads local tables in dataset folder.
### app.R 
  This App is currently NOT used to publish on rsc.csde.washington.edu and integrate into the social weather website, BUT will require 
  for the future once the database authentication to shiny server issue is solved.
  Note: this app reads tables in bbfsw database
### code base
  This folder is used to connect bbfsw database, and please make sure when it goes public, password in pgpass.R file is removed.
  
## Current Dashboard
  1. For dataset_id = 4, it currently takes average of school graduation rate for each county, the methodology might change later after funders review it.
  2. Line trend comparison plot originally tends to compare with United States data. However, due to lack of US data, we take the averages of location data in each geographic level.

## Question
For any other questions regarding to dashboard, please contact Ruihan(Bonnie) Bao at ruihab@uw.edu
