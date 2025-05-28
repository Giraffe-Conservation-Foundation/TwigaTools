#### R SHINY; ER & WB ##########################################################
### GCF CM v1.0 20240924
### GCF CM v1.1 20250211 updated giraffe_nw_monitoring with giraffe_nw_monitoring0 for Audi 2024 data processing
### GCF CM v1.2 20250227 updated for both giraffe_nw_monitoring AND giraffe_nw_monitoring0 
### GCF CM v1.3 20250410 updated to add leading zero for images when <4 numbers are retained
### GCF CM v1.4 20250410 updated to add leading zero for images when <4 numbers are retained

#install.packages(c("shiny", "shinyFiles", "rsconnect", "httr", "jsonlite", "dplyr", "lubridate", "fs", "progress", "shinyWidgets", "shinyjs", "writexl", "purr"))

#### LIBRARIES #################################################################
library(shiny)
library(shinyFiles)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(writexl)
library(purrr)
library(sf)
library(exifr)
library(zip)
library(tidyverse)
library(lubridate)
library(zip)


#### ACCOUNT AUTHENTICATION ####################################################
rsconnect::setAccountInfo(name='giraffeconservation',
                          token='9C40169B342D0A1EDC0AECB78C0F9B5D',
                          secret='crcMR9tXlS6VhysZz2XdQCdknTuq0RHmZabywXGT')
#rsconnect::deployApp("G:/My Drive/Data management/Shiny_ER2WB")


source("TT_mod1.R")
source("TT_mod2.R")  # you'll build this next
#source("TT_mod3.R")  # and this too

ui <- tagList(
  # header with title and logo
  div(style = "display: flex; justify-content: space-between; align-items: center; 
               padding: 10px 20px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
      div("Twiga Tools", 
          style = "font-size: 36px; font-weight: bold;"),
      tags$img(src = "GCF_logo.png", height = "80px")
  ),
  
  
  navbarPage(
    title = NULL,
    tabPanel("ER2WB", tab1UI("tab1")),
    tabPanel("ERshp", tab2UI("tab2"))
#    tabPanel("Shapefile Export", tab3UI("tab3"))
  )
)




server <- function(input, output, session) {
  tab1Server("tab1")
  tab2Server("tab2")
#  tab3Server("tab3")
}



#### RUN THE APPLICATION #######################################################
shinyApp(ui, server)