library(shiny)
#library(shinydashboard)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly) #Spiderchart
library(robservable)
library(tidyverse) #Needed for read_csv (not to be confused with read.csv)
library(readr)
library(readxl)
library(ggplot2)
library(leaflet)
library(htmltools)
library(scales)
#library(shinyBS) #Jquery conflict, have to switch to bsplus which is updated
library(prompter)
library(dplyr)

#import the R files inside the R folder and process them
lapply(list.files("R"), FUN = function(x) source(paste0("R/", x)))
