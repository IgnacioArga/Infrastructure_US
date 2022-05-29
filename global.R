
# Libraries ---------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(maps)

library(tidyverse)
library(readr)


# Load Data ---------------------------------------------------------------

infrastucture_data <- read.csv("Input/infrastucture_data.csv")

geo_data <- read.csv("Input/geo_data.csv")

states <- map_data("state")

high_cat <- infrastucture_data$high_cat %>% unique()
sub_cat <- infrastucture_data$category %>% unique()


unit <- c("Gross investment","Gross investment at 2012","GDP")
unit_geo <- c("Gross investment","Gross investment at 2012","GDP","Population","Investment at 2012 per capita")

# Modules -----------------------------------------------------------------


invisible(lapply(list.files(path = "modules", full.names = T), source))

