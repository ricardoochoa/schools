# global.R

# Librerías
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(reshape2)
library(plotly)
library(ProgGUIinR)
library(scales) 

# Archivos
source(file = "functions.R")
holidays_table <- read.csv("_data/holidays.csv")
holidays_table$month_name <-factor(holidays_table$month_name, levels=month.abb[1:12]) 
use_table <- read.csv("_data/usefactors.csv")
prefilter_table <- read.csv("_data/prefilters.csv")
filter_table <- read.csv("_data/filters.csv")
runoff_table <- read.csv("_data/runoff.csv")
tanks_table <- read.csv("_data/cisternsizes.csv")
empty_file <- read.csv("_data/empty_file.csv")

water_use_peruser_perday <- 20

#Renamed tables for plotting values
prefilter_table_P <- prefilter_table
  colnames(prefilter_table_P) <- c("Prefilter type", "Waste", "Coefficient", "Cost")
runoff_table_P <- runoff_table
  colnames(runoff_table_P) <- c("Material", "With slope", "Without slope")
tanks_table_P <- tanks_table
  colnames(tanks_table_P) <- c("Size", "Price")
use_table_P <- use_table
  colnames(use_table_P) <- c("Use", "Demand per capita/day")
my.results <- data.frame(use = c("lluvia", "red"), value = c(30, 70))

# Prepare data
# _____ import municipality and state data
load(file = "_data/all_municipalities.RData")
load(file = "_data/all_states.RData")
# _____ import precipitation data
available_data <- 
  data.frame(
    CVE_ENT = substr(x = list.files("_data/precipitation"), 
                     start = 1, 
                     stop  = 2), 
    CVE_MUN = substr(x = list.files("_data/precipitation"), 
                     start = 3, 
                     stop  = 5))
available_data$CVE_ENT <- as.character(available_data$CVE_ENT)
available_data$CVE_MUN <- as.character(available_data$CVE_MUN)

# _____ subset data 
# _____ _____ states
all_states <- merge(all_states, available_data[-2]) # subset only states with data
all_states[!duplicated(all_states), ] # remove duplicated lines
# _____ _____ municipalities
my.data <- merge(all_municipalities, available_data) # subset only municipalities with data



