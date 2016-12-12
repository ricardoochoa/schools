# This file contains a set of functions for RWH calculations
# Most of this functions are based on the SAAVi 2.0 technical 
# manual (see page 30). However, some variables were renamed 
# (translated to english).

# Function: get_cve_geo
# Description
# Finds the INEGI's cvegeo from a state name and a municipality name.
# Usage
# get_cve_geo(nom_ent, nom_mun, ...)
# Arguments
# nom_ent (character): The name of a Mexican state
# nom_mun (character): The name of a Mexican municipality
# address (logical): Whether the output should have a filename (default, TRUE) or only the cvegeo. 
# if address is set to TRUE (dafault), the output can be used to import the precipitation 
# file of the selected state and municipality.
get_cve_geo <- function(nom_ent, nom_mun, address = T){
  the.row <- subset(my.data, NOM_ENT == nom_ent & NOM_MUN == nom_mun)
if(address == T){
  return(
    paste0(
      "_data/precipitation/",
      the.row["CVE_ENT"],
      the.row["CVE_MUN"], 
      ".csv")
  )
} else {    
  return(
    paste0(
      the.row["CVE_ENT"],
      the.row["CVE_MUN"])
  )}
}

# Function: read_precipitation_file
# Description
# Returns a dataframe with hourly precipitation for a selected state and municipality. 
# The dataframe contains the follwing variables: month (m), day (d) and precipitation (p).
# Usage
# read_precipitation_file(nom_ent, nom_mun)
# Arguments
# nom_ent (character): The name of a Mexican state
# nom_mun (character): The name of a Mexican municipality
read_precipitation_file <- function(nom_ent, nom_mun){
  return(
    aggregate(p~d+m,
              FUN = sum, 
              data = 
                tryCatch({
                        read.csv(file = get_cve_geo(nom_ent, nom_mun), 
                                 header = F, 
                                 col.names = c("m", "d", "p"))
                }, warning = function(w) {
                  empty_file # if you find a warning, return an empty table
                }, error = function(e) {
                  empty_file # if you find an error, return an empty table
                }, finally = {
                  # do nothing  
                })
    )
  )
}

# Function: get_runoffcoeficient
# Description
# Calculate the runoff coeficient of a selected roof and slope
# Usage
# get_runoffcoeficient(material, slope)
# Arguments
# material (character): the roof material, according to the runnoff_table dataframe
# slope (logical): whether the slope is higher than 5%
get_runoffcoeficient <- function(material, slope){
  return(
    ifelse(test = slope, 
           yes = runoff_table[which(runoff_table$material == material), "with_slope"], 
           no = runoff_table[which(runoff_table$material == material), "without_slope"]))
}

# Function: get_prefiltercoeficient
# Description
# Calculate the pre-filter coeficient from a selected pre-filter type
# Usage
# get_prefiltercoeficient(prefilter)
# Arguments
# prefilter (character): the pre-filter type, according to the prefilter_table dataframe
get_prefiltercoeficient <- function(prefilter){
  return(prefilter_table[which(prefilter_table$prefilter == prefilter), "coefficient"])
}

# Function: get_usefactor
# Description
# Calculate the water use factor from a set of possible water uses.
# Usage
# get_usefactor(use)
# Arguments
# use (character): the type of use, according to the use_table dataframe
get_usefactor <- function(use){
  return(use_table[which(use_table$use == use), "demand_percapita_perday"])
}

# Function: get_someanswers
# Description
# 
# Usage
# 
# Arguments
# 
get_someanswers <- function(precipitation_file,    # file
                            area, material, slope, # catchment
                            #filter, # filter
                            prefilter,     # prefilter
                            users, use,      # use
                            tank){              # storage
  # Calulate water demand
  precipitation_file$demand <- users * get_usefactor(use) * holidays_table$active
  # Calculate harvested rainwater
  precipitation_file$supply <- area * 
                               precipitation_file$p * 
                               get_prefiltercoeficient(prefilter = prefilter) *
                               get_runoffcoeficient(material = material, slope = slope)
  # Inicialize variables
  precipitation_file$usedrw <- 0
  precipitation_file$clevel <- 0
  precipitation_file$wasted.water <- 0
  
  # First line
  precipitation_file$usedrw[1] <- min(tank, 
                                      precipitation_file$demand[1], 
                                      0 + precipitation_file$supply[1])
  precipitation_file$wasted.water[1] <- max(0, precipitation_file$usedrw[1] - precipitation_file$demand[1] - tank)
  precipitation_file$clevel[1] <- max(0, precipitation_file$usedrw[1] - precipitation_file$demand[1] - precipitation_file$wasted.water[1])

  
  # All the rest
  for(c in 2:365){
    precipitation_file$usedrw[c] <- min(tank, 
                                        precipitation_file$demand[c], 
                                        precipitation_file$clevel[c-1] + precipitation_file$supply[c])
    precipitation_file$wasted.water[c] <- max(0, precipitation_file$clevel[c-1] + precipitation_file$usedrw[c] - precipitation_file$demand[c] - tank)
    precipitation_file$clevel[c]       <- max(0, precipitation_file$clevel[c-1] + precipitation_file$supply[c] - precipitation_file$demand[c] - precipitation_file$wasted.water[c])
  }
  
    return(precipitation_file)
}

# GET WATER DEMAND
get_waterdemand <- function(precipitation_file,    # file
                            area, material, slope, # catchment
                            #filter, 
                            prefilter,     # prefilter
                            users, use,      # use
                            tank){              # storage

  temp.table <- tryCatch({get_someanswers(precipitation_file,    # file
                          area, material, slope, # catchment
                          #filter, 
                          prefilter,     # prefilter
                          users, use,      # use
                          tank)}, warning = function(w) {
                            data.frame(m = 0, d = 0, p = 0, demand = 0, supply = 0, usedrw = 0, clevel = 0, wasted.water = 0)
                          }, error = function(e) {
                            # do nothing
                          }, finally = {
                            # do nothing  
                          })
    return(
      round(100*sum(temp.table$usedrw)/sum(temp.table$demand), 2)
  )
}

# GET TOTAL USED RAINWATER 
get_rain <- function(precipitation_file,    # file
                     area, material, slope, # catchment
                     #filter, 
                     prefilter,     # prefilter
                     users, use,      # use
                     tank){              # storage
  
  temp.table <- tryCatch({get_someanswers(precipitation_file,    # file
                                          area, material, slope, # catchment
                                          #filter, 
                                          prefilter,     # prefilter
                                          users, use,      # use
                                          tank)}, warning = function(w) {
                                            data.frame(m = 0, d = 0, p = 0, demand = 0, supply = 0, usedrw = 0, clevel = 0, wasted.water = 0)
                                          }, error = function(e) {
                                            # do nothing
                                          }, finally = {
                                            # do nothing  
                                          })
  return(
    round(sum(temp.table$usedrw), 2)
  )
}

# GET OPTIMAL TANK SIZE
get_optimaltanksize <- function(precipitation_file,    # file
                                   area, material, slope, # catchment
                                   #filter, 
                                   prefilter,     # filter
                                   users, use){           # use
  
  fill.table <- data.frame(m = numeric(), d = numeric(), 
                           p = numeric(), demand = numeric(), 
                           supply = numeric(), usedrw = numeric(), 
                           clevel = numeric(), wasted.water = numeric(), 
                           tank = character())
  
  for(tank in tanks_table$size){
    temp.table <- tryCatch({get_someanswers(precipitation_file,    # file
                                            area, material, slope, # catchment
                                            #filter, 
                                            prefilter,     # prefilter
                                            users, use,      # use
                                            tank)}, warning = function(w) {
                                              data.frame(m = 0, d = 0, p = 0, demand = 0, supply = 0, usedrw = 0, clevel = 0, wasted.water = 0)
                                            }, error = function(e) {
                                              # do nothing
                                            }, finally = {
                                              # do nothing  
                                            })
    temp.table$tank <- tank
    fill.table <- rbind(fill.table, temp.table)
  }
  
  fill.table$tank <- as.factor(fill.table$tank)
  
  fill.table <- tryCatch({ddply(fill.table, c("tank"), summarise,
                                "Rainwater" = sum(usedrw), 
                                "Other source"   = sum(demand - usedrw))}, warning = function(w){
                                  data.frame(m = numeric(), d = numeric(), 
                                             p = numeric(), demand = numeric(), 
                                             supply = numeric(), usedrw = numeric(), 
                                             clevel = numeric(), wasted.water = numeric(), 
                                             tank = character())
                                }, error = function(e) {
                                  # do nothing
                                }, finally = {
                                  # do nothing  
                                })
  
  fill.table <- melt(fill.table,
                     id.vars=c("tank"), 
                     variable.name="Source", 
                     value.name="Volume")
  
  return(fill.table)
}

get_costs <- function(prefilter, size, area, filter, users, tabular = FALSE){
  # Assumptions: 
    # The school will require one prefilter for the first 100 m2 and one more for every additional 50 m2 
    # The installation represents the 20% of the total costs. 
  number_of_prefilters <- ifelse(area <= 100, 1, (area) %/% 50)
  
  number_of_filters <- ifelse(users <= 10, 1, (users) %/% 10)
  
  subtotal <- (number_of_prefilters *
                prefilter_table[which(prefilter_table$prefilter == prefilter), "cost"] +
                tanks_table[which(tanks_table$size == size), "price"] +
                number_of_filters * filter_table[which(filter_table$filter == filter), "cost"])
  
  installation_costs <- 0.20 * subtotal
  
  total_cost <- data.frame(Description = c("pre-filters", "tank", "filters", "installation", "TOTAL"), 
                           QTY  = c(number_of_prefilters, 1, number_of_filters, 1, ""),
                           Unit_price =  c(prefilter_table[which(prefilter_table$prefilter == prefilter), "cost"], 
                                           tanks_table[which(tanks_table$size == size), "price"], 
                                           filter_table[which(filter_table$filter == filter), "cost"], 
                                           installation_costs, 
                                           ""),
                           Amount =   c(number_of_prefilters *
                                      prefilter_table[which(prefilter_table$prefilter == prefilter), "cost"], 
                                      tanks_table[which(tanks_table$size == size), "price"], 
                                      filter_table[which(filter_table$filter == filter), "cost"], 
                                      installation_costs, 
                                      1.20 * subtotal))
  
  ifelse(tabular, 
         return(total_cost), 
         return(total_cost[5, "Amount"]))
}

# MEN AT WORK
get_rwdemand <- function(holidays, users, use_factor){
  return(1)
}

get_rwsupply <- function(){
  return(1)
}
