#script for getting census data
library(jsonlite)
library(httr)
get_data <- function(year) {
  url <- paste0("https://api.census.gov/data/", year,
                "/acs/acs1/pums/variables.json")
  response <- GET(url)
  parsed_data <- fromJSON(rawToChar(response$content))
  
  state_var <- if (year %in% c(2021, 2022)) "ST" else "STATE"
  variables <- c("AGEP", "GASP", "GRPIP", 
                 "JWAP", "JWDP","JWMNP", "PWGTP",
                 "FER", "HHL", "SCH", 
                 "SCHL", "SEX", state_var, "REGION", "DIVISION")
  parsed_data$variables[variables]
  
}
