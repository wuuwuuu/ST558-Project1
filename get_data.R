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


convert_to_tibble <- function(response) {
  parsed <- fromJSON(rawToChar(reponse$content))
  colnames(parsed) <- parsed[1,]
  as_tibble(parsed[-1,,drop=FALSE])
}

get_census_data <- function(year =2024,
                            num_vars = c("AGEP", "PWGTP"),
                            cat_vars = "SEX",
                            geography = "state",
                            geo_value = NULL) {
  if (!year %in% 2021:2024 || length(year) != 1) {
    stop("Year must be a year from 2021-2024 and only a single value")
  }
  
  #Numerical variable checks
  allowed_num_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  num_vars <- unique(c(num_vars, "PWGTP"))
  
  #check for user inputs to be within allowed numeric inputs
  if (!all(num_vars %in% c(allowed_num_vars,"PWGTP"))) {
    stop("numeric variables must be from the list: ", paste(allowed_num_vars))
  }
  
  #check for PWGTP and at least 1 more numeric variable
  if (length(num_vars) < 2) {
    stop("Must provide at least 1 numeric variable other than PWGTP")
  }
  
  # Categorical variable checks
  allowed_cat_vars <- c("SEX", "FER", "HHL", "SCH", "SCHL")
  #check for user inputs to be within allowed categorical inputs
  if (!all(cat_vars %in% allowed_cat_vars)) {
    stop("categoricals variables must be from the list: ", paste(allowed_cat_vars))
  }
  
  #check at least 1 categorical variable is inputted
  if (cat_vars < 1) {
    stop("Must input at least 1 categorical variable")
  }
  
  #Geography variable checks
  geography <- toupper(geography)
  allowed_geography <- c("REGION", "DIVISION", "STATE")
  #check if geography has more than 1 value and within allowed values
  if (length(geography) != 1 || geography %in% allowed_geography) {
    stop("Geography must have only 1 value and be either region, state, or division")
  }
  
  rds_geo <- readRDS(paste0("data/", year, "_data.rds"))
  #change state to either ST or state b
  geo_var <- if (geography == "STATE" && year %in% 2021:2022) "ST" else "STATE"
  valid_codes <- names(var_info[[geo_var]]$values$item)
  
  #check geo values
  if (is.null(geo_value)) {
  }
}

