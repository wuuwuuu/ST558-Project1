#script for getting census data
library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)
library(tidyverse)

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

#helper function for converting to tibble
convert_to_tibble <- function(response) {
  parsed <- fromJSON(rawToChar(response$content))
  colnames(parsed) <- parsed[1,]
  as_tibble(parsed[-1,,drop=FALSE])
}
#helper function to parse the time
parse_time_midpoint <- function(labels) {
  pattern <- "(\\d{1,2}):(\\d{2}) ([ap])\\.m\\. to (\\d{1,2}):(\\d{2}) ([ap])\\.m\\."
  m <- str_match(labels, pattern)
  
  to_seconds <- function(hour, minute, ampm) {
    (as.numeric(hour) %% 12 + ifelse(ampm == "p", 12, 0)) * 3600 +
      as.numeric(minute) * 60
  }
  
  start <- to_seconds(m[, 2], m[, 3], m[, 4])
  end   <- to_seconds(m[, 5], m[, 6], m[, 7])
  seconds_to_period((start + end) / 2)
}

#helper function to look of time codes
lookup_time_code <- function (code, var_info) {
  lookup <- unlist(var_info$values$item)
  unname(lookup[code])
}
get_census_data <- function(year =2024,
                            num_vars = c("AGEP", "PWGTP"),
                            cat_vars = "SEX",
                            geography = "state",
                            geo_value = NULL,
                            api_key = "0065bb939f5c09eb016d3f55ea5f5522c49679e4") {
  if (length(year) != 1 || !year %in% 2021:2024) {
    stop("Year must be a year from 2021-2024 and only a single value")
  }
  
  #Numerical variable checks
  allowed_num_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  num_vars <- unique(c(num_vars, "PWGTP"))
  
  #check for user inputs to be within allowed numeric inputs
  if (!all(num_vars %in% c(allowed_num_vars,"PWGTP"))) {
    stop("numeric variables must be from the list: ", paste(allowed_num_vars, collapse=", "))
  }
  
  #check for PWGTP and at least 1 more numeric variable
  if (length(num_vars) < 2) {
    stop("Must provide at least 1 numeric variable other than PWGTP")
  }
  
  # Categorical variable checks
  allowed_cat_vars <- c("SEX", "FER", "HHL", "SCH", "SCHL")
  #check for user inputs to be within allowed categorical inputs
  if (!all(cat_vars %in% allowed_cat_vars)) {
    stop("categoricals variables must be from the list: ", paste(allowed_cat_vars, collapse = ", "))
  }
  
  #check at least 1 categorical variable is inputted
  if (length(cat_vars) < 1) {
    stop("Must input at least 1 categorical variable")
  }
  
  #Geography variable checks
  geography <- toupper(geography)
  allowed_geography <- c("REGION", "DIVISION", "STATE")
  
  #check if geography has more than 1 value and within allowed values
  if (length(geography) != 1 || !geography %in% allowed_geography) {
    stop("Geography must have only 1 value and be either region, state, or division")
  }
  
  rds_info <- readRDS(paste0("data/", year, "_data.rds"))
  #change state to either ST or state b
  geo_var <- if (geography == "STATE" && year %in% 2021:2022) "ST" else geography
  valid_codes <- names(rds_info[[geo_var]]$values$item)
  
  #check geo values
  if (is.null(geo_value)) {
    geo_value <- switch(geography, REGION = "3",DIVISION = "5", STATE= "37" )
  }
  
  #change 'all' to * 
  geo_value <- if (toupper(geo_value) == "ALL") "*" else as.character(geo_value)
  
  #check if geo value contains a valid code
  if (geo_value != "*" && !geo_value %in% valid_codes) {
    stop("Invalid geo code")
  }
  
  # get api
  url <- paste0("https://api.census.gov/data/", year, "/acs/acs1/pums?get=",
                paste(c(num_vars, cat_vars), collapse = ","),
                "&for=", tolower(geography), ":", geo_value,"&key=",api_key)
  reponse <- GET(url)
  
  result <- convert_to_tibble(reponse)
  
  # get time vars and only the numeric excluding the time vars
  time_vars <- intersect(num_vars, c("JWAP", "JWDP"))
  only_num <- setdiff(num_vars, time_vars)
  
  result <- result |> mutate(across(all_of(only_num), as.numeric))
  for (v in time_vars) {
    result[[v]] <- parse_time_midpoint(lookup_time_code(result[[v]],rds_info[[v]]))
  }
  
  for (v in cat_vars) {
    code <- names(rds_info[[v]]$values$item)
    labels <- unname(unlist(rds_info[[v]]$values$item))
    result[[v]] <- factor(result[[v]], levels = code, labels = labels)
  }
  class(result) <- c("census", class(result))
  result
}

get_mult_years <- function(years =2024,
                           num_vars = c("AGEP", "PWGTP"),
                           cat_vars = "SEX",
                           geography = "state",
                           geo_value = NULL,
                           api_key = "0065bb939f5c09eb016d3f55ea5f5522c49679e4") {
  combined <- NULL
  for (i in years) {
    one_year <- get_census_data(year = i,
                    num_vars = num_vars,
                    cat_vars = cat_vars,
                    geography = geography,
                    geo_value = geo_value,
                    api_key = "0065bb939f5c09eb016d3f55ea5f5522c49679e4")
    one_year <- mutate(one_year, year = i)
    combined <- bind_rows(combined, one_year)
  }
  class(combined) <- c("census", setdiff(class(combined), "census"))
  combined
}

summary.census <- function(census_table, num_vars = NULL, cat_vars = NULL) {
  if (is.null(num_vars)) {
    num_vars <- c("AGEP", "GASP", "GRPIP", "JWNP")
  }
  if (is.null(cat_vars)) {
    all_cat <- c("SEX", "FER", "HHL", "SCH", "SCHL", "year")
  }
  
  num_summary <- list()
  weight <- census_table$PWGTP
  for (i  in num_vars) {
    num_col <- census_table[[i]]
    sample_mean <- sum(num_col * weight) / sum(weight)
    sample_sd <- sqrt(sum(num_col^2 * weight) / sum(weight) - sample_mean^2) 
    num_summary[[i]] <- list(mean = sample_mean, sd = sample_sd)
  }
  
  cat_summary<- list()
  for (i in cat_vars) {
    cat_summary[[i]] <- table(census_table[[i]])
  }
  list(numeric = num_summary, categorical = cat_summary)
}

library(ggplot2)
plot.census <- function(census_data, cat_var, num_var) {
  if(!length(cat_var) == 1 || !length(num_var) == 1) {
    stop("Must include at least 1 numerical and categorical variable")
  }
  allowed_cat_vars <- c("SEX", "FER", "HHL", "SCH", "SCHL", "year")
  allowed_num_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  if (!all(cat_var %in% allowed_cat_vars)) {
    stop("Categorical variable must be in the allowed list")
  }
  if (!all(num_var %in% allowed_num_vars)) {
    stop("Numerical variable must be in the allowed list")
  }
  plot_data <- census_data 
  plot_data[[cat_var]] <- as.factor(plot_data[[cat_var]])
  ggplot(census_data, aes(x= get(cat_var), y= get(num_var), weight = PWGTP)) +
    geom_boxplot() +
    labs(x = cat_var, y= num_var,
         title = paste(num_var, "by", cat_var))
}
