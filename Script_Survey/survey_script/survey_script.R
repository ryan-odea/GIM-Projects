require(tidyverse)
require(magrittr)
require(data.table)
require(lubridate)
require(wesanderson)

## User Inputs ----------------------------------------------------------------
year_to_observe <- 2021
single_month_observation <- "March"

## Data Loading and Manipulation ----------------------------------------------
responses <- fread("survey_responses.csv")
responses$Timestamp %<>% mdy_hms()
responses$year <- year(responses$Timestamp)
responses$month <- month(responses$Timestamp)

responses[, c("Clinic",
              "pronouns",
              "depends",
              "urg")] %<>% lapply(., factor)
## Plotting -------------------------------------------------------------------
### Separations----------------------------------------------------------------
# By clinic--------------------------------------------------------------------

responses %>%
subset(.$year == year_to_observe) %>% 
  ggplot(aes(x = month.name[month], y = as.factor(calls_msgs), col = Clinic)) + 
  geom_boxplot() + 
  labs(x = "Month", y = "Very Disatisfied")
