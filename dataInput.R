library(tidyverse)

temp <- read_csv("data/unfolded_covid _w social_distance _w demographics _w weekly_patterns.csv")

saveRDS(temp, "data/unfoldedGA.RData")

uf <- temp %>% 
  # Convert variable to factor, so we can use complete later. We do this within
  # group_by, because we want to operate by level of variable
  group_by(census_block_group) %>% 
  # Remove all rows of variable if there aren't any rows with values==0
  filter(any(cases==0)) %>% 
  # Remove all rows with values != 0
  filter(cases != 0) %>% 
  # Keep the first row of each variable, after sorting by Date
  # This gives us the first non-zero row
  arrange(census_block_group,datestr) %>% 
  mutate(startdate = first(datestr)) %>% 
  # Use complete to bring back a row for any level of variable that
  # didn't start with any rows with values==0
  mutate(dayssince = as.double(difftime(datestr,startdate,unit="days")), newcases = cases - lag(cases))  

g <- ggplot(uf, aes(x = dayssince, y = log10(cases), color=census_block_group)) + geom_path() + geom_smooth()
g


l <- lm(log10(cases) ~ dayssince, uf)
l
