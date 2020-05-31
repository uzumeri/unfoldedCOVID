library(tidyverse)
library(ggforce)

setwd("~/github/unfoldedCOVID")

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

sf <- uf %>% group_by(county_name, datestr) %>% summarize(covid = sum(cases), mort = sum(deaths) )

cbgcount <- uf %>% filter(datestr == last(datestr)) %>% group_by(county_name) %>% summarize(n())

pdf(file="multiplotuf.pdf")
for (i in 1:18) {
p <-  ggplot(uf, aes(x = dayssince, y = log10(cases), color=census_block_group)) + geom_path() + facet_wrap_paginate(~county_name, ncol=3, nrow=3, page=i)
plot(p)
}
dev.off()

l <- lm(log10(cases) ~ dayssince, uf)
l
