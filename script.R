library(tongfen)

library(dplyr)
library(ggplot2)

variables=c(population="H011001",households="H013001")

meta <- c(2000,2010) %>%
  lapply(function(year){
    v <- variables %>% setNames(paste0(names(.),"_",year))
    meta_for_additive_variables(paste0("dec",year),v)
  }) %>%
  bind_rows()

meta

census_data <- get_tongfen_us_census(regions = list(state="PR"), 
                                     meta=meta, 
                                     level="tract") %>%
  mutate(change = (population_2010 - population_2000)/population_2000) #%>%
  #mutate(change = if_else(change == -Inf, 0, change))


census_data %>% names()

library(purrr)

perc_func <- function(num) {
  return (quantile(census_data$change, num, na.rm = T))
}

library(sf)
library(viridis)

census_data <- st_as_sf(census_data)

census_data %>%
  mutate(c=cut(change, map_dbl(0:9/10, perc_func))) %>%
  ggplot() +
  geom_sf(aes(fill = c), alpha = 0.75) +
  scale_fill_viridis_d() +
  labs(title="Puerto Rico change in average household size between 2000-2010", 
       fill= 'Percentage Change'
       ) +
  coord_sf(datum=NA,
           xlim=c(-67.55412,-65.22111),
           ylim=c(17.88481,18.51576)) +
  theme_minimal()