# ---- Tidy Tuesday ----
# -> Week 19
# -> "Water Sources"
#-----------------------

# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)

data <- tidytuesdayR::tt_load(2021, week = 19)

water <- data$water

mode_func <- function(x) levels(fct_infreq(x))[1]


# Data prep ---------------------------------------------------------------

# Load Africa base maps
  # Countries
afr_cntrs <- 
  ne_countries(continent = "Africa", scale = "large", returnclass = "sf") %>% 
  mutate(pop_est = as.integer(pop_est))

  # States
afr_sts <- 
  ne_states(iso_a2 = unique(afr_cntrs$iso_a2), returnclass = "sf")

# Edit water source data
water_afr <- water %>% 
  # Convert to sf object
  st_as_sf(coords = c("lon_deg", "lat_deg"), remove = FALSE) %>%
  # Set to same CRS
  st_set_crs(st_crs(afr_cntrs)) %>% 
  # Reduce to African countries
  filter(lengths(st_intersects(x = ., y = afr_cntrs)) > 0) %>% 
  # Join population estimates
  st_join(., y = afr_cntrs["pop_est"]) %>% 
  # Count water sources per country and capita
  group_by(country_name) %>% 
  mutate(water_pcntry = n(),
         water_pcapita = water_pcntry/pop_est) %>% 
  ungroup() %>% 
  # Add state codes
  st_join(., y = afr_sts["adm1_code"]) %>% 
  # Calculate indicators by state
  group_by(adm1_code) %>% 
  mutate(water_pstate = n(),
         water_source_prim = mode_func(water_source),
         water_tech_prim = mode_func(water_tech),
         water_avail = mean(status_id == "y")) %>% 
  ungroup()

# Add water source data to country shapes
cntrs_water <- 
  st_join(afr_cntrs, water_afr[c("water_pcntry", "water_pcapita")]) %>% 
  # Filter out duplicates produced by st_join
  filter(str_detect(row.names(.), "\\.[0-9]", negate = T))

# Add water source data to state shapes
sts_water <- 
  st_join(afr_sts, water_afr[c("water_pstate", "water_source_prim", 
                               "water_tech_prim", "water_avail")]) %>% 
  # Filter out duplicates produced by st_join
  filter(str_detect(row.names(.), "\\.[0-9]", negate = T))


# Plotting ----------------------------------------------------------------

ggplot() +
  geom_sf(data = cntrs_water, aes(fill = water_pcntry))
  
ggplot() +
  geom_sf(data = sts_water, aes(fill = water_pstate))

ggplot() +
  geom_sf(data = sts_water, aes(fill = water_source_prim))

