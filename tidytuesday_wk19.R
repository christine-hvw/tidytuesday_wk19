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
  st_crop(xmin = -25, ymin = -35, xmax = 51, ymax = 37)

  # States
afr_sts <- 
  ne_states(iso_a2 = unique(afr_cntrs$iso_a2), returnclass = "sf") %>% 
  st_crop(xmin = -25, ymin = -35, xmax = 51, ymax = 37)

# Edit water source data
water_afr <- water %>% 
  # Convert to sf object
  st_as_sf(coords = c("lon_deg", "lat_deg"), remove = FALSE) %>%
  # Set to same CRS
  st_set_crs(st_crs(afr_cntrs)) %>% 
  # Reduce to African countries
  filter(lengths(st_intersects(x = ., y = afr_cntrs)) > 0) %>% 
  # Add state codes
  st_join(., y = afr_sts["adm1_code"]) %>% 
  mutate(
    water_tech_grp = ifelse(str_detect(water_tech, "Hand Pump"), 
                            "Hand Pump", 
                            water_tech)
    ) %>% 
  # Calculate indicators by state
  group_by(adm1_code) %>% 
  mutate(
    water_pstate = n(),
    water_source_prim = mode_func(water_source),
    water_tech_prim = mode_func(water_tech),
    water_tech_grp_prim = mode_func(water_tech_grp),
    water_avail = mean(status_id == "y")
    ) %>% 
  ungroup()
  

# Add water source data to state shapes
sts_water <- 
  st_join(afr_sts, water_afr[c("water_pstate", "water_source_prim", 
                               "water_tech_prim", "water_tech_grp",
                               "water_tech_grp_prim", "water_avail")]) %>% 
  # Filter out duplicates produced by st_join
  filter(str_detect(row.names(.), "\\.", negate = T))

# Plotting ----------------------------------------------------------------

extrafont::loadfonts(device = "win")

ghibli_pal <- ghibli::ghibli_palette("PonyoMedium", type = "discrete")[c(2:3, 5:7)]

ggplot() +
  geom_sf(data = sts_water, 
          aes(fill = water_tech_grp_prim),
          color = "white", size = .3) +
  geom_sf(data = afr_cntrs, 
          fill = "transparent", color = "black", size = .3) +
  coord_sf(datum = NA) +
  scale_fill_manual(values = ghibli_pal, na.value = "lightgray") +
  labs(fill = "Primary system by state", 
       title = "Water Transportation Systems",
       subtitle = "How does water get from its source to the point of collection?",
       caption = "Data: Water Point Data Exchange") +
  theme(panel.background = element_rect(fill = "transparent"),
        legend.position = c(0.16, 0.19), 
        text = element_text(family = "CMU Sans Serif"),
        plot.title = element_text(size = 24))

ggsave("plot.pdf", device = cairo_pdf)
ggsave("plot.png", device = "png", dpi = 1000)
