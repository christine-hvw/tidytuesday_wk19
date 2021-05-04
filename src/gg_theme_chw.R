library(ggplot2)

# If necessary install font (https://www.1001fonts.com/cmu-font.html)
# and load with:
# extrafont::font_import(paths = "pathtofonts", pattern = "^cmu")

extrafont::loadfonts(device = "win")

# Customize Theme ---------------------------------------------------------

theme_chw <- function(base_size = 12) {
  
  theme_minimal() +
    
    theme(
      
      axis.title = element_text(face = "bold"),
        
      axis.title.x = element_text(vjust = -4),
      
      axis.title.y = element_text(
        vjust = 4,
        angle = 90),
      
      legend.background = element_blank(),
      
      legend.box.margin = margin(base_size/2, base_size/3, base_size/3, base_size/3),
      
      legend.justification = "center",
      
      legend.key = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(vjust = .7),
      
      panel.grid = element_line(
        colour = "#d2d5d8", 
        size = .1),
      
      panel.grid.minor = element_blank(),
      
      panel.spacing.x = unit(base_size/2, "mm"),
      
      panel.spacing.y = unit(base_size/1.5, "mm"),
      
      plot.caption = element_text(hjust = 0),
      
      plot.caption.position = "plot",
      
      plot.margin = margin(base_size/5, base_size/2, base_size/5, base_size/2, unit = "mm"),
      
      plot.title = element_text(
        face = "bold",
        size = rel(1.33),
        vjust = -1.2,
        hjust = 0,
        margin = margin(0, 0, base_size/3, 0, unit = "mm")
      ),
      
      plot.title.position = "plot",
      
      plot.subtitle = element_text(
        size = rel(1.1),
        hjust = 0,
        vjust = -1.5,
        margin = margin(0, 0, base_size/2, 0, unit = "mm")
        ),
      
      strip.text = element_text(size = rel(1.1)),
      
      strip.text.x = element_text(
        margin = margin(base_size/4, 0, base_size/4, 0, unit = "mm")),
      
      strip.text.y = element_text(
        margin = margin(0, base_size/4, 0, base_size/4, unit = "mm")),
      
      text = element_text(
        family = "CMU Sans Serif",
        size = base_size)
    )
  
}

theme_set(theme_chw())


# Customize Default Colors ------------------------------------------------

# Discrete palette from {ghibli} package: https://github.com/ewenme/ghibli
# ghibli::ghibli_palette("PonyoMedium", type = "discrete")[c(6,2,7,4,1)]

ponyo_disc <- c("#D8AF39FF", "#5A6F80FF", "#E8C4A2FF", "#E75B64FF", "#4C413FFF")


# Continuous palette
# colorRampPalette(ponyo_disc[c(3,2,5)])(100)

ponyo_cont <- 
  c("#E8C4A2", "#E5C2A1", "#E2C0A0", "#DFBE9F", "#DCBD9F", "#D9BB9E", "#D6B99D",
    "#D3B79D", "#D1B69C", "#CEB49B", "#CBB29B", "#C8B19A", "#C5AF99", "#C2AD99",
    "#BFAB98", "#BCAA97", "#BAA897", "#B7A696", "#B4A595", "#B1A394", "#AEA194",
    "#AB9F93", "#A89E92", "#A69C92", "#A39A91", "#A09990", "#9D9790", "#9A958F",
    "#97938E", "#94928E", "#91908D", "#8F8E8C", "#8C8D8C", "#898B8B", "#86898A",
    "#838789", "#808689", "#7D8488", "#7A8287", "#788187", "#757F86", "#727D85",
    "#6F7B85", "#6C7A84", "#697883", "#667683", "#647582", "#617381", "#5E7181",
    "#5B6F80", "#596E7F", "#596D7E", "#596C7C", "#596B7B", "#586A7A", "#586978",
    "#586877", "#576876", "#576774", "#576673", "#576572", "#566470", "#56636F",
    "#56626E", "#55616C", "#55606B", "#555F6A", "#555E69", "#545D67", "#545C66",
    "#545B65", "#535B63", "#535A62", "#535961", "#53585F", "#52575E", "#52565D",
    "#52555B", "#51545A", "#515359", "#515257", "#515156", "#505055", "#504F54",
    "#504E52", "#4F4E51", "#4F4D50", "#4F4C4E", "#4F4B4D", "#4E4A4C", "#4E494A",
    "#4E4849", "#4D4748", "#4D4646", "#4D4545", "#4D4444", "#4C4342", "#4C4241",
    "#4C4140", "#4C413F")


# Set default ggplot options

options(ggplot2.discrete.colour = ponyo_disc, 
        
        ggplot2.discrete.fill = ponyo_disc,
        
        ggplot2.continuous.colour = function(...){
          scale_color_gradientn(colors = ponyo_cont, ...)
          },
        
        ggplot2.continuous.fill = function(...){
          scale_fill_gradientn(colors = ponyo_cont, ...)
          }
        )


# Try out -----------------------------------------------------------------

# library(palmerpenguins)
# library(dplyr)
# library(purrr)
# 
# data("penguins")
# 
# base <-
#   penguins %>%
#   mutate(bill_length_grp = cut(bill_length_mm, breaks = c(30, 36, 42, 48, 54, 60),
#                                labels = c("xs", "s", "m", "l", "xl"))) %>%
#   filter(!is.na(sex)) %>%
# 
#   ggplot(aes(x = flipper_length_mm,
#              y = body_mass_g)) +
#   geom_point(aes(color = bill_length_grp,
#                  shape = species),
#              size = 3,
#              alpha = 0.8) +
#   facet_wrap(~ island, ncol = 2) +
#   labs(title = "Palmer Penguins", subtitle = "An Example Plot",
#        caption = "Made with ggplot2")
# 
# base_cont <-
#   penguins %>%
#   filter(!is.na(sex)) %>%
# 
#   ggplot(aes(x = flipper_length_mm,
#              y = body_mass_g)) +
#   geom_point(aes(shape = species, color = body_mass_g),
#              size = 3,
#              alpha = 0.8) +
#   facet_wrap(~ island, ncol = 2) +
#   #scale_color_gradientn() +
#   labs(title = "Palmer Penguins", subtitle = "An Example Plot",
#        caption = "Made with ggplot2")
# 
# 
# base + theme_chw()
# base_cont + theme_chw()
# 
# penguins %>%
#   mutate(bill_length_grp = cut(bill_length_mm, breaks = c(30, 36, 42, 48, 54, 60),
#                                labels = c("xs", "s", "m", "l", "xl"))) %>%
#   filter(!is.na(sex)) %>%
# 
#   ggplot(aes(x = bill_length_grp,
#              y = body_mass_g)) +
#   geom_boxplot(aes(fill = bill_length_grp)) +
#   #facet_wrap(~ island, ncol = 2) +
#   labs(title = "Palmer Penguins", subtitle = "An Example Plot",
#        caption = "Made with ggplot2") +
# theme_chw()
