# HW 3 SETUP
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(stringr)

# Full Data sets
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

# Problem 1: Part A
# Reproduce the graph in the HW. Use theme_bw() and readable legends.
p_a <- ggplot(vehicles, aes(displ, hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ drv) +
  labs(title = "Reference A", x = "Engine  displacement (litres)", 
       y = "Highway fuel economy (mpg)")
# Explanation: Explain why there are three fitted lines even when drv is mapped to color only in the point layer.
#    Mapping a discrete value creates groups automatically.
#    The color is inherited by both layers.

# Problem 1: Part B
p_b <- ggplot(vehicles, aes(x = drv, fill = factor(cyl)))
pb_stack <- p_b + geom_bar(position = "stack")
pb_fill <- p_b + geom_bar(position = "fill") +
  ylab("Proportions within drive type")

p_b2 <- ggplot(vehicles, aes(y = drv, fill = factor(cyl)))
pb2_stack <- p_b2 + geom_bar(position = "stack")
pb2_fill <- p_b2 + geom_bar(position = "fill") +
  xlab("Proportions within drive type")
# Equal total heights does not mean equal sample sizes because it is showing proportions to 1.

# Problem 1: Part C
# Create two histograms from carat. p_fine and p_coarse.
gems_smaller <- gems |> sample_n(5000)
p_fine <- ggplot(gems_smaller, aes(x = carat)) + 
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") + 
  labs(title = "Carat Binwidth = 0.01") +
  coord_cartesian()
p_coarse <- ggplot(gems_smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  labs(title = "Carat Binwidth = 0.5")
retained <- sum(!is.na(gems_smaller$carat))
excluded <- sum(is.na(gems_smaller$carat))

gems %>% filter(carat %in% c(0.99, 1.00)) |> 
  count(carat)
# In narrower bins, it is easier to see small clusters or peaks.
# You can see more details and fine fluctuations.
# For example, the specific carat sizes with the most occurrences.

# SET UP FOR Problem 2 & 3
libraries <- st_read("Libraries.csv")
areas <- st_read("Boundaries.geojson")

# Problem 2
before_clean <- nrow(libraries)
sum(is.na(libraries$latitude) | is.na(libraries$longitude))
sum(duplicated(libraries$name))
libraries_clean <- libraries |> mutate(
    LOCATION = str_trim(LOCATION),
    latitude = as.numeric(str_extract(LOCATION, "(?<=\\()[-0-9.]+")),
    longitude = as.numeric(str_extract(LOCATION, "(?<=, )[-0-9.]+(?=\\))"))
  )
libraries_clean$site_id <- seq_len(nrow(libraries_clean))

sites <- st_as_sf(
  libraries_clean,
  coords = c("longitude", "latitude"),
  crs = 4326
)

sites_m <- st_transform(sites, 26916)
after_clean <- nrow(libraries_clean)
sum(duplicated(st_coordinates(sites)))

# Map
buffer <- st_buffer(sites_m, dist = 500)
buffer_union <- st_union(buffer)

p_sites <- ggplot() + geom_sf(data = sites_m, aes(color = "Library")) +
  labs(
    title = "Chicago Public Library Locations",
    color = "Layer",
    caption = "Source: City of Chicago Open Data Portal") + 
      theme_minimal()

p_buffers <- ggplot() + geom_sf(data = buffer_union,
          aes(fill = "500 m Buffer"), alpha = 0.4, color = NA) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.5) +
  labs(
    title = "Chicago Public Libraries + Buffer",
    fill = "Layer",
    color = "Layer",
    caption = "Source: City of Chicago Open Data Portal") +
  theme_minimal()
