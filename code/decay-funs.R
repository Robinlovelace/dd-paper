# remotes::install_cran("minpack.lm")
library(tidyverse)

# l = pct::get_pct(geography = "msoa", layer = "l", national = TRUE)
# rgx = "Hereford|Leeds|Cambridge$"
# l_cities = l %>%
#   filter(str_detect(lad_name1, rgx) & str_detect(lad_name2, rgx))
# regions = c("cambridgeshire", "avon", "west-yorkshire")
regions = c("cambridgeshire", "humberside", "west-yorkshire")
modes_of_interest = c("foot", "bicycle", "car_driver")
modes_of_interest = c("foot", "bicycle")

# l = purrr::map_dfr(regions, ~pct::get_pct_lines(., geography = "msoa"), .id = "id")
# sf::st_crs(l) = 4326
# saveRDS(l, "l_regions-2020-10.Rds")
l = readRDS("l_regions-2020-10.Rds")
names(l)
# l_cities = l_cities %>% filter(all >= 20) %>%
l_cities = l %>% filter(all >= 50) %>%
  select(region = id, all:taxi_other, distance_euclidean = e_dist_km, distance = rf_dist_km, gradient = rf_avslope_perc, dutch_slc) %>%
  mutate(pcycle = bicycle / all, pwalk = foot / all, pdrive = car_driver / all)
unique(l_cities$region)
l_cities$region = as.factor(l_cities$region)
region_names_title = c("Cambridgeshire", "Avon", "West Yorkshire")
unique(l_cities$region)
# mapview::mapview(l_cities["pwalk"])
class(l_cities$region)
saveRDS(l_cities, "l_cities.Rds")
l_cities_long = l_cities %>%
  select(-matches("pc|pw|pd")) %>%
  sf::st_drop_geometry() %>%
  tidyr::pivot_longer(cols = bicycle:taxi_other, names_to = "mode") %>%
  mutate(proportion = value/all) %>%
  filter(mode %in% modes_of_interest)
class(l_cities_long$region) # char
l_cities_long$region = as.factor(l_cities_long$region)
summary(l_cities_long$region)
levels(l_cities_long$region) = region_names_title
# l_cities_long$Region = factor(as.numeric(l_cities_long$region), ordered = TRUE, levels = region_names_title)
# summary(l_cities_long$Region)

ggplot(l_cities_long) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  ylim(c(0, 1))

# linear models

by_city = l_cities_long %>%
  group_by(region, mode) %>%
  nest()

modlm = function(d) {
  stats::lm(proportion ~ distance, weights = all, data = d)
}

by_city_lm = by_city %>%
  mutate(model_lm = map(data, modlm), pred = map(model_lm, predict)) %>%
  unnest(cols = c(data, pred)) %>%
  select(region, mode, all, distance, distance_euclidean, gradient, value, proportion, pred) %>%
  ungroup()

names(by_city_lm)

g_lm = ggplot(by_city_lm) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

saveRDS(by_city_lm, "by_city_lm.Rds" )

s_2 = list(a1 = 0.1, s1 = 2, s2 = 3)
s_3 = list(a1 = 0.5, s1 = 3, s2 = 1)
modbeta = function(d) {
  minpack.lm::nlsLM(proportion ~ a1*dbeta(distance/max(distance), s1, s2), start = s_2, data = d, weights = all)
}

by_city_beta = by_city %>%
  mutate(mo = map(data, modbeta), pred = map(mo, predict)) %>%
  unnest(cols = c(data, pred)) %>%
  select(region, mode, all, distance, distance_euclidean, gradient, value, proportion, pred) %>%
  ungroup()

ggplot(by_city_beta) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

saveRDS(by_city_beta, "by_city_beta.Rds")
