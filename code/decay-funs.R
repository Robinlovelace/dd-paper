remotes::install_cran("minpack.lm")
library(tidyverse)

# l = pct::get_pct(geography = "msoa", layer = "l", national = TRUE)
# rgx = "Hereford|Leeds|Cambridge$"
# l_cities = l %>%
#   filter(str_detect(lad_name1, rgx) & str_detect(lad_name2, rgx))
# regions = c("cambridgeshire", "avon", "west-yorkshire")
regions = c("cambridgeshire", "humberside", "west-yorkshire")
modes_of_interest = c("foot", "bicycle", "car_driver")
modes_of_interest = c("foot", "bicycle")

l = purrr::map_dfr(regions, ~pct::get_pct_lines(., geography = "msoa"), .id = "id")
sf::st_crs(l) = 4326
saveRDS(l, "l_regions-2020-10.Rds")
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
  tidyr::pivot_longer(cols = bicycle:taxi_other, names_to = "Mode") %>%
  mutate(proportion = value/all) %>%
  filter(Mode %in% modes_of_interest)
class(l_cities_long$region) # char
l_cities_long$region = as.factor(l_cities_long$region)
summary(l_cities_long$region)
levels(l_cities_long$region) = region_names_title
# l_cities_long$Region = factor(as.numeric(l_cities_long$region), ordered = TRUE, levels = region_names_title)
# summary(l_cities_long$Region)

ggplot(l_cities_long) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(region ~ Mode) +
  ylim(c(0, 1))

# linear models

by_city = l_cities %>%
  sf::st_drop_geometry() %>%
  group_by(region) %>%
  nest()



modlm = function(df) {
  # minpack.lm::nlsLM(pdrive ~ a1*dbeta(distance/max(distance), s1, s2), start = s_3, data = df, lower = c(0.1, 0.1, 0.1))
  stats::lm(pdrive ~ distance, weights = all)
}

by_city_slm = by_city %>%
  mutate(model_walk_lm = map(data, modlm), predw = map(modelw, predict),
         modelc = map(data, modc), predc = map(modelc, predict),
         modeld = map(data, modd), predd = map(modeld, predict)
         ) %>%


by_city = by_city %>%
  mutate(modelw = map(data, modw), predw = map(modelw, predict),
         modelc = map(data, modc), predc = map(modelc, predict),
         modeld = map(data, modd), predd = map(modeld, predict)
         )
# # saving as it doesn't work with knitr for some reason...
# saveRDS(by_city, "by_city-beta.Rds")
by_city = readRDS("by_city-beta.Rds")
l_beta = unnest(by_city, cols = c(data, predw, predc, predd))
# text_betaw = map_dfr(by_city$modelw, ~round(coef(.), digits = 3))
# text_betaw$a1 = paste("a: ", text_betaw$a1)
# text_betaw$s1 = paste("s1: ", text_betaw$s1)
# text_betaw$s2 = paste("s2: ", text_betaw$s2)
# text_betaw$region = region_names_title
# text_betac = map_dfr(by_city$modelc, ~round(coef(.), digits = 3))
# text_betac$a1 = paste("a: ", text_betac$a1)
# text_betac$s1 = paste("s1: ", text_betac$s1)
# text_betac$s2 = paste("s2: ", text_betac$s2)
# text_betac$region = region_names_title

# for printing model results

g1 = ggplot(l_beta) +
  geom_point(aes(distance, pwalk, size = all), alpha = 0.1) +
  geom_line(aes(distance, predw), col = "red", size = 2) +
  # geom_text(aes(x = 10, y = 0.8, label = a1), data = text_betaw) +
  # geom_text(aes(x = 10, y = 0.7, label = s1), data = text_betaw) +
  # geom_text(aes(x = 10, y = 0.6, label = s2), data = text_betaw) +
  facet_wrap(~region) +
  ylim(c(0, 1)) +
  xlim(c(0, 5))
g2 = ggplot(l_beta) +
  geom_point(aes(distance, pcycle, size = all), alpha = 0.1) +
  geom_line(aes(distance, predc), col = "red", size = 2) +
  # geom_text(aes(x = 10, y = 0.8, label = a1), data = text_betac) +
  # geom_text(aes(x = 10, y = 0.7, label = s1), data = text_betac) +
  # geom_text(aes(x = 10, y = 0.6, label = s2), data = text_betac) +
  facet_wrap(~region) +
  ylim(c(0, 1))
g3 = ggplot(l_beta) +
  geom_point(aes(distance, pdrive, size = all), alpha = 0.1) +
  geom_line(aes(distance, predd), col = "red", size = 2) +
  # geom_text(aes(x = 10, y = 0.8, label = a1), data = text_betac) +
  # geom_text(aes(x = 10, y = 0.7, label = s1), data = text_betac) +
  # geom_text(aes(x = 10, y = 0.6, label = s2), data = text_betac) +
  facet_wrap(~region) +
  ylim(c(0, 1))
g1 / g2 / g3