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


# Linear models -----------------------------------------------------------

by_city = l_cities_long %>%
  group_by(region, mode) %>%
  nest()

modlm = function(d) {
  stats::lm(proportion ~ distance, weights = all, data = d)
}

by_city_lm_nest = by_city %>%
  mutate(model_lm = map(data, modlm), pred = map(model_lm, predict))

# check model contents
by_city_lm_nest[1, ]$model_lm

names(summary(by_city_lm_nest$model_lm[[6]]))
summary(by_city_lm_nest$model_lm[[6]])$r.squared #can't do this as vector of 6?


for(i in 1:nrow(by_city_lm_nest)){
  by_city_lm_nest$rsq[i] = summary(by_city_lm_nest$model_lm[[i]])$r.square
  by_city_lm_nest$adjrsq[i] = summary(by_city_lm_nest$model_lm[[i]])$adj.r.square
  by_city_lm_nest$aic[i] = AIC(by_city_lm_nest$model_lm[[i]])
}

by_city_lm_fit = by_city_lm_nest %>%
  select(region, mode, rsq, adjrsq, aic) %>%
  arrange(mode)
saveRDS(by_city_lm_fit, "by_city_lm_fit.Rds")

# by_city_lm_nest[1, ]$pred
# by_city_lm_nest[1, ]$data[[1]]$proportion
# by_city_lm_nest[1, ]$cor


# by_city_lm_nest = by_city %>%
#   mutate(
#     model_lm = map(data, modlm),
#     pred = map(model_lm, predict),
#     cor = map(
#       model_lm,
#       function(x) summary(x$r.squared)
#     )
#   )

# plot
by_city_lm = by_city_lm_nest %>%
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

g_lm
saveRDS(by_city_lm, "by_city_lm.Rds" )


# Exponential -------------------------------------------------------------
s_2 = list(a1 = 0.1)

modexp = function(d) {
  minpack.lm::nlsLM(
    proportion ~ exp(distance)^a1,
    start = s_2,
    data = d,
    weights = all
  )
}

by_city_exp_nest = by_city %>%
  mutate(model_exp = map(data, modexp), pred = map(model_exp, predict))

names(summary(by_city_exp_nest$model_exp[[6]]))
#which fit?


by_city_exp = by_city_exp_nest %>%
  unnest(cols = c(data, pred)) %>%
  select(region, mode, all, distance, distance_euclidean, gradient, value, proportion, pred) %>%
  ungroup()

names(by_city_exp)

g_exp = ggplot(by_city_exp) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

g_exp
saveRDS(by_city_exp, "by_city_exp.Rds" )

# Beta function -----------------------------------------------------------

s_2 = list(a1 = 0.1, s1 = 2, s2 = 3)
s_3 = list(a1 = 0.5, s1 = 3, s2 = 1)
modbeta = function(d) {
  minpack.lm::nlsLM(
    proportion ~ a1 * dbeta(distance / max(distance), s1, s2),
    start = s_2,
    data = d,
    weights = all
  )
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

# Power function (Gravity) -------------------------------------------------------------
s_2 = list(a1 = 0.1)

modpwr = function(d) {
  minpack.lm::nlsLM(
    proportion ~ distance^a1,
    start = s_2,
    data = d,
    weights = all
  )
}

by_city_pwr = by_city %>%
  mutate(model_pwr = map(data, modpwr), pred = map(model_pwr, predict)) %>%
  unnest(cols = c(data, pred)) %>%
  select(region, mode, all, distance, distance_euclidean, gradient, value, proportion, pred) %>%
  ungroup()

names(by_city_pwr)

g_pwr = ggplot(by_city_pwr) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

g_pwr
saveRDS(by_city_pwr, "by_city_pwr.Rds" )
