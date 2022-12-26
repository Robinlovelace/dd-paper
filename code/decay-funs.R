# remotes::install_cran("minpack.lm")
library(tidyverse)

l = pct::get_pct(geography = "msoa", layer = "l", national = TRUE)
rgx = "Hereford|Leeds|Cambridge$"
l_cities = l %>%
  filter(str_detect(lad_name1, rgx) & str_detect(lad_name2, rgx))
regions = c("cambridgeshire", "avon", "west-yorkshire")
regions = c("cambridgeshire", "humberside", "west-yorkshire")
modes_of_interest = c("foot", "bicycle", "car_driver")
modes_of_interest = c("foot", "bicycle")
saveRDS(l, "l_national.Rds")
saveRDS(l_cities, "l_cities_original.Rds")

# l = purrr::map_dfr(regions, ~pct::get_pct_lines(., geography = "msoa"), .id = "id")
# sf::st_crs(l) = 4326
# saveRDS(l, "l_regions-2020-10.Rds")
# l = readRDS("l_regions-2020-10.Rds")
# l = readRDS("l_cities.Rds")
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
l_cities = readRDS("l_cities.Rds")
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


# Global models -----------------------------------------------------------

m_lm = l_cities_long %>%
  lm(proportion ~ distance, weights = all, data = .)
x_grid = data.frame(distance = seq(from = 0.1, to = 20, by = 0.5))
x_grid$p_lm = predict(m_lm, newdata = x_grid)
ggplot() +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1, data = l_cities_long) +
  geom_line(aes(distance, p_lm), data = x_grid, col = "red", size = 3) +
  ylim(c(0, 1))

# brms
library(brms)
m_brm = l_cities_long %>%
  brm(proportion | weights(all) ~ distance, data = .)
p_brm = predict(m_brm, newdata = x_grid, probs = c(0.25, 0.75))
head(p_brm)
x_grid$p_brm.Estimate = p_brm[, "Estimate"]
x_grid$p_brm.Q25 = p_brm[, "Q25"]
x_grid$p_brm.Q75 = p_brm[, "Q75"]
head(x_grid)
ggplot() +
  geom_ribbon(aes(distance, ymin = p_brm.Q25, ymax = p_brm.Q75), data = x_grid, fill = "blue") +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1, data = l_cities_long) +
  geom_line(aes(distance, p_lm), data = x_grid, col = "red", size = 3) +
  ylim(c(-0.2, 1))

# Preparation for INLA:
l_cities_with_na = l_cities_long %>%
  slice(1:1000)
for(i in setdiff(x = names(l_cities_long), "geometry")) {
  l_cities_with_na[[i]] = NA
}
l_cities_with_na$distance = runif(n = nrow(l_cities_with_na), min = min(l_cities_long$distance), max = max(l_cities_long$distance))
lc_nas = rbind(l_cities_long, l_cities_with_na)

m_inla = inla(proportion ~ distance, weights = all, data = lc_nas)
predictions = m_inla$summary.fitted.values
predictions$distance = lc_nas$distance
predictions %>%
  ggplot(aes(distance, mean)) +
  geom_line()
predictions %>%
  ggplot(aes(distance, ymin = `0.025quant`, ymax = `0.975quant`)) +
  geom_ribbon()

ggplot() +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1, data = l_cities_long) +
  geom_line(aes(distance, p_lm), data = x_grid, col = "red", size = 3) +
  geom_ribbon(aes(x = distance, ymin = `0.025quant`, ymax = `0.975quant`), data = predictions)
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

g_lm = ggplot(by_city_lm) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

g_lm
saveRDS(by_city_lm, "by_city_lm.Rds")

# # brms version
# library(brms)
# system.time({
#   m1_brms = brm(proportion ~ distance, weights = all, data = l_cities_long)
# })


# INLA --------------------------------------------------------------------

modlm_inla = function(d) {
  inla(proportion ~ distance, weights = all, data = d)
}

by_city_lm_nest = by_city %>%
  mutate(model_lm = map(data, modlm_inla), pred = map(model_lm, predict))

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

g_lm = ggplot(by_city_lm) +
  geom_point(aes(distance, proportion, size = all), alpha = 0.1) +
  facet_grid(mode ~ region) +
  geom_line(aes(distance, pred), col = "red", size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))

g_lm
saveRDS(by_city_lm, "by_city_lm.Rds")


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
