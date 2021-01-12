remotes::install_cran("minpack.lm")
library(tidyverse)

region_names_title = c("Cambridgeshire", "Avon", "West Yorkshire")
l_cities = readRDS("l_cities.Rds")
g1 = ggplot(l_cities) +
  geom_point(aes(distance, pwalk, size = all), alpha = 0.1) +
  facet_wrap(~region) +
  ylim(c(0, 1))
g2 = ggplot(l_cities) +
  geom_point(aes(distance, pcycle, size = all), alpha = 0.1) +
  facet_wrap(~region) +
  ylim(c(0, 1))
g3 = ggplot(l_cities) +
  geom_point(aes(distance, pdrive, size = all), alpha = 0.1) +
  facet_wrap(~region) +
  ylim(c(0, 1))
library(patchwork)
g1 / g2 / g3

# linear models

by_city = l_cities %>%
  sf::st_drop_geometry() %>%
  group_by(region) %>%
  nest()

by_city_mode = l_cities %>%
  sf::st_drop_geometry() %>%
  tidyr::pivot_longer(cols = all:taxi_other, names_to = "mode") %>%
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