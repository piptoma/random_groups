# load library and data ---------------------------------------------------
library(tidyverse)
dat <- as_tibble(swiss) # hab ich als beispiel datensatz benützt
dat <- rownames_to_column(dat) %>%
  rename(subject = rowname)

# scale all variables but subject (is chr)
dat <- dat %>%
  mutate_at(vars(everything(), -subject), scale)

# function call -----------------------------------------------------------
results <- create_groups_homogeneously(data = dat,
                                       number_of_groups = 4,
                                       p_value_minimum = .90,
                                       sd_maximum = 1.4,
                                       seed = 20170826,
                                       max_iterations = 200)

# recreate result ---------------------------------------------------------
# um die resultate zu replizieren wird hier der seed der iteration gesetzt,
# welche die bedingung(en) erfüllt. im beispiel oben mit
# p_value_minimum = .90
# sd_maximum = 1.4
# ist das iteration 10
results[["iteration_10"]][["coefficients"]]         # coefficients anschauen
results[["iteration_10"]][["descriptives"]][["sd"]] # sds anschauen

# seed reproduzieren
.Random.seed <- results$iteration_10$random_seed

# hier manuell das zeugs einfügen, was man mit dem seed reproduzieren möchte...
dat %>%
  mutate(group = sample(rep_len(x = seq_len(4), length.out = nrow(.)))) %>%
  lm(Infant.Mortality ~ group, data = .) %>%
  summary()
