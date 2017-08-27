# load library and data ---------------------------------------------------
library(tidyverse)
dat <- as_tibble(swiss) # hab ich als beispiel datensatz benützt
dat <- rownames_to_column(dat) %>% 
  rename(subject = rowname)

# function call -----------------------------------------------------------
results <- create_groups_homogeneously(data = dat, 
                                       number_of_groups = 4, 
                                       p_value_minimum = .90, 
                                       seed = 20170826,
                                       iterations = 200)

# recreate result ---------------------------------------------------------
# um die resultate zu replizieren wird hier der seed der iteration gesetzt,
# welche die bedingung(en) erfüllt. im beispiel hier ist das iteration 6
.Random.seed <- results$iteration_6$random_seed

# hier manuell das zeugs einfügen, was man haben möchte... 
dat %>% 
  mutate(group = sample(rep_len(x = seq_len(4), length.out = nrow(.)))) %>% 
  lm(Infant.Mortality ~ group, data = .) %>% 
  summary()
