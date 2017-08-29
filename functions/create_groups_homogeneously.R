library(psych) # for skew and kurtosi

create_groups_homogeneously <- function(data, 
                                        number_of_groups, 
                                        p_value_minimum,
                                        seed,
                                        iterations) {
  # teilt sample zufällig in gruppen. bestimmt p-wert der anova, macht
  # deskriptive analyse. stoppt, wenn p-wert der anova über schwellenwert liegt.
  # 
  # Args:
  #   data: datensatz
  #   number_of_groups: anzahl zu bildende gruppen
  #   p_value_minimum: p-wert des gruppenfaktors, der die anova mindestens haben
  #      muss
  #   seed: für die reproduktion der ergebnisse der sample()-funktion
  #   iterations: maximale anzahl an durchzuführenden iterationen
  #
  # Returns:
  #   eine liste mit dem .Random.seed, den coefficients der anova und den 
  #      deskriptiven statistiken (für jede iteration einzeln)
  N <- nrow(data)                         # stichprobengrösse
  iterations <- seq_len(iterations)       # vector der iterationen
  set.seed(seed)                          # bestimmen des seeds
  my_output_list <- NULL                  # erstellen des objektes, die mit dem
                                          # loop aufgefüllt wird
  
  for (i in seq_along(iterations)) {            # für jede iteration i mach:
    
    i_list <- list(random_seed = .Random.seed)  # eine liste i mit .Random.seed
    
    # dieser teil prüft, ob das maximum des p-werts der anova über dem
    # p_value_minimum liegt. wenn TRUE, dann gib die output_list zurück und
    # break() aus dem loop -> funktion bricht ab
    if (i > 1) {
      if (lapply(my_output_list, "[[", 2) %>% 
          lapply(., "[[", 7) %>% 
          lapply(., "[[", 1) %>% 
          unlist() %>% 
          max() > p_value_minimum) {
        message("p value > ", p_value_minimum, " identified :)")
        return(my_output_list)
        break()
      }
    }
    
    # damit man sieht, wie weit die funktion ist
    message("iteration ", i, "...")
    
    # sample zufällig in number_of_groups und speichere datensatz als i_dat
    i_dat <- data %>% 
      mutate(group = sample(rep_len(x = seq_len(number_of_groups), length.out = N)))

    # sag AV mit group vorher, mach summary und gib coefficients wieder. speichere
    # den data.frame in i_list unter dem namen coefficients
    i_list[["coefficients"]] <- i_dat %>% 
      manova(cbind(Infant.Mortality, Education) ~ group, data = .) %>% 
      summary.manova() %>% 
      .$stats %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    # berechne deskriptive angaben für die gruppen. speichere die ergebnisse
    # in i_list under dem nahmen descriptives.
    i_list[["descriptives"]][["mean"]]     <- get_grouped_descriptives(i_dat, group, mean)
    i_list[["descriptives"]][["sd"]]       <- get_grouped_descriptives(i_dat, group, sd)
    i_list[["descriptives"]][["min"]]      <- get_grouped_descriptives(i_dat, group, min)
    i_list[["descriptives"]][["max"]]      <- get_grouped_descriptives(i_dat, group, max)
    i_list[["descriptives"]][["skew"]]     <- get_grouped_descriptives(i_dat, group, skew)
    i_list[["descriptives"]][["kurtosis"]] <- get_grouped_descriptives(i_dat, group, kurtosi)
    
    # speichere i_list (alle ergebnisse der iteration i) in die übergeordnete
    # output list (welche die infos für alle iterationen beinhaltet)
    my_output_list[[paste("iteration", i, sep = "_")]] <- i_list
  }
}