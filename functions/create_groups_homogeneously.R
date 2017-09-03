library(psych) # for skew and kurtosi

create_groups_homogeneously <- function(data,
                                        number_of_groups,
                                        p_value_minimum,
                                        sd_maximum,
                                        seed,
                                        max_iterations) {
  # teilt sample zufällig in gruppen. bestimmt p-wert der anova, macht
  # deskriptive analyse (gibt SD aus). stoppt, wenn p-wert der anova über
  # schwellenwert liegt und SD der gruppen nicht über sd_maximum liegt
  #
  # Args:
  #   data: datensatz
  #   number_of_groups: anzahl zu bildende gruppen
  #   p_value_minimum: p-wert des gruppenfaktors, der die anova mindestens haben
  #      muss
  #   sd_maximum: maximum der SD, die die gruppen auf den variablen haben müssen
  #   seed: für die reproduktion der ergebnisse der sample()-funktion
  #   max_iterations: maximale anzahl an durchzuführenden iterationen
  #
  # Returns:
  #   eine liste mit dem .Random.seed, den coefficients der anova und den
  #      deskriptiven statistiken (für jede iteration einzeln)

  # loop vorbereiten:
  N <- nrow(data)                         # stichprobengrösse
  iterations <- seq_len(max_iterations)   # vector der iterationen
  set.seed(seed)                          # bestimmen des seeds
  my_output_list <- NULL                  # erstellen des objektes, die mit dem
                                          # loop aufgefüllt wird

  for (i in seq_along(iterations)) {            # für jede iteration i mach:

    i_list <- list(random_seed = .Random.seed)  # eine liste i mit .Random.seed

    # sample zufällig in number_of_groups und speichere datensatz als i_dat.
    # i_dat wird also jede iteration neu bestimmt
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

    # die vier if bedingungen:
    # 1. setzt p_value_test auf TRUE oder FALSE. je nach dem, ob der p value
    #    bei der iteration i über dem schwellwert liegt oder nicht
    if (lapply(my_output_list, "[[", "coefficients") %>%
        .[[i]] %>%
        select(., "Pr(>F)") %>%
        max(., na.rm = TRUE) > p_value_minimum) {
      p_value_test <- TRUE
    } else {
      p_value_test <- FALSE
    }

    # 2. setzt sd_test auf TRUE oder FALSE. je nach dem, ob die SD bei der
    #    iteration i unter dem schwellenwert liegt oder nicht
    if (lapply(my_output_list, "[[", "descriptives") %>%
        lapply(.,              "[[", "sd") %>%
        .[[i]] %>%
        select(., -contains("group")) %>%
        max() < sd_maximum) {
      sd_test <- TRUE
    } else {
      sd_test <- FALSE
    }

    # 3. evaluation der beiden objekte p_value_test und sd_test:
    #    wenn beide TRUE -> break()
    #    wenn eines der beiden TRUE -> message() darüber und loop weiterführen
    #    wenn beides FALSE -> nur message() mit iteration number
    if (p_value_test == TRUE & sd_test == TRUE) {
      message("iteration ", i, "... maximum SD < ", sd_maximum, " and p value > ", p_value_minimum, " identified :)")
      return(my_output_list)
      break()
    } else if (p_value_test == TRUE & sd_test == FALSE) {
      message("iteration ", i, "... p value > ", p_value_minimum, " BUT maximum SD > ", sd_maximum, " ...")
    } else if (p_value_test == FALSE & sd_test == TRUE) {
      message("iteration ", i, "... maximum SD < ", sd_maximum, " BUT p value < ", p_value_minimum, " ...")
    } else {
      message("iteration ", i, "...")
    }

    # 4. wenn das maximum der bestimmten iterationen erreicht und keine lösung
    #    gefunden wurde -> message() und return() (das objekt mit den results)
    if (i == max_iterations) {
      message("... maximum of iterations (", max_iterations,  ") reached and no soultion found")
      return(my_output_list)
    }
  }
}


