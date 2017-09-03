get_grouped_descriptives <- function(data, group, x) {
  # berechnet die gruppierten deskriptiven angaben
  #
  # Args:
  #   x: funktion (will man mean, sd, min, max etc.?)
  #
  # Returns:
  #   einen dataframe mit allen ausgewählten variablen
  data %>%
    group_by(group) %>%                      # gruppierungsvariable
    select(-subject, -Infant.Mortality) %>%  # Infant.Mortality ist die AV :)
    summarise_all(x)                         # berechne x auf allen variablen
}
