library(ggplot2)

blue <- "#003087"
teal <- "#147D80"
gold <- "#E3A22D"
course_codes <- c(Biology = "B", Chemistry = "C", Physics = "P")
short_sequence <- function(x) {
  vapply(strsplit(x, " → ", fixed = TRUE), function(parts) {
    paste(course_codes[parts], collapse = " → ")
  }, character(1))
}

figure_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = "#172436"),
      axis.text = element_text(colour = "#29384B"),
      legend.position = "bottom",
      plot.margin = margin(9, 14, 9, 9)
    )
}

plot_directions <- function(course_pairs) {
  cells <- course_pairs |>
    mutate(source = factor(source, levels = rev(courses)), target = factor(target, levels = courses),
           shown = if_else(source == target, NA_real_, weight),
           text = if_else(source == target, "—", paste0(eligible, " / ", weight)))
  ggplot(cells, aes(target, source)) +
    geom_tile(aes(fill = shown), colour = "white", linewidth = 1.5) +
    geom_text(aes(label = text, colour = shown >= 10), fontface = "bold", size = 5,
              show.legend = FALSE) +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "#172436"), na.value = "#586476") +
    scale_fill_gradient(low = "#EDF5FA", high = blue, na.value = "#F0F2F5",
                        name = "Total weight") +
    labs(x = "Dependent course", y = "Prerequisite course") +
    figure_theme() +
    theme(panel.grid = element_blank())
}

plot_scores <- function(scores) {
  order <- rev(scores$sequence)
  ggplot(scores, aes(weighted, factor(sequence, levels = order))) +
    geom_col(aes(fill = rank == 1), width = .66, na.rm = TRUE, show.legend = FALSE) +
    geom_text(aes(label = scales::percent(weighted, accuracy = 1)),
              hjust = -.22, size = 3.7, colour = "#172436", na.rm = TRUE) +
    scale_fill_manual(values = c("TRUE" = blue, "FALSE" = "#8198AE")) +
    scale_x_continuous(labels = scales::label_percent(), breaks = seq(0, 1, .25),
                       limits = c(0, 1.10), expand = expansion(mult = 0)) +
    labs(x = "Weighted satisfaction", y = NULL) +
    figure_theme() +
    theme(panel.grid.major.y = element_blank())
}

plot_missed <- function(missed, scores) {
  display <- missed |>
    mutate(sequence = factor(short_sequence(sequence), levels = short_sequence(scores$sequence)),
           target = factor(target, levels = courses),
           dependency_type = factor(dependency_type, levels = c("enriching", "helpful", "required")))
  ggplot(display, aes(sequence, missed, fill = dependency_type)) +
    geom_col(width = .66) +
    facet_grid(rows = vars(target), switch = "y") +
    scale_fill_manual(values = c(enriching = gold, helpful = teal, required = blue),
                      labels = c("Enriching", "Helpful", "Required")) +
    scale_y_continuous(breaks = scales::breaks_pretty(4), expand = expansion(mult = c(0, .08))) +
    labs(x = "Course sequence", y = "Unsatisfied dependencies", fill = NULL) +
    figure_theme() +
    theme(panel.grid.major.x = element_blank(), strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold"))
}

plot_sensitivity <- function(sensitivity, scores) {
  display <- sensitivity |>
    mutate(sequence = factor(short_sequence(sequence), levels = short_sequence(scores$sequence)),
           scenario = factor(scenario, levels = rev(unique(scenario))),
           text = if_else(is.na(weighted), "—", scales::percent(weighted, accuracy = 1)))
  ggplot(display, aes(sequence, scenario)) +
    geom_tile(aes(fill = weighted), colour = "white", linewidth = 1) +
    geom_tile(data = filter(display, rank == 1), fill = NA, colour = "#172436", linewidth = .55) +
    geom_text(aes(label = text, colour = weighted >= .64), fontface = "bold",
              size = 3.45, show.legend = FALSE) +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "#172436"), na.value = "#586476") +
    scale_fill_gradient(low = "#EDF5FA", high = blue, limits = c(0, 1),
                        labels = scales::label_percent(), name = "Satisfaction", na.value = "#F0F2F5",
                        guide = guide_colourbar(barwidth = grid::unit(1.85, "in"))) +
    labs(x = "Course sequence", y = NULL) +
    figure_theme() +
    theme(panel.grid = element_blank())
}
