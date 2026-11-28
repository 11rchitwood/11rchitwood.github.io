library(dplyr)
library(tidyr)
library(purrr)
library(readr)

courses <- c("Biology", "Chemistry", "Physics")
study_weights <- c(required = 3, helpful = 2, enriching = 1, reinforcing = 0)
confidence_levels <- c("high", "medium", "low")
review_levels <- c("provisional", "accepted", "rejected")
sequence_key <- expand_grid(first = courses, second = courses, third = courses) |>
  filter(first != second, first != third, second != third) |>
  mutate(sequence = paste(first, second, third, sep = " → "))
sequence_levels <- sequence_key$sequence
course_order <- sequence_key |>
  pivot_longer(c(first, second, third), names_to = "position", values_to = "course") |>
  mutate(position = match(position, c("first", "second", "third")))

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

load_inputs <- function(data_dir = "data") {
  standards <- read_csv(file.path(data_dir, "standards.csv"),
                        col_types = cols(.default = col_character()), trim_ws = FALSE)
  dependencies <- read_csv(file.path(data_dir, "dependencies.csv"),
                           col_types = cols(.default = col_character(), weight = col_double()))
  stop_for_problems(standards)
  stop_for_problems(dependencies)
  validate_inputs(standards, dependencies)
  lookup <- standards |> select(substandard_id, standard_id, discipline)
  edges <- dependencies |>
    left_join(
      rename(lookup, prerequisite_id = substandard_id, source_standard = standard_id,
             source = discipline), by = "prerequisite_id", relationship = "many-to-one"
    ) |>
    left_join(
      rename(lookup, dependent_id = substandard_id, target_standard = standard_id,
             target = discipline), by = "dependent_id", relationship = "many-to-one"
    ) |>
    mutate(cross_course = source != target)
  list(standards = standards, edges = edges)
}

validate_inputs <- function(standards, dependencies) {
  fields <- c("discipline", "standard_id", "substandard_id", "standard_text",
              "source_url", "source_version")
  edge_fields <- c("prerequisite_id", "dependent_id", "dependency_type", "weight",
                   "rationale", "confidence", "review_status", "concept")
  check(all(fields %in% names(standards)), "The standards CSV is missing required columns.")
  check(all(edge_fields %in% names(dependencies)), "The dependency CSV is missing required columns.")
  complete <- function(x) all(vapply(x, function(y) all(!is.na(y) & nzchar(trimws(y))), logical(1)))
  check(nrow(standards) > 0 && complete(standards[fields]), "Standards contain missing values.")
  check(complete(dependencies[edge_fields]), "Dependencies contain missing values.")
  check(!anyDuplicated(standards$substandard_id), "Repeated objective ID in standards.")
  prefix <- c(Biology = "SB", Chemistry = "SC", Physics = "SP")
  check(setequal(standards$discipline, courses) &&
          all(grepl("^S[BCP][0-9]+\\.[a-z]$", standards$substandard_id)) &&
          all(sub("\\.[a-z]$", "", standards$substandard_id) == standards$standard_id) &&
          all(substr(standards$standard_id, 1, 2) == prefix[standards$discipline]),
        "An objective ID, its parent ID, or its discipline is invalid.")
  endpoints <- c(dependencies$prerequisite_id, dependencies$dependent_id)
  check(all(endpoints %in% standards$substandard_id), "Dependencies reference an unknown objective.")
  check(all(dependencies$prerequisite_id != dependencies$dependent_id), "Self-edge in dependencies.")
  check(!anyDuplicated(dependencies[c("prerequisite_id", "dependent_id")]),
        "Repeated directed pair in dependencies.")
  check(all(dependencies$dependency_type %in% names(study_weights)) &&
          all(dependencies$confidence %in% confidence_levels) &&
          all(dependencies$review_status %in% review_levels),
        "Invalid dependency category, confidence, or review status.")
  check(all(is.finite(dependencies$weight) &
              if_else(dependencies$dependency_type == "reinforcing", dependencies$weight == 0,
                      dependencies$weight > 0)),
        "Weights must be finite and positive, with zero for reinforcement.")
}

weight_summaries <- function(edges, sizes) {
  edges |>
    group_by(source, target) |>
    summarise(relationships = n(), eligible = sum(weight > 0), weight = sum(weight),
              .groups = "drop") |>
    complete(source = courses, target = courses, fill = list(relationships = 0, eligible = 0, weight = 0)) |>
    mutate(source_n = sizes[source], target_n = sizes[target],
           possible = source_n * (target_n - as.integer(source == target)),
           per_target = eligible / target_n,
           per_100_pairs = 100 * eligible / possible,
           weight_per_100_pairs = 100 * weight / possible)
}

network_summary <- function(edges, standards) {
  vertices <- standards |> transmute(name = substandard_id, discipline)
  graph <- igraph::graph_from_data_frame(
    select(edges, prerequisite_id, dependent_id, weight, dependency_type), vertices = vertices
  )
  membership <- igraph::components(graph, mode = "strong")$membership
  nodes <- vertices |>
    transmute(
      objective = name, discipline,
      prerequisites = as.integer(igraph::degree(graph, v = name, mode = "in")),
      supports = as.integer(igraph::degree(graph, v = name, mode = "out")),
      incoming_weight = igraph::strength(graph, vids = name, mode = "in"),
      outgoing_weight = igraph::strength(graph, vids = name, mode = "out"),
      component = unname(membership[name])
    )
  components <- nodes |>
    group_by(component) |>
    summarise(objectives = paste(sort(objective), collapse = ", "), n = n(),
              disciplines = n_distinct(discipline), .groups = "drop") |>
    filter(n > 1) |>
    select(-component) |>
    arrange(desc(n), objectives)
  reciprocals <- edges |>
    inner_join(select(edges, prerequisite_id, dependent_id, reverse_type = dependency_type),
               by = c("prerequisite_id" = "dependent_id", "dependent_id" = "prerequisite_id"),
               relationship = "one-to-one") |>
    filter(prerequisite_id < dependent_id) |>
    select(prerequisite_id, dependent_id, dependency_type, reverse_type, cross_course)
  list(nodes = nodes, components = components, reciprocals = reciprocals)
}

safe_ratio <- function(numerator, denominator) {
  if_else(denominator > 0, numerator / denominator, NA_real_)
}

ordering <- function(edges) {
  edges |>
    crossing(sequence = sequence_levels) |>
    left_join(rename(course_order, source = course, source_position = position),
              by = c("sequence", "source"), relationship = "many-to-one") |>
    left_join(rename(course_order, target = course, target_position = position),
              by = c("sequence", "target"), relationship = "many-to-one") |>
    mutate(satisfied = source_position < target_position)
}

score_sequences <- function(edges) {
  ordering(edges) |>
    group_by(sequence) |>
    summarise(
      eligible = n(), satisfied_edges = sum(satisfied),
      eligible_weight = sum(weight), satisfied_weight = sum(weight[satisfied]),
      required = sum(dependency_type == "required"),
      satisfied_required = sum(satisfied & dependency_type == "required"),
      .groups = "drop"
    ) |>
    right_join(select(sequence_key, sequence), by = "sequence") |>
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 0)),
      unweighted = safe_ratio(satisfied_edges, eligible),
      weighted = safe_ratio(satisfied_weight, eligible_weight),
      required_only = safe_ratio(satisfied_required, required),
      unsatisfied = eligible - satisfied_edges,
      unsatisfied_weight = eligible_weight - satisfied_weight,
      rank = min_rank(desc(round(weighted, 12)))
    ) |>
    arrange(rank, match(sequence, sequence_levels))
}

missed_by_course <- function(cross_edges) {
  ordering(cross_edges) |>
    filter(!satisfied) |>
    group_by(sequence, target, dependency_type) |>
    summarise(missed = n(), weight = sum(weight), .groups = "drop") |>
    complete(sequence = sequence_levels, target = courses,
             dependency_type = names(study_weights)[study_weights > 0],
             fill = list(missed = 0, weight = 0)) |>
    arrange(match(sequence, sequence_levels), match(target, courses), dependency_type)
}

collapse_edges <- function(edges, ...) {
  edges |>
    arrange(desc(weight), match(dependency_type, names(study_weights))) |>
    group_by(source, target, ...) |>
    slice_head(n = 1L) |>
    ungroup()
}

run_scenarios <- function(cross_edges, sizes) {
  incoming <- cross_edges |> group_by(target) |> mutate(weight = weight / sum(weight)) |> ungroup()
  alternatives <- c(required = 5, helpful = 2, enriching = 1)
  scenarios <- list(
    "Baseline" = cross_edges,
    "Equal edge weights" = mutate(cross_edges, weight = 1),
    "Emphasize required (5/2/1)" = mutate(
      cross_edges, weight = unname(alternatives[dependency_type])
    ),
    "High and medium confidence" = filter(cross_edges, confidence != "low"),
    "High confidence only" = filter(cross_edges, confidence == "high"),
    "Without enriching" = filter(cross_edges, dependency_type != "enriching"),
    "Required only" = filter(cross_edges, dependency_type == "required"),
    "Accepted only" = filter(cross_edges, review_status == "accepted"),
    "Equal weight per receiving course" = incoming,
    "Per objective pair" = mutate(cross_edges, weight = weight / (sizes[source] * sizes[target])),
    "Collapse to parent standards" = collapse_edges(cross_edges, source_standard, target_standard),
    "Collapse to concept per direction" = collapse_edges(cross_edges, concept)
  )
  imap(scenarios, ~ mutate(score_sequences(.x), scenario = .y)) |> list_rbind()
}

simulate_rankings <- function(cross_edges, iterations = 2000, seed = 2718) {
  # Hypothetical recoding: one step up or down with equal probability when perturbed.
  # A downgrade from enriching to zero removes the edge. Accepted edges stay fixed.
  # Adjacent categories use the illustrative weights for the simulated classification.
  set.seed(seed)
  if (!nrow(cross_edges)) {
    return(tibble(sequence = sequence_levels, outright_wins = NA_integer_,
                  joint_wins = NA_integer_, iterations = iterations, scored_draws = 0L))
  }
  direction <- vapply(seq_along(sequence_levels), function(i) {
    order <- unlist(sequence_key[i, c("first", "second", "third")], use.names = FALSE)
    match(cross_edges$source, order) < match(cross_edges$target, order)
  }, logical(nrow(cross_edges)))
  # vapply simplifies a one-edge case to a vector.
  direction <- matrix(direction, nrow = nrow(cross_edges))
  p <- c(high = .10, medium = .30, low = .60)[cross_edges$confidence]
  p[cross_edges$review_status == "accepted"] <- 0
  values <- unname(study_weights[cross_edges$dependency_type])
  u <- matrix(runif(nrow(cross_edges) * iterations), nrow = nrow(cross_edges))
  shift <- (u >= 1 - p / 2) - (u < p / 2)
  recoded <- pmin(3, pmax(0, values + shift))
  weights <- matrix(ifelse(shift == 0, cross_edges$weight, recoded),
                    nrow = nrow(cross_edges))
  weight_totals <- colSums(weights)
  scores <- crossprod(weights[, weight_totals > 0, drop = FALSE], direction)
  if (!nrow(scores)) {
    return(tibble(sequence = sequence_levels, outright_wins = NA_integer_,
                  joint_wins = NA_integer_, iterations = iterations, scored_draws = 0L))
  }
  tied_first <- abs(scores - apply(scores, 1, max)) <= 1e-10
  unique_first <- rowSums(tied_first) == 1
  tibble(sequence = sequence_levels,
         outright_wins = colSums(tied_first & unique_first),
         joint_wins = colSums(tied_first & !unique_first),
         iterations = iterations, scored_draws = nrow(scores))
}

analyse <- function(data_dir = "data") {
  data <- load_inputs(data_dir)
  standards <- data$standards
  sizes <- count(standards, discipline) |> tibble::deframe()
  edges <- filter(data$edges, review_status != "rejected")
  positive <- filter(edges, weight > 0)
  cross <- filter(positive, cross_course)
  list(
    standards = standards, edges = edges, cross = cross,
    course_pairs = weight_summaries(edges, sizes),
    strengths = count(edges, cross_course, dependency_type, name = "relationships") |>
      complete(cross_course = c(FALSE, TRUE), dependency_type = names(study_weights),
               fill = list(relationships = 0)),
    concepts = cross |>
      group_by(concept, source, target) |>
      summarise(edges = n(), weight = sum(weight), .groups = "drop") |>
      arrange(desc(weight), concept, source, target),
    network = network_summary(positive, standards),
    all_relationships = network_summary(edges, standards),
    scores = score_sequences(cross),
    edge_satisfaction = ordering(cross),
    missed = missed_by_course(cross),
    sensitivity = run_scenarios(cross, sizes),
    leave_one_concept_out = map(sort(unique(cross$concept)), function(omitted) {
      score_sequences(filter(cross, concept != omitted)) |> mutate(omitted_concept = omitted)
    }) |> list_rbind(),
    simulation = simulate_rankings(cross)
  )
}
