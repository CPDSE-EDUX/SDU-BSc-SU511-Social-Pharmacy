# Reads the filter variables defined in the qmd (run via source(..., local = TRUE)).

library(arrow)
library(tidyverse)

# ---- Setup -------------------------------------------------------------

person <- open_dataset(file.path(data_dir, "person")) %>%
  collect() %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    age = floor(as.numeric(reference_date - as.Date(birthdate)) / 365.25)
  ) %>%
  filter(age >= 0)

# large tables are only read if a filter needs them
diagnosis <- NULL
if (!is.null(diagnosis_icd_any) || !is.null(diagnosis_icd_all) || !is.null(diagnosis_icd_exclude)) {
  diagnosis <- open_dataset(file.path(data_dir, "diagnosis")) %>%
    select(id, diag) %>%
    collect()
}

drug <- open_dataset(file.path(data_dir, "drug")) %>%
  select(id, atc)

# ---- The cohort: every filter that is not NULL is applied ----------------

# ids matching code patterns in a table; "any" = at least one, "all" = every one
has_code <- function(data, column, patterns, match = "any") {
  hits <- map(patterns, ~ data %>% filter(str_detect(.data[[column]], .x)) %>% distinct(id))
  if (match == "all") reduce(hits, inner_join, by = "id") else bind_rows(hits) %>% distinct(id)
}
has_diagnosis <- function(patterns, match = "any") has_code(diagnosis, "diag", patterns, match)

# ids with at least one or every requested ATC code
has_drug <- function(atc_codes, match = "any") {
  ids <- drug %>%
    filter(atc %in% atc_codes) %>%
    distinct(id, atc) %>%
    collect() %>%
    count(id)

  if (match == "all") {
    filter(ids, n >= length(unique(atc_codes))) %>% select(id)
  } else {
    filter(ids, n > 0) %>% select(id)
  }
}

cohort <- person

if (!is.null(diagnosis_icd_any)) cohort <- semi_join(cohort, has_diagnosis(diagnosis_icd_any, "any"), by = "id")
if (!is.null(diagnosis_icd_all)) cohort <- semi_join(cohort, has_diagnosis(diagnosis_icd_all, "all"), by = "id")
if (!is.null(diagnosis_icd_exclude)) cohort <- anti_join(cohort, has_diagnosis(diagnosis_icd_exclude, "any"), by = "id")
if (!is.null(sex_keep))          cohort <- filter(cohort, str_to_lower(sex) == str_to_lower(sex_keep))
if (!is.null(age_min))           cohort <- filter(cohort, age >= age_min)
if (!is.null(age_max))           cohort <- filter(cohort, age <= age_max)
if (!is.null(drug_atc_any))      cohort <- semi_join(cohort, has_drug(drug_atc_any, "any"), by = "id")
if (!is.null(drug_atc_all))      cohort <- semi_join(cohort, has_drug(drug_atc_all, "all"), by = "id")
if (!is.null(drug_atc_exclude))  cohort <- anti_join(cohort, has_drug(drug_atc_exclude, "any"), by = "id")

if (!is.null(custom_filter)) {
  # a single include()/exclude() rule or a list of them
  for (rule in c(custom_filter)) cohort <- rule(cohort)
}

cat("All persons:", format(nrow(person), big.mark = ","), "\n")
cat("Cohort:     ", format(nrow(cohort), big.mark = ","), "\n")

# ---- Plot: all persons vs. cohort -----------------------------------------

n_fmt <- function(d) format(nrow(d), big.mark = ",")
lvl_all    <- paste0("All persons (N = ", n_fmt(person), ")")
lvl_cohort <- paste0("Cohort (N = ", n_fmt(cohort), ")")

both <- bind_rows(
  person %>% mutate(group = lvl_all,    fill = as.character(sex)),
  cohort %>% mutate(group = lvl_cohort, fill = as.character(sex))
) %>%
  mutate(group = factor(group, levels = c(lvl_all, lvl_cohort)),
         fill  = factor(fill, levels = c("Male", "Female")))

p <- ggplot(both, aes(x = age, fill = fill)) +
  geom_histogram(binwidth = bin_width, boundary = 0, colour = "white", linewidth = 0.3,
                 position = "stack", alpha = 0.95) +
  facet_wrap(vars(group), nrow = 1, drop = FALSE) +
  scale_fill_manual(values = sex_colours) +
  scale_x_continuous(breaks = scales::breaks_width(10)) +
  scale_y_continuous(breaks = scales::breaks_pretty(),
                     labels = scales::label_comma(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Age distribution by sex",
       subtitle = paste0("Reference date: ", format(reference_date, "%d %B %Y")),
       x = "Age (years)", y = "People", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 2)),
    plot.subtitle = element_text(colour = "grey35", margin = margin(b = 10)),
    plot.title.position = "plot",
    strip.text = element_text(face = "bold", size = 13, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88"),
    panel.spacing = unit(1.5, "lines"),
    axis.title = element_text(colour = "grey25"),
    axis.text = element_text(colour = "grey35"),
    axis.line.x = element_line(colour = "grey60"),
    legend.position = "bottom",
    legend.key.size = unit(0.9, "lines"),
    plot.margin = margin(12, 16, 6, 12)
  )

age_filter <- if (!is.null(age_min) && !is.null(age_max)) {
  paste0("Age: ", age_min, " to ", age_max)
} else if (!is.null(age_min)) {
  paste0("Age: at least ", age_min)
} else if (!is.null(age_max)) {
  paste0("Age: up to ", age_max)
}

criteria <- c(
  if (!is.null(sex_keep)) paste0("Sex: ", paste(sex_keep, collapse = ", ")) else NULL,
  age_filter,
  if (!is.null(diagnosis_icd_any)) paste0("Diagnosis (any): ", paste(diagnosis_icd_any, collapse = ", ")) else NULL,
  if (!is.null(diagnosis_icd_all)) paste0("Diagnosis (all): ", paste(diagnosis_icd_all, collapse = ", ")) else NULL,
  if (!is.null(diagnosis_icd_exclude)) paste0("Exclude diagnosis: ", paste(diagnosis_icd_exclude, collapse = ", ")) else NULL,
  if (!is.null(drug_atc_any)) paste0("Drug (any): ", paste(drug_atc_any, collapse = ", ")) else NULL,
  if (!is.null(drug_atc_all)) paste0("Drug (all): ", paste(drug_atc_all, collapse = ", ")) else NULL,
  if (!is.null(drug_atc_exclude)) paste0("Exclude drug: ", paste(drug_atc_exclude, collapse = ", ")) else NULL,
  if (!is.null(custom_filter)) paste0("Custom filter: ", custom_filter_label) else NULL
)
if (length(criteria) == 0) criteria <- "No optional filters (all persons)"

criteria_text <- paste(c("Current filter criteria", str_wrap(criteria, width = 110)), collapse = "\n")

print(
  p +
    labs(caption = criteria_text) +
    theme(plot.caption = element_text(hjust = 0, size = 10, colour = "grey20",
                                      lineheight = 1.2, margin = margin(t = 12)),
          plot.caption.position = "plot")
)
