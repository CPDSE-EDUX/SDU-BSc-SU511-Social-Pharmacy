# Helpers for custom_filter in SU511.qmd:
# include_any(), include_all(), exclude_any(), exclude_all().
# Loaded before the filter variables, so custom_filter <- include_any(...) works there.

# ids with at least one record in `table` that matches a condition
has_record <- function(table, condition) {
  cond <- rlang::enquo(condition)
  records <- arrow::open_dataset(file.path(data_dir, table))
  # Arrow cannot filter on a bare TRUE, so TRUE means "any record"
  if (!isTRUE(rlang::quo_get_expr(cond))) records <- dplyr::filter(records, !!cond)
  records |>
    dplyr::distinct(id) |>
    dplyr::collect()
}

# ids with a record matching at least one ("any") or every ("all") pattern in `column`
# no column = any record in the table
ids_matching <- function(table, column, pattern, match) {
  if (is.null(column)) return(has_record(table, TRUE))
  one <- function(p) {
    cond <- rlang::call2("str_detect", rlang::sym(column), p)
    has_record(table, !!cond)
  }
  if (match == "all") {
    Reduce(function(a, b) dplyr::inner_join(a, b, by = "id"), lapply(pattern, one))
  } else {
    one(paste(pattern, collapse = "|"))
  }
}

make_rule <- function(keep, match) {
  function(table, column = NULL, pattern = NULL) {
    column <- if (missing(column)) NULL else rlang::as_name(rlang::ensym(column))
    join <- if (keep) dplyr::semi_join else dplyr::anti_join
    function(cohort) join(cohort, ids_matching(table, column, pattern, match), by = "id")
  }
}

# include_any("drug", atc, c("A10BA02", "C09AA05")): at least one of the codes
# include_all(...): every one of the codes; exclude_*() removes those people instead
# "^C50" = starts with C50. Without column and pattern = any record in the table
include_any <- make_rule(TRUE,  "any")
include_all <- make_rule(TRUE,  "all")
exclude_any <- make_rule(FALSE, "any")
exclude_all <- make_rule(FALSE, "all")
