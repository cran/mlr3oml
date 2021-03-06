#' @title List Tasks from OpenML
#'
#' @description
#' This function allows to find tasks on \url{https://openml.org/t} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters is exposed here.
#' For a more feature-complete package, see \CRANpkg{OpenML}.
#'
#' @details
#' Filter values can be provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
#' @param task_id (`integer()`)\cr
#'   Vector of task ids to restrict to.
#' @inheritParams list_oml_data_sets
#'
#' @return (`data.table()`) of results, or a Null data.table if no task matches the criteria.
#'
#' @references
#' `r format_bib("openml_r", "vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' list_oml_tasks(number_instances = 150, number_features = c(1, 10))
#' }
list_oml_tasks = function(task_id = NULL, number_instances = NULL, number_features = NULL,
  number_classes = NULL, number_missing_values = NULL, tag = NULL, limit = 5000L, ...) {

  dots = list(
    task_id = task_id,
    number_instances = number_instances,
    number_features = number_features,
    number_classes = number_classes,
    number_missing_values = number_missing_values,
    tag = tag
  )
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)
  dots = insert_named(discard(dots, is.null), list(...))
  tab = get_paginated_table(dots, "task", limit)

  if (nrow(tab)) {
    setnames(tab, "did", "data_id")
    qualities = transpose_name_value(tab$quality, as_integer = TRUE)
    rcbind(remove_named(tab, c("task_type_id", "task_type_id", "format", "input", "quality")), qualities)
  }

  return(tab)
}
