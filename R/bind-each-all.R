#' Bind each all.
#'
#' @description
#' Binds data to support plotting each category _and_ all combined data.
#'
#' @param data A data frame or tibble.
#' @param ... Require named arguments (and support trailing commas).
#' @param name A variable name. Defaults to `"each_all"`.
#' @param each A string for the each value. Defaults to `"Each"`.
#' @param all A string for the all value. Defaults to `"All"`.
#' @param after A number for where the all value should be placed after. Defaults to `Inf`, which puts `"All"` last. Use `0` to put `"All"` first.
#'
#' @return A data frame or tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' palmerpenguins::penguins |>
#'   count(species)
#' 
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   count(species, each_all)
#' 
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   ggplot(aes(x = species, y = body_mass_g)) +
#'   geom_jitter()
#' 
#' palmerpenguins::penguins |>
#'   bind_each_all(species, all = "All\nspecies") |>
#'   ggplot(aes(x = species, y = body_mass_g, colour = each_all)) +
#'   geom_jitter() +
#'   facet_wrap(facets = vars(each_all), scales = "free_x", space = "free_x") +
#'   theme(strip.text.x = element_blank()) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL) +
#'   scale_colour_discrete(palette = c("steelblue", "darkgrey"))
#'
bind_each_all <- function(
    data,
    ...,
    name = "each_all",
    each = "Each",
    all = "All",
    after = Inf
) {
  if (...length() != 1) {
    stop("Please provide one variable")
  }
  by <- rlang::enquos(...)[1][[1]]
  if (inherits(rlang::eval_tidy(by, data), what = c("factor"))) {
    levels <- levels(rlang::eval_tidy(by, data))
    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := factor(!!by, levels = c(levels, all))) |>
      dplyr::mutate(
        !!by := {
          col_values <- !!by
          if (any(is.na(col_values))) {
            forcats::fct_relevel(
              forcats::fct_na_value_to_level(col_values),
              all,
              after = after
            )
          } else {
            forcats::fct_relevel(col_values, all, after = after)
          }
        }
      )
  } else {
    data <- data |>
      dplyr::mutate(!!by := as.character(!!by)) |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(
        !!by := {
          col_values <- !!by
          if (any(is.na(col_values))) {
            forcats::fct_relevel(
              forcats::fct_na_value_to_level(col_values),
              all,
              after = after
            )
          } else {
            forcats::fct_relevel(as.factor(col_values), all, after = after)
          }
        }
      )
  }
  data <- data |>
    dplyr::mutate(
      !!name := dplyr::if_else(!!by == all, all, each, missing = each)
    ) |>
    dplyr::mutate(dplyr::across(!!name, forcats::fct_inorder))
  if (after == 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(!!name, forcats::fct_rev))
  }
  return(data)
}
