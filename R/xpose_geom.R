#' Parse arguments from xpose to ggplot2 format 
#' 
#' @description Transform arguments from xpose (e.g. point_color) 
#' to ggplot2 (e.g. color) format.
#' 
#' @param x A list of xpose plot aesthetics.
#' @param name Name of the destination layer for the argument 
#' parsing (e.g. point).
#' 
#' @return ggplot2 aesthetics for the layer defined in `name`.
#' 
#' @keywords internal
#' @export
parse_arg <- function(x = NULL, name) {
  if (is.null(x)) return()
  x[stringr::str_detect(names(x), stringr::str_c('^', name, '_'))] %>% 
    purrr::set_names(nm = stringr::str_replace(names(.), stringr::str_c(name, '_'), '')) %>% 
    purrr::set_names(nm = stringr::str_replace(names(.), 'color', 'colour'))
}


#' Update `xpose_geoms` arguments
#' 
#' @description Combine the arguments from the user and the `xp_theme`.
#' 
#' @param thm_arg A subset of `xp_theme` used to test defaults.
#' @param name Name of the destination layer for the argument 
#' parsing (e.g. point).
#' @param ... User arguments.
#' 
#' @return A list of arguments for the layer defined in `name`.
#' 
#' @keywords internal
#' @export
update_args <- function(thm_arg, name, ...) {
  thm_arg <- parse_arg(x = thm_arg, name = name)
  usr_arg <- parse_arg(x = list(...), name = name)
  usr_changes <- intersect(names(thm_arg), names(usr_arg))
  thm_arg[usr_changes] <- usr_arg[usr_changes]
  c(thm_arg, usr_arg[!names(usr_arg) %in% names(thm_arg)])
}

#' ggplot2 layer call
#' 
#' @description Function calling the ggplot2 layer function 
#' with its parsed arguments.
#' 
#' @param arg A list of arguments for the target layer.
#' @param mapping ggplot2 aesthetics for the target layer.
#' @param ggfun Name of the ggplot2 layer function to be called.
#' 
#' @return Output of the `ggfun` call.
#' 
#' @keywords internal
#' @export
xp_map <- function(arg, mapping, ggfun) {
  x <- do.call(ggfun, arg[!names(arg) %in% names(mapping)])
  if (!is.null(mapping)) x$mapping <- mapping
  x
}

#' Generic ggplot2 layer for `xpose_plots`
#' 
#' @description Generic wrapper around ggplot2 layer functions.
#' 
#' @param mapping ggplot2 aesthetics for the target layer.
#' @param xp_theme An `xpose_theme` object.
#' @param name Name of the destination layer for the argument 
#' parsing (e.g. point).
#' @param ggfun Name of the ggplot2 layer function to be called.
#' @param ... Additional arguments to be parsed and passed to the 
#' destination layer.
#' 
#' @return Output of the `ggfun` call.
#' 
#' @keywords internal
#' @export
xp_geoms <- function(mapping = NULL, xp_theme, name, ggfun, ...) {
  if (!is.null(mapping)) mapping <- parse_arg(mapping, name)
  thm_arg <- filter_xp_theme(xp_theme, stringr::str_c('^', name, '_')) 
  arg     <- update_args(thm_arg, name, ...)
  xp_map(arg, mapping, ggfun)
}

#' Generic panel function for `xpose_plots`
#' 
#' @description Convenience wrapper around ggforce faceting functions.
#' 
#' @param xp_theme An `xpose_theme` object.
#' @param extra_args User arguments to be passed to the 
#' faceting functions.
#' 
#' @return Output a `facet` layer. Layer will be `facet_wrap_paginate` if 
#' `facets` is a string, and `facet_grid_paginate` if `facets` is a formula.
#' 
#' @keywords internal
#' @export
xpose_panels <- function(xp_theme, extra_args) {
  if (!is.formula(extra_args$facets)) {
    thm_arg  <- xp_theme[c('facets', 'nrow', 'ncol', 'scales', 'shrink', 
                           'labeller', 'as.table', 'drop', 'dir', 
                           'switch', 'strip.position', 'page')]
    facet_fun <- 'facet_wrap_paginate'
  } else {
    thm_arg  <- xp_theme[c('facets', 'margins', 'scales', 'space', 'shrink', 
                           'labeller', 'as.table', 'switch', 'drop', 'ncol', 
                           'nrow', 'page', 'byrow')]
    facet_fun <- 'facet_grid_paginate'
  }
  usr_changes    <- intersect(names(thm_arg), names(extra_args))
  class(thm_arg) <- 'list'
  thm_arg[usr_changes] <- extra_args[usr_changes]
  do.call(facet_fun, thm_arg)
}
