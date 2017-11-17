#' Create xpose theme
#'
#' @description Create an xpose theme.
#' This function will update the theme of an xpdb object.
#' All plots generated with this xpdb will automatically use the
#' defined xpose (\code{xp_theme}) and ggplot2 (\code{gg_theme}) themes.
#'
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param gg_theme A ggplot2 theme object (e.g. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (e.g. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @examples
#' # Before default theme
#' dv_vs_ipred(xpdb_ex_pk, facets = 'SEX')
#' 
#' # Updating the gg_theme and xp_theme
#' xpdb_ex_pk %>% 
#'   update_themes(gg_theme = theme_bw(),
#'                 xp_theme = list(point_color = 'blue',
#'                                 line_color  = 'blue')) %>% 
#'   dv_vs_ipred(facets = 'SEX')
#'   
#' @export
update_themes <- function(xpdb     = NULL,
                          gg_theme = NULL,
                          xp_theme = NULL,
                          quiet) {
  # Check input
  check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Replace/Update gg_theme
  if (is.theme(gg_theme)) {
    if (attr(gg_theme, 'complete')) {
      attr(gg_theme, 'theme') <- as.character(substitute(gg_theme)) 
      xpdb$gg_theme <- gg_theme
    } else {
      attr(xpdb$gg_theme, 'theme') <- paste(attr(xpdb$gg_theme, 'theme'), '(modified)')
      xpdb$gg_theme <- xpdb$gg_theme + gg_theme
    }  
  } else if (!is.null(gg_theme) & !is.theme(gg_theme)) {
    msg('`gg_theme` argument not used. Reason: invalid input.', quiet)
  }
  
  # Replace/Update xp_theme
  if (!is.null(xp_theme)) { 
    if (is.xp.theme(xp_theme)) {
      attr(xp_theme, 'theme') <- as.character(substitute(xp_theme)) 
      xpdb$xp_theme <- xp_theme
      
    } else if (!is.null(names(xp_theme))) {
      for (x in seq_along(xp_theme)) {
        xpdb$xp_theme[[names(xp_theme[x])]] <- xp_theme[[x]] # Beware drops NULL elements
      }
      attr(xpdb$xp_theme, 'theme') <- paste(attr(xpdb$xp_theme, 'theme'), '(modified)')
    } else {
      msg('`xp_theme` argument not used. Reason: invalid input.', quiet)
    }
  }
  xpdb
}
