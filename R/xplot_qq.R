#' Default xpose QQ plot function
#'
#' @description Manually generate QQ plots from an xpdb object.
#'
#' @inheritParams xplot_scatter
#' @param type String setting the type of plot to be used. Can only be points 'p'.
#' @param guide Should the guide (e.g. reference line) be displayed.
#' 
#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. point_color = 'blue', etc.
#' \itemize{
#'   \item point: options to \code{geom_point}
#'   \item guide: options to \code{geom_abline}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}} \code{\link{xplot_distrib}}
#' 
#' @examples
#' xplot_qq(xpdb_ex_pk, aes(sample = CWRES), guide = TRUE)
#' 
#' @export
xplot_qq <- function(xpdb,
                     mapping   = NULL,
                     type      = 'p',
                     guide     = FALSE,
                     xscale    = 'continuous',
                     yscale    = 'continuous',
                     title     = NULL,
                     subtitle  = NULL,
                     caption   = NULL,
                     plot_name = 'qq_plot',
                     gg_theme,
                     xp_theme,
                     opt,
                     quiet,
                     ...) {
  # Check input
  check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (missing(opt)) opt <- data_opt()
  data <- fetch_data(xpdb, quiet = quiet, .problem = opt$problem, .subprob = opt$subprob, 
                     .method = opt$method, .source = opt$source, simtab = opt$simtab, 
                     filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col, 
                     value_col = opt$value_col, post_processing = opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    stop('No data available for plotting. Please check the variable mapping and filering options.', 
         call. = FALSE)
  }
  
  # Check type
  check_plot_type(type, allowed = 'p')
  
  # Assing xp_theme and gg_theme
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  if (missing(gg_theme)) gg_theme <- xpdb$gg_theme
  
  # Create ggplot base
  xp <- ggplot(data = data, mapping) + gg_theme 
  
  # Add points (note: could not get geom_text to work with stat_qq)
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping    = mapping,
                        xp_theme   = xpdb$xp_theme,
                        name       = 'point',
                        ggfun      = 'geom_point',
                        point_stat = 'qq',
                        ...)
  }
  
  # Add reference line
  if (guide) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guide',
                        ggfun    = 'geom_qq_line',
                        ...)
  }
  
  # Define scales
  xp <- xp + 
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = paste0('scale_x_', xscale),
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = paste0('scale_y_', yscale),
             ...)
  
  # Define panels
  if (!is.null(list(...)[['facets']])) {
    xp <- xp + xpose_panels(xp_theme = xpdb$xp_theme, 
                            extra_args = list(...))
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle', 'caption'), 
                                                           '_suffix')])
  
  # Ouptut the plot
  structure(xp, class = c('xpose_plot', class(xp)))
}


# Function based on code written by from Nick Solomon: https://github.com/nicksolomon
geom_qq_line <- function(mapping = NULL, data = NULL,
                         geom = 'path', position = 'identity',
                         ..., distribution = stats::qnorm,
                         dparams = list(),
                         line.p = c(.25, .75),
                         line.expand = c(-.1, .1),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = StatQqLine,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, 
        params = list(distribution = distribution,
                      dparams = dparams, na.rm = na.rm,
                      line.p = line.p, line.expand = line.expand, ...))
}

StatQqLine <- ggproto('StatQqLine', Stat,
                      default_aes = aes(x = ..x.., y = ..y..),
                      required_aes = c('sample'),
                      compute_group = function(data, scales, quantiles = NULL,
                                               distribution = stats::qnorm,
                                               dparams = list(),
                                               na.rm = FALSE,
                                               line.p = c(.25, .75),
                                               line.expand = c(-.1, .1)) {
                        sample <- sort(data$sample)
                        n <- length(sample)
                        
                        # Compute theoretical quantiles
                        if (is.null(quantiles)) {
                          quantiles <- stats::ppoints(n)
                        } else {
                          stopifnot(length(quantiles) == n)
                        }
                        
                        theoretical <- do.call(distribution,
                                               c(list(p = quote(quantiles)), dparams))
                        if (length(line.p) != 2) {
                          stop('Cannot fit line quantiles ', line.p,
                               '. Parameter line.p must have length 2.',
                               call = FALSE)
                        }
                        x_coords <- do.call(distribution, c(list(p = line.p), dparams))
                        y_coords <- quantile(sample, line.p)
                        slope = diff(y_coords)/diff(x_coords)
                        intercept = y_coords[1L] - slope * x_coords[1L]
                        
                        if (length(line.expand) != 2) {
                          stop('Parameter line.expand must have length 2.', call = FALSE)
                        }
                        
                        out <- data.frame(x = c(min(theoretical) + line.expand[1L],
                                                max(theoretical) + line.expand[2L]))
                        out$y <- slope * out$x + intercept
                        out
                      }
)
