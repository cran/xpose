#' Save xpose plot
#'
#' @description Inspired by the \link[ggplot2]{ggsave}, this function 
#' facilitates the export of xpose plots.
#'
#' @param plot A xpose plot object.
#' @param file A name with file extension (if device is \code{NULL}) to be given to the 
#' output file. Template variables such as \code{@run} (run number) and \code{@plotfun} 
#' (plot function) can be used to automatically name files e.g. 
#' \code{file = '@run_@plotfun.pdf'}.
#' @param dir Directory under which the xpose plots will be saved. Template variables 
#' such as \code{@dir} can be used to generate template names.
#' @param device Graphical device to use. Can be either be a device function
#'   (e.g. \code{\link{png}}), or one of 'eps', 'ps', 'tex' (pictex),
#'   'pdf' (default), 'jpeg', 'tiff', 'png', 'bmp', 'svg' or 'wmf' (windows only).
#' @param width,height Plot size in \code{units}.
#' @param units Units of the plot size ('in', 'cm', or 'mm').
#' @param dpi Plot resolution. Applies only to raster output types.
#' @param ... Additional arguments passed on to \link[ggplot2]{ggsave} or graphics \code{device}.
#'
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  dv_vs_ipred() %>% 
#'  xpose_save(file = file.path(tempdir(), "dv_vs_ipred_example.pdf"))
#' }
#' @export
xpose_save <- function(plot     = last_plot(),
                       file     = NULL,
                       dir      = NULL,
                       device   = NULL,
                       width    = 7,
                       height   = 6,
                       units    = c('in', 'cm', 'mm'),
                       dpi      = 200,
                       ...) {
  
  if (is.null(plot)) {
    stop('Argument `plot` required.', call. = FALSE)
  } else if (is.null(file)) {
    stop('Argument `file` required.', call. = FALSE)
  }
  
  # Parse the dir and file arguments for keywords
  if (!is.null(dir)) {
    dir <- parse_title(string = dir, xpdb = plot$xpose,
                       problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                       extra_key = 'plotfun', extra_value = plot$xpose$fun)
  }
  
  file <- parse_title(string = file, xpdb = plot$xpose,
                      problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                      extra_key = 'plotfun', extra_value = plot$xpose$fun)
  
  
  # Add device to file
  if (!is.null(device) && !is.function(device) && !grepl('\\.[[:alnum:]]+$', file)) {
    file <- paste0(file, '.', device)
  }
  
  # Call ggsave
  ggsave(plot = plot, filename = file, path = dir, device = device, 
         width = width, height = height, units = units, dpi = dpi, ...)
}
