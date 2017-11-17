#' Print an xpose_data object
#' 
#' @description This function returns to the console a list of the files and options 
#' attached to an \code{\link{xpose_data}} object.
#' 
#' @param x An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param ... Ignored in this function
#' 
#' @method print xpose_data
#' @examples 
#' # Using the print function
#' print(xpdb_ex_pk)
#' 
#' # Or simply by writting the xpdb name
#' xpdb_ex_pk
#' 
#' @export
print.xpose_data <- function(x, ...) {
  if (!is.null(x$data) && any(!x$data$simtab)) {
    tab_names <- x$data %>% 
      dplyr::filter(.$simtab == FALSE) %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_(.dots = 'grouping') %>% 
      tidyr::nest() %>% 
      dplyr::mutate(string = purrr::map_chr(.$data, summarize_table_names)) %>% 
      {stringr::str_c(.$string, collapse = '\n               ')}
  } else {
    tab_names <- '<none>'
  }
  
  if (!is.null(x$data) && any(x$data$simtab)) {
    sim_names <- x$data %>% 
      dplyr::filter(.$simtab == TRUE) %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_(.dots = 'grouping') %>% 
      tidyr::nest() %>% 
      dplyr::mutate(string = purrr::map_chr(.$data, summarize_table_names)) %>% 
      {stringr::str_c(.$string, collapse = '\n               ')}
  } else {
    sim_names <- '<none>'
  }
  
  if (!is.null(x$files)) {
    out_names <- x$files %>% 
      dplyr::distinct_(.dots = 'name', .keep_all = TRUE) %>% 
      dplyr::arrange_(.dots = c('name')) %>% 
      {stringr::str_c(.$name, ifelse(.$modified, ' (modified)', ''), collapse = ', ')}
  } else {
    out_names <- '<none>'
  }
  
  if (!is.null(x$special)) {
    special_names <- stringr::str_c(x$special$method, ' ', x$special$type, 
                                    ' (#', x$special$problem,
                                    ifelse(x$special$modified, ', modified', ''),
                                    ')', collapse = ', ')
  } else {
    special_names <- '<none>'
  }
  
  cat(x$summary$value[x$summary$label == 'file'], 'overview:',
      '\n - Software:', x$summary$value[x$summary$label %in% c('software', 'version') & x$summary$value != 'na'],
      stringr::str_c('\n - Attached files (memory usage ', format(utils::object.size(x), units = 'auto'), '):'),  
      '\n   + obs tabs:', tab_names,
      '\n   + sim tabs:', sim_names,
      '\n   + output files:', out_names,
      '\n   + special:', special_names,
      '\n - gg_theme:', attr(x$gg_theme, 'theme'),
      '\n - xp_theme:', attr(x$xp_theme, 'theme'),
      '\n - Options:', stringr::str_c(names(x$options), x$options, sep = ' = ', collapse = ', '))
}

summarize_table_names <- function(dat) {
  purrr::map(dat$index, ~.$table) %>% 
    purrr::flatten_chr() %>% 
    sort() %>% 
    unique() %>% 
    stringr::str_c(collapse = ', ') %>% 
    {stringr::str_c('$prob no.', dat$problem, ifelse(dat$modified, ' (modified)', ''), ': ', .)}
}
