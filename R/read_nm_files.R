#' NONMEM output file import function
#'
#' @description Quickly import NONMEM output files into R.
#'
#' @param runno Run number to be evaluated.
#' @param prefix Prefix of the model file names.
#' @param ext A vector of the file extension to import. By default '.ext', '.cor', '.cov', '.phi', '.grd', '.shk'
#' files are listed.
#' @param file Names of the model output file to be imported. Alternative argument to \code{prefix},
#' \code{runno} and \code{ext}.
#' @param dir Location of the model files.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @inheritSection xpose_data File path generation
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' # Using the `file` argument to import a model file:
#' ext_file <- read_nm_files(file = 'run001.ext', dir = 'models')
#' 
#' # Using the `runno` argument to import a model file:
#' ext_file <- read_nm_files(runno = '001', ext = '.ext', dir = 'models')
#' }
#' @export
read_nm_files <- function(runno  = NULL,
                          prefix = 'run',
                          ext    = c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk'),
                          file   = NULL,
                          dir    = NULL,
                          quiet  = FALSE) {
  
  # Check inputs
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (missing(quiet)) quiet <- !interactive()
  
  # Check for readr lazy loading
  if (readr::should_read_lazy() & .Platform$OS.type == "windows") {
    warning("Using lazy loading in `readr` on Windows can cause unexpected behavior and is not recommended with `xpose`.", call. = FALSE)
  }
  
  # Generate full paths
  if (!is.null(runno)) {
    full_path <- file_path(dir, stringr::str_c(prefix, runno, make_extension(ext)))
  } else {
    full_path <- file_path(dir, file)
  }
  
  full_path <- sort(unique(full_path))
  bases     <- basename(full_path)
  
  msg('\nLooking for nonmem output files', quiet)
  
  if (!any(file.exists(full_path))) {
    stop('No output files could be found.', call. = FALSE)
  }
  
  msg(c('Reading: ', stringr::str_c(bases[file.exists(full_path)], collapse = ', ')), quiet)
  
  out <- full_path %>% 
    dplyr::tibble(path = ., name = basename(.)) %>% 
    dplyr::filter(file.exists(.$path)) %>% 
    dplyr::mutate(grouping = 1:n(),
                  raw = purrr::map(.$path, .f = readr::read_lines)) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(tmp = purrr::map(.$data, .f = parse_nm_files, quiet)) %>% 
    dplyr::mutate(drop = purrr::map_lgl(.$tmp, is.null)) 
  
  if (all(out$drop)) stop('No output file imported.', call. = FALSE)
  
  out %>% 
    dplyr::filter(!.$drop) %>% 
    tidyr::unnest(dplyr::one_of('data')) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::mutate(extension = get_extension(.$name, dot = FALSE),
                  modified = FALSE) %>% 
    dplyr::select(dplyr::one_of('name', 'extension', 'problem', 'subprob', 
                                'method', 'data', 'modified'))
}


#' Parse NONMEM output files
#' 
#' @description Function parsing NONMEM output files from their 
#' raw input.
#' 
#' @param dat A list containing the raw data as vector of strings (`dat$raw`)
#' and their respective file names (`dat$name`).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A tibble containing the parsed `data` along with `name`, `problem`, 
#' `subprob`, and `method`.
#' 
#' @keywords internal
#' @export
parse_nm_files <- function(dat, quiet) {
  if (length(unlist(dat$raw)) == 0) {
    tab_rows <- NULL 
  } else {
    x <- dplyr::tibble(
      raw = unlist(dat$raw), 
      problem = NA_character_, 
      subprob = NA_character_, 
      method  = NA_character_, 
      header  = FALSE
      )
    tab_rows <- which(stringr::str_detect(x$raw, '^\\s*TABLE NO'))
  }
  
  if (length(tab_rows) == 0) {
    warning(c('Dropped `', dat$name, '` due to inappropriate format.'), call. = FALSE)
    return()
  }
  
  x[tab_rows, ]$problem <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Problem=(\\d+)')[,2]
  
  ## Give stacked estimation methods a unique subproblem number
  ## Note: this allows the use of the same estimation method several times within one $PROBLEM
  x <- dplyr::mutate(x, subprob = cumsum(duplicated(!!rlang::sym("problem")) & !is.na(!!rlang::sym("problem"))) + 1)
  
  x[tab_rows, ]$method  <- dplyr::case_when(stringr::str_detect(x[tab_rows, ]$raw, 'First Order Conditional') ~ 'foce',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'First Order') ~ 'fo',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Laplacian Conditional') ~ 'lce', 
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Iterative Two Stage') ~ 'its',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Importance Sampling') ~ 'imp',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Stochastic Approximation') ~ 'saem',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Markov-Chain') ~ 'bayes',
                                            TRUE ~ 'na')
  
  # Assumes that header are always present
  x[tab_rows + 1, ]$header <- TRUE
  
  # Guess column separator from the first problem only
  sep <- dplyr::case_when(
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ';[A-z]+') ~ ';[A-z]|[A-z];',
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ',[A-z]+') ~ ',[A-z]|[A-z],',
    TRUE ~ '\\s+')
  
  x %>% 
    tidyr::fill(dplyr::one_of('problem', 'subprob', 'method')) %>% 
    dplyr::slice(-tab_rows) %>%
    dplyr::mutate(problem = as.numeric(.$problem),
                  subprob = as.numeric(.$subprob),
                  raw = stringr::str_trim(.$raw, side = 'both')) %>% 
    dplyr::group_by_at(.vars = c('problem', 'subprob', 'method')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(data = purrr::map(.$data, 
                                    .f   = raw_to_tibble, 
                                    sep  = sep, 
                                    file = dat$name))
}  


#' Convert raw strings to tibble
#' 
#' @description Convert raw data strings to a tibble format.
#' 
#' @param x A list containing the raw data as vector of strings (`x$raw`).
#' @param sep A separator which will be used to create columns.
#' @param file The name of the file to be parsed.
#' 
#' @return A tibble.
#' 
#' @keywords internal
#' @export
raw_to_tibble <- function(x, sep, file) {
  header <- x$raw[x$header] %>% 
    stringr::str_split(pattern = sep) %>% 
    purrr::flatten_chr()
  
  if (any(is.na(header))) {
    warning(c('Issue encountered while parsing ', file, '.'), call. = FALSE)
    return()
  }
  
  x[!x$header, ] %>%   
    tidyr::separate(col = 'raw', sep = sep, into = header) %>% 
    dplyr::select(-ncol(.)) %>% 
    dplyr::mutate_if(colnames(.) != 'NAME', as.numeric)
}
