#' Generate a list of options for VPC data generation
#' 
#' @description Provide a list of options to \code{vpc_data} function.
#' 
#' @param bins Binning method, can be one of 'density', 'time', 'data', 'none', or one of the approaches 
#' available in \code{classInterval()} such as 'jenks' (default), 'pretty', or a numeric vector specifying 
#' the bin separators.
#' @param n_bins When using the 'auto' binning method, what number of bins to aim for.
#' @param bin_mid Specify how to is the mid bin value calculated, can be either 'mean' for the mean of all 
#' timepoints (default) or 'middle' to use the average of the bin boundaries.
#' @param pred_corr Option reserved to continuous VPC. Logical, should a prediction correction (pcVPC) of the data be used.
#' @param pred_corr_lower_bnd Option reserved to continuous VPC. Lower bound for the prediction-correction.
#' @param pi Option reserved to continuous VPC. Simulated prediction interval to plot. Default is c(0.05, 0.95).
#' @param ci Confidence interval around the percentiles to plot. Default is c(0.05, 0.95)
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.               
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param rtte Option reserved to time-to-event VPC. Is the data repeated time-to-event (RTTE) \code{TRUE} or 
#' single time-to-event (TTE) \code{FALSE}.
#' @param rtte_calc_diff Option reserved to time-to-event VPC. Should the time be recalculated? When simulating in NONMEM, 
#' you will probably need to set this to \code{TRUE} to recalculate the TIME to the relative time between events (unless you 
#' output the time difference between events and specify that as independent variable in the index.
#' @param kmmc Option reserved to time-to-event VPC. Either NULL for regular TTE VPC (default), or a variable name 
#' for a KMMC plot (e.g. 'WT').
#' @param events Option reserved to time-to-event VPC. Numeric vector describing which events to show a VPC for when 
#' repeated TTE data, e.g. c(1:4). Default is \code{NULL}, which shows all events.
#' @param reverse_prob Option reserved to time-to-event VPC. Should the probability be reversed (i.e. plot 1-probability).
#' @param as_percentage Should the Y-scale be in percent (0-100) \code{TRUE} (default), or standard (0-1) \code{FALSE}.
#'
#' @seealso \code{\link{vpc}} \code{\link{vpc_data}}
#' 
#' @examples
#' vpc_opt()
#' 
#' @export
vpc_opt <- function(bins                = 'jenks',
                    n_bins              = 'auto',
                    bin_mid             = 'mean',
                    pred_corr           = FALSE, 
                    pred_corr_lower_bnd = 0,
                    pi                  = c(0.025, 0.975), 
                    ci                  = c(0.025, 0.975), 
                    lloq                = NULL, 
                    uloq                = NULL,
                    rtte                = FALSE,
                    rtte_calc_diff      = TRUE,
                    events              = NULL,
                    kmmc                = NULL,
                    reverse_prob        = FALSE,
                    as_percentage       = TRUE) {
  
  list(bins = bins, n_bins = n_bins, bin_mid = bin_mid, pred_corr = pred_corr, 
       pred_corr_lower_bnd = pred_corr_lower_bnd, pi = pi, ci = ci, lloq = lloq, 
       uloq = uloq, rtte = rtte, rtte_calc_diff = rtte_calc_diff, events = events, 
       kmmc = kmmc, reverse_prob = reverse_prob, as_percentage = as_percentage, 
       usr_call = names(match.call()[-1]))
}


# Gets strata info from PsN command
get_psn_vpc_strat <- function(psn_cmd) {
  if (stringr::str_detect(psn_cmd, '-stratify_on')) {
    psn_cmd %>% 
    {stringr::str_match(string = ., pattern = '-stratify_on=\\s*([^\\s]+)')[1, 2]} %>% 
      stringr::str_split(',') %>% 
      purrr::flatten_chr()
  }
}

# Gets vpc options from PsN directory
psn_vpc_parser <- function(xpdb, psn_folder, psn_bins, opt, quiet) {
  
  psn_folder <- parse_title(string = psn_folder, xpdb = xpdb, quiet = quiet,
                            problem = default_plot_problem(xpdb))
  
  if (!dir.exists(psn_folder)) {
    stop('The `psn_folder`:', psn_folder, ' could not be found.', call. = FALSE)
  }
  msg('Importing PsN generated data', quiet)
  
  if (!dir.exists(file_path(psn_folder, 'm1')) &
      file.exists(file_path(psn_folder, 'm1.zip'))) {
    msg('Unziping PsN m1 folder', quiet)
    utils::unzip(zipfile = file_path(psn_folder, 'm1.zip'), 
                 exdir   = file_path(psn_folder, ''))
    unzip <- TRUE
  } else {
    unzip <- FALSE 
  }
  obs_data <- read_nm_tables(file = dir(file_path(psn_folder, 'm1'), pattern = 'original.npctab')[1],
                             dir = file.path(psn_folder, 'm1'), quiet = TRUE)
  sim_data <- read_nm_tables(file = dir(file_path(psn_folder, 'm1'), pattern = 'simulation.1.npctab')[1],
                             dir = file.path(psn_folder, 'm1'), quiet = TRUE)
  if (unzip) unlink(x = file_path(psn_folder, 'm1'), recursive = TRUE, force = TRUE)
  
  # Getting multiple options form the psn command
  if (file.exists(file_path(psn_folder, 'version_and_option_info.txt'))) {
    # Get list of options from PsN
    psn_opt <- readr::read_lines(file = file_path(psn_folder, 'version_and_option_info.txt')) 
    psn_cmd <- psn_opt[which(stringr::str_detect(psn_opt, '^Command:')) + 1]
    psn_opt <- dplyr::tibble(raw = psn_opt[stringr::str_detect(psn_opt,'^-')]) %>% 
      tidyr::separate(col = 'raw', into = c('arg', 'value'), sep = '=') %>% 
      dplyr::mutate(arg = stringr::str_replace(.$arg, '^-', ''))
    
    # Sets obs and sim cols
    obs_cols <- list(id = 'ID', idv = psn_opt$value[psn_opt$arg == 'idv'], 
                     dv = psn_opt$value[psn_opt$arg == 'dv'], pred = 'PRED')  
    sim_cols <- obs_cols
    
    # Sets additional options
    if (!any(opt$usr_call == 'pred_corr')) {
      pred_corr <- as.logical(as.numeric(psn_opt$value[psn_opt$arg == 'predcorr']))
      if (!is.na(pred_corr)) opt$pred_corr <- pred_corr
    }
    if (!any(opt$usr_call == 'lloq')) {
      lloq <- as.numeric(psn_opt$value[psn_opt$arg == 'lloq'])
      if (length(lloq) > 0 && !is.na(lloq)) opt$lloq <- lloq
    }
    if (!any(opt$usr_call == 'uloq')) {
      uloq <- as.numeric(psn_opt$value[psn_opt$arg == 'uloq'])
      if (length(uloq) > 0 && !is.na(uloq)) opt$uloq <- uloq
    }
    
    # Get number of samples [would be better to compute and use IREP in the future]
    nsim <- ifelse(!stringr::str_detect(psn_cmd, '-sampl'), 'na',
                   stringr::str_match(psn_cmd, '-sampl[a-z]+=\\s*([^\\s]+)')[1, 2])
  } else {
    msg('PsN file `version_and_option_info.txt` not found. Using default options.', quiet)
    obs_cols <- c(id = 'ID', idv = 'TIME', dv = 'DV', pred = 'PRED')
    sim_cols <- obs_cols
  }
  if (file.exists(file_path(psn_folder, 'vpc_bins.txt')) && !any(opt$usr_call == 'bins') && psn_bins) {
    psn_bins <- readr::read_lines(file = file_path(psn_folder, 'vpc_bins.txt')) %>% 
      .[nchar(.) > 0] %>% 
      utils::tail(n = 1) %>% 
      stringr::str_replace('^.+=', '') %>% 
      {stringr::str_split(., ':')[[1]]} %>% 
      {stringr::str_split(., ',')} %>% 
      purrr::map(~as.numeric(.x))
    
    if (!any(is.na(psn_bins[[1]]))) {
      opt$bins <- psn_bins[[1]] # vpc does not handle panel based binning yet so take the first one
      msg(c('Using PsN-defined binning', ifelse(length(unique(psn_bins)) == 1 , '',
                                              '. Only a single bin_array (i.e. first) can be used by xpose.')), quiet)
    } else {
      warning('Failed to read PsN-defined binning.', call. = FALSE)
    }
  }
  
  list(obs_data = obs_data, obs_cols = obs_cols, 
       sim_data = sim_data, sim_cols = sim_cols,
       opt = opt, psn_folder = psn_folder, 
       psn_cmd = psn_cmd, nsim = nsim)
}
