
#' Mean and variance of tranformed parameters
#' 
#' @description The function calculates the mean and variance of function of parameters
#'
#' @param formula A formula describing the transformation.
#' @param mu Vector of mean values for the variables in formula.
#' @param sigma Covariance matrix for the variables in formula. 
#' @param method A string indicating which method to use for the calculation.
#' @param samples Number of Monte-Carlo samples for method "mc"
#'
#' @return A list with entries `mean`, `variance` and `rse`
#' @examples
#' # Mean, variance and rse for sqrt of variable with mean 1 and variance 0.01
#' transform_prm(~sqrt(a), mu = c(a = 1), sigma = c(a = 0.01))
#' 
#' @keywords internal
#' @export
transform_prm <- function(formula, mu, sigma, method = 'delta', samples = 1000) {
  if (!rlang::is_formula(formula)) {
    stop('Argument `formula` needs to be a formula', call. = FALSE)
  }
  variables <- all.vars(formula)
  if (!all(variables %in% names(mu))) {
    stop('A mean value needs to be supplied for each variable.', call. = FALSE)
  }
  
  #  if(NROW(sigma)!=length(variables) || NCOL(sigma)!=length(variables)) {
  #   stop('Argument `sigma` needs to be a matrix with size equal to the number of variables.', call. = FALSE)
  # }
  
  ex <- formula[[2]]
  if (NROW(sigma) == 1) { 
    tmp <- diag(sigma, NROW(sigma))
    rownames(tmp) <- colnames(tmp) <- names(sigma)
    sigma <- tmp
  }
  
  if (!all(variables %in% rownames(sigma))) {
    stop('A variance value needs to be supplied for each variable.', call. = FALSE)
  }
  
  # ensure mu and sigma have same order as variable list
  mu <- mu[variables, drop = FALSE]
  sigma <- sigma[variables, variables, drop = FALSE]
  
  if (method == 'delta') {
    t_mean <- stats::deriv(ex, variables) %>% eval(envir = as.list(mu))
    grad <- attr(t_mean, 'gradient')
    attr(t_mean, 'gradient') <- NULL 
    t_var <- {grad %*% sigma %*% t(grad)}[1, 1]
    return(list(mean = t_mean, 
                variance = t_var, 
                rse = sqrt(t_var)/t_mean))
  } else if (method == 'mc') {
    if (!requireNamespace('mvtnorm', quietly = TRUE)) {
      stop('Package `mvtnorm` needs to installed for this method to work.', call. = FALSE)
    }
    mvtnorm::rmvnorm(samples, mean = mu, sigma = sigma) %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(f = eval(ex, .)) %>% 
      dplyr::summarize(mean     = mean(.$f, na.rm = TRUE), 
                       variance = stats::var(.$f, na.rm = TRUE)) %>%
      dplyr::mutate(rse = sqrt(.$variance)/.$mean) %>% 
      purrr::transpose() %>% 
      dplyr::first() 
  } else {
    stop('Method ', method, ' is not available.', call. = FALSE)
  }
}

