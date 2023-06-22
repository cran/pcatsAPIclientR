#' @title Performs a data analysis for data with non-adaptive treatment(s).
#' @description Performs Bayesian's Gaussian process regression or Bayesian
#'    additive regression tree for data with non-adaptive treatment(s).
#' @param datafile File to upload (.csv or .xls)
#' @param dataref Reference to already uploaded file.
#' @param method The method to be used. "GP" for GP method and "BART" for BART method. The default value is "BART".
#' @param outcome The name of the outcome variable.
#' @param outcome.type Outcome type ("Continuous" or "Discrete"). The default value is "Continuous".
#' @param outcome.bound_censor The default value is "neither".
#'    "neither" if the outcome is not bounded or censored.
#'    "bounded" if the outcome is bounded.
#'    "censored" if the outcome is censored.
#' @param outcome.lb Putting a lower bound if the outcome is bounded.
#' @param outcome.ub Putting a upper bound if the outcome is bounded.
#' @param outcome.censor.yn Censoring variable if outcome is censored.
#' @param outcome.censor.lv lower variable of censored interval if outcome is censored.
#' @param outcome.censor.uv upper variable of censored interval if outcome is censored.
#' @param outcome.link function for outcome; the default value is "identity".
#'    "identity" if no transformation needed.
#'    "log" for log transformation.
#'    "logit" for logit transformation.
#' @param treatment The vector of the name of the treatment variables. Users can input at most two treatment variables.
#' @param x.explanatory The vector of the name of the explanatory variables.
#' @param x.confounding The vector of the name of the confounding variables.
#' @param tr.type The type of the first treatment. "Continuous" for continuous treatment and "Discrete" for categorical treatment. The default value is "Discrete".
#' @param tr.values user-defined values for the calculation of ATE if the first treatment variable is continuous
#' @param c.margin An optional vector of user-defined values of c for PrTE.
#' @param tr.hte An optional vector specifying variables which may have heterogeneous treatment effect with the first treatment variable
#' @param time Time variable.
#' @param time.value Pre-specified time exposure.
#' @param burn.num numeric; the number of MCMC 'burn-in' samples, i.e. number of MCMC to be discarded. The default value is 500.
#' @param mcmc.num numeric; the number of MCMC samples after 'burn-in'. The default value is 500.
#' @param x.categorical A vector of the name of categorical variables in data.
#' @param mi.datafile File to upload (.csv or .xls) that contains the imputed data in the model.
#' @param mi.dataref Reference to already uploaded file that contains the imputed data in the model.
#' @param sheet If \code{datafile} or \code{dataref} points to an Excel file this variable specifies which sheet to load.
#' @param mi.sheet If \code{mi.datafile} or \code{mi.dataurl} points to an Excel file this variable specifies which sheet to load.
#' @param seed Sets the seed. The default value is 5000.
#' @param token Authentication token.
#' @param use.cache Use cached results (default True).
#' @return jobid
#' @export
#' @import httr
#' @import utils
#' @importFrom methods hasArg
staticGP <- function(datafile = NULL,
                     dataref = NULL,
                     method = "BART",
                     outcome,
                     outcome.type = "Continuous",
                     outcome.bound_censor = "neither",
                     outcome.lb = NULL,
                     outcome.ub = NULL,
                     outcome.censor.yn = NULL,
                     outcome.censor.lv = NULL,
                     outcome.censor.uv = NULL,
                     outcome.link = "identity",
                     treatment,
                     x.explanatory = NULL,
                     x.confounding = NULL,
                     tr.type = "Discrete",
                     tr.values = NULL,
                     c.margin = NULL,
                     tr.hte = NULL,
                     time,
                     time.value = NULL,
                     burn.num = 500,
                     mcmc.num = 500,
                     x.categorical = NULL,
                     mi.datafile = NULL,
                     mi.dataref = NULL,
                     sheet = NULL,
                     mi.sheet = NULL,
                     seed = 5000,
                     token = NULL,
                     use.cache = NULL) {
  data <- NULL
  if (!is.null(datafile)) {
    data <- httr::upload_file(datafile)
  }
  mi.data <- NULL
  if (!is.null(mi.datafile)) {
    mi.data <- httr::upload_file(mi.datafile)
  }

  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }
  if (!hasArg(use.cache) && Sys.getenv("PCATS_USE_CACHE") != "") use.cache <- Sys.getenv("PCATS_USE_CACHE")
  if (!is.null(use.cache) && (use.cache == T || use.cache == "1")) {
    headers <- c(headers, "X-API-Cache" = "1")
  }
  if (!is.null(use.cache) && (use.cache == F || use.cache == "0")) {
    headers <- c(headers, "X-API-Cache" = "0")
  }

  res <- POST(
    url = "https://pcats.research.cchmc.org/api/staticgp",
    add_headers(headers),
    encode = "multipart",
    body = list(
      data = data,
      dataref = dataref,
      outcome = outcome,
      treatment = treatment,
      x.explanatory = x.explanatory,
      x.confounding = x.confounding,
      tr.hte = tr.hte,
      time = time,
      time.value = time.value,
      burn.num = burn.num, mcmc.num = mcmc.num,
      outcome.lb = outcome.lb,
      outcome.ub = outcome.ub,
      outcome.bound_censor = outcome.bound_censor,
      outcome.type = outcome.type,
      outcome.censor.lv = outcome.censor.lv,
      outcome.censor.uv = outcome.censor.uv,
      outcome.censor.yn = outcome.censor.yn,
      outcome.link = outcome.link,
      tr.type = tr.type,
      tr.values = tr.values,
      c.margin = c.margin,
      x.categorical = x.categorical,
      method = method,
      mi.data = mi.data,
      mi.dataref = mi.dataref,
      sheet = sheet,
      mi.sheet = mi.sheet,
      seed = seed
    )
  )

  cont <- content(res)
  jobid <- cont$jobid[[1]]
  jobid
}
