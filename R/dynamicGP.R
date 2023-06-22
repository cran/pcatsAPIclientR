###################### DynamicResults###################################

#' @title Performs a data analysis for data with adaptive treatments.
#' @description Performs Bayesian's Gaussian process regression or Bayesian
#'    additive regression tree for data with adaptive treatment(s).
#' @param datafile File to upload (.csv or .xls)
#' @param dataref Reference to already uploaded file.
#' @param method The method to be used. "GP" for GP method and "BART" for BART method. The default value is "BART".
#' @param stg1.outcome The name of the intermediate outcome variable for stage 1.
#' @param stg1.treatment The name of the treatment variable for stage 1.
#' @param stg1.x.explanatory A vector of the name of the explanatory variables for stage 1.
#' @param stg1.x.confounding A vector of the name of the confounding variables for stage 1.
#' @param stg1.tr.hte An optional vector specifying categorical variables which may have heterogeneous treatment effect with the treatment variable for stage 1.
#' @param stg1.outcome.bound_censor The default value is "neither".
#'    "neither" if the intermediate outcome is not bounded or censored.
#'    "bounded" if the intermediate outcome is bounded.
#'    "censored" if the intermediate outcome is censored.
#' @param stg1.outcome.lb Stage 1 lower bound if the intermediate outcome is bounded.
#' @param stg1.outcome.ub Stage 1 upper bound if the intermediate outcome is bounded.
#' @param stg1.outcome.type Intermediate outcome type ("Continuous" or "Discrete") for stage 1.
#' @param stg1.outcome.censor.yn Censoring variable if the intermediate outcome is censored.
#' @param stg1.outcome.censor.lv lower variable of censored interval if the intermediate outcome is censored.
#' @param stg1.outcome.censor.uv upper variable of censored interval if the intermediate outcome is censored.
#' @param stg1.outcome.link function for the intermediate outcome; the default value is ``identity''.
#'    "identity" if no transformation needed.
#'    "log" for log transformation.
#'    "logit" for logit transformation.
#' @param stg1.tr.values User-defined values for the calculation of ATE if the treatment variable is continuous for stage 1.
#' @param stg1.tr.type The type of treatment at stage 1. "Continuous" for continuous treatment and "Discrete" for categorical treatment. The default value is "Discrete".
#' @param stg1.time Time variable.
#' @param stg1.time.value Pre-specified time exposure.
#' @param stg1.c.margin An optional vector of user-defined values of c for PrTE at stage 1.
#' @param stg2.outcome The name of the outcome variable for stage 2.
#' @param stg2.treatment The name of the treatment variable for stage 2.
#' @param stg2.x.explanatory A vector of the name of the explanatory variables for stage 2.
#' @param stg2.x.confounding A vector of the name of the confounding variables for stage 2.
#' @param stg2.tr1.hte At stage 2, an optional vector specifying categorical variables which may have heterogeneous treatment effect with the stage 1 treatment variable
#' @param stg2.tr2.hte At stage 2, an optional vector specifying categorical variables which may have heterogeneous treatment effect with the stage 2 treatment variable
#' @param stg2.outcome.bound_censor The default value is "neither".
#'    "neither" if the intermediate outcome is not bounded or censored.
#'    "bounded" if the intermediate outcome is bounded.
#'    "censored" if the intermediate outcome is censored.
#' @param stg2.outcome.lb Stage 2 lower bound if the outcome is bounded.
#' @param stg2.outcome.ub Stage 2 upper bound if the outcome is bounded.
#' @param stg2.outcome.type Outcome type ("Continuous" or "Discrete") for stage 2.
#' @param stg2.outcome.censor.yn Censoring variable if the outcome is censored.
#' @param stg2.outcome.censor.lv lower variable of censored interval if the outcome is censored.
#' @param stg2.outcome.censor.uv upper variable of censored interval if the outcome is censored.
#' @param stg2.outcome.link function for the outcome; the default value is ``identity''.
#'    "identity" if no transformation needed.
#'    "log" for log transformation.
#'    "logit" for logit transformation.
#' @param stg2.tr.values User-defined values for the calculation of ATE if the treatment variable is continuous for stage 2.
#' @param stg2.tr.type The type of treatment at stage 2. "Continuous" for continuous treatment and "Discrete" for categorical treatment. The default value is "Discrete".
#' @param stg2.time Time variable.
#' @param stg2.time.value Pre-specified time exposure.
#' @param stg2.c.margin An optional vector of user-defined values of c for PrTE at stage 2.
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
#'
dynamicGP <- function(datafile = NULL,
                      dataref = NULL,
                      method = "BART",
                      # stage 1
                      stg1.outcome,
                      stg1.treatment,
                      stg1.x.explanatory = NULL,
                      stg1.x.confounding = NULL,
                      stg1.tr.hte = NULL,
                      stg1.tr.values = NULL,
                      stg1.tr.type = "Discrete",
                      stg1.time,
                      stg1.time.value = NULL,
                      stg1.outcome.type = "Continuous",
                      stg1.outcome.bound_censor = "neither",
                      stg1.outcome.lb = NULL,
                      stg1.outcome.ub = NULL,
                      stg1.outcome.censor.lv = NULL,
                      stg1.outcome.censor.uv = NULL,
                      stg1.outcome.censor.yn = NULL,
                      stg1.outcome.link = "identity",
                      stg1.c.margin = NULL,
                      # stage 2
                      stg2.outcome,
                      stg2.treatment,
                      stg2.x.explanatory = NULL,
                      stg2.x.confounding = NULL,
                      stg2.tr1.hte = NULL,
                      stg2.tr2.hte = NULL,
                      stg2.tr.values = NULL,
                      stg2.tr.type = "Discrete",
                      stg2.time,
                      stg2.time.value = NULL,
                      stg2.outcome.type = "Continuous",
                      stg2.outcome.bound_censor = "neither",
                      stg2.outcome.lb = NULL,
                      stg2.outcome.ub = NULL,
                      stg2.outcome.censor.lv = NULL,
                      stg2.outcome.censor.uv = NULL,
                      stg2.outcome.censor.yn = NULL,
                      stg2.outcome.link = "identity",
                      stg2.c.margin = NULL,
                      # common parameters
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
    url = "https://pcats.research.cchmc.org/api/dynamicgp",
    add_headers(headers),
    encode = "multipart",
    body = list(
      data = data,
      dataref = dataref,
      stg1.outcome = stg1.outcome, stg1.treatment = stg1.treatment,
      stg1.x.explanatory = stg1.x.explanatory, stg1.x.confounding = stg1.x.confounding,
      stg1.tr.hte = stg1.tr.hte,
      stg1.tr.values = stg1.tr.values,
      stg1.tr.type = stg1.tr.type,
      stg1.time = stg1.time,
      stg1.time.value = stg1.time.value,
      stg1.outcome.type = stg1.outcome.type,
      stg1.outcome.bound_censor = stg1.outcome.bound_censor,
      stg1.outcome.lb = stg1.outcome.lb, stg1.outcome.ub = stg1.outcome.ub,
      stg1.outcome.censor.lv = stg1.outcome.censor.lv, stg1.outcome.censor.uv = stg1.outcome.censor.uv,
      stg1.outcome.censor.yn = stg1.outcome.censor.yn, stg1.outcome.link = stg1.outcome.link,
      stg1.c.margin = stg1.c.margin,

      # stage 2
      stg2.outcome = stg2.outcome, stg2.treatment = stg2.treatment,
      stg2.x.explanatory = stg2.x.explanatory, stg2.x.confounding = stg2.x.confounding,
      stg2.tr1.hte = stg2.tr1.hte,
      stg2.tr2.hte = stg2.tr2.hte,
      stg2.tr.values = stg2.tr.values,
      stg2.tr.type = stg2.tr.type,
      stg2.time = stg2.time,
      stg2.time.value = stg2.time.value,
      stg2.outcome.type = stg2.outcome.type,
      stg2.outcome.bound_censor = stg2.outcome.bound_censor,
      stg2.outcome.lb = stg2.outcome.lb, stg2.outcome.ub = stg2.outcome.ub,
      stg2.outcome.censor.lv = stg2.outcome.censor.lv, stg2.outcome.censor.uv = stg2.outcome.censor.uv,
      stg2.outcome.censor.yn = stg2.outcome.censor.yn, stg2.outcome.link = stg2.outcome.link,
      stg2.c.margin = stg2.c.margin,
      burn.num = burn.num, mcmc.num = mcmc.num,
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
