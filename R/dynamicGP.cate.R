#' @title Get conditional average treatment effect for data with two time points.
#' @description Estimate the conditional average treatment effect of user-specified treatment groups.
#' @details The contrast of potential outcomes for the reference group and the treatment group is estimated at a list of x values if x is not a factor. If x is a factor, the conditional average treatment effect is estimated at each value of levels of x.
#' @note The conditional average treatment effect is estimated based on the sample data. The observations with missing covariates in the model are excluded. For the unspecified variables in the model, the observed data is used to estimate the conditional average treatment effect.
#' @param jobid job id of the "dynamicGP".
#' @param x The name of variable which may have the heterogeneous treatment effect. x should be a categorical variable.
#' @param control.tr A vector of the values of the treatment variables at all stages as the reference group.
#' @param treat.tr A vector of the values of the treatment variables at all stages compared to the reference group.
#' @param c.margin An optional vector of user-defined values of c for PrCTE.
#' @param token Authentication token.
#' @param use.cache Use cached results (default True).
#' @return jobid
#' @export
#' @importFrom methods hasArg
#'
dynamicGP.cate <- function(jobid,
                           x,
                           control.tr,
                           treat.tr,
                           c.margin = NULL,
                           token = NULL,
                           use.cache = NULL) {
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

  retry_count <- 5
  while(1) {
    jobid <- tryCatch({
      res <- POST(
        url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/dynamicgp.cate"),
        add_headers(headers),
        encode = "multipart",
        body = list(
          x = x,
          control.tr = control.tr,
          treat.tr = treat.tr,
          c.margin = c.margin
        )
      )
      cont <- content(res)
      jobid <- cont$jobid[[1]]
      jobid
      }, error = function(e) {
      return("")  # Catch connection errors
    })
    if (jobid != "") break;

    retry_count <- retry_count - 1
    if (retry_count == 0) break;
    Sys.sleep(5-retry_count)
  }
  jobid
}
