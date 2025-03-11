#' @title Return plot URL
#' @description Return plot URL
#' @param jobid Job ID of the previously submitted job
#' @param plottype Plot type
#' @param token Authentication token.
#' @return url
#' @export
#' @import httr
#' @import utils
#'
ploturl <- function(jobid, plottype = "", token = NULL) {
  if (plottype != "") {
    plottype <- paste0("/", plottype)
  }

  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }

  tryCatch({
    res <- GET(
      url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/ploturl"),
      add_headers(headers)
    )
  }, error = function(e) {
    return(NULL)  # Catch connection errors
  })

  if (res$status_code != 200) {
    return(NULL)
  }

  url <- jsonlite::fromJSON(httr::content(res, as = "text"))

  # url<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/ploturl"))
  output <- paste0(url$url, plottype)
  output
}
