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

  retry_count <- 5
  while(1) {
    output <- tryCatch({
      res <- GET(
        url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/ploturl"),
        add_headers(headers)
      )
      if (res$status_code != 200) {
        return(NULL)
      }
  
      url <- jsonlite::fromJSON(httr::content(res, as = "text"))
      output <- paste0(url$url, plottype)
      output
    }, error = function(e) {
      return(NULL)  # Catch connection errors
    })
    if (output != "") break;

    retry_count <- retry_count - 1
    if (retry_count == 0) break;
    Sys.sleep(1)
  }

  # url<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/ploturl"))
  output
}
