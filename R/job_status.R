#' @title Return job status
#' @description Return status of the previously submitted job
#' @param jobid Job ID of the previously submitted job
#' @param token Authentication token.
#' @return status
#' @export
#' @import httr
#' @import utils
#' @import jsonlite
#'
job_status <- function(jobid, token = NULL) {
  if (is.null(jobid)) {
    stop("jobid is null")
  }

  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }

  retry_count <- 5
  while(1) {
    status <- tryCatch({
      res <- GET(
        url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/status"),
        add_headers(headers)
      )
      if (res$status_code != 200) {
        return(NULL)
      }
      status <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "utf-8"))
      status
    }, error = function(e) {
      return(NULL)  # Catch connection errors
    })
    if (!is.null(status)) break;

    retry_count <- retry_count - 1
    if (retry_count == 0) break;
    Sys.sleep(5-retry_count)
  }
  unlist(status)
}
