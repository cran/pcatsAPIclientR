#' @title Return job results
#' @description Return job results
#' @param jobid Job ID of the previously submitted job
#' @param token Authentication token.
#' @return results
#' @export
#' @import httr
#' @import utils
#' @import jsonlite
#'
results <- function(jobid, token = NULL) {
  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }

  res <- GET(
    url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/results"),
    add_headers(headers)
  )

  if (res$status_code != 200) {
    return(NULL)
  }

  output <- jsonlite::fromJSON(httr::content(res, as = "text"))

  # output<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/results"))
  output
}
