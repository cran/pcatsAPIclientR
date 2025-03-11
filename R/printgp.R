#' @title Print job results
#' @description Return formatted string with job results
#' @param jobid Job ID of the previously submitted job
#' @param token Authentication token.
#' @return formatted text
#' @export
#' @import httr
#' @import utils
#'
printgp <- function(jobid, token = NULL) {
  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }

  tryCatch({
    res <- GET(
      url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/print"),
      add_headers(headers)
    )
  }, error = function(e) {
    return(NULL)  # Catch connection errors
  })

  if (res$status_code != 200) {
    return(NULL)
  }

  output <- httr::content(res, as = "text")

  # output <- paste(paste(readLines(paste0('https://pcats.research.cchmc.org/api/job/',jobid,'/print'),warn=FALSE),sep="\n", collapse = "\n"),"","",sep="\n")
  output
}
