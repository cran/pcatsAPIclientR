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

  retry_count <- 5
  while(1) {
    output <- tryCatch({
      res <- GET(
        url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/print"),
        add_headers(headers)
      )
      if (res$status_code != 200) {
        return("")
      }
      output <- httr::content(res, as = "text")
      output
    }, error = function(e) {
      return("")  # Catch connection errors
    })
    if (output != "") break;
    
    retry_count <- retry_count - 1
    if (retry_count == 0) break;
    Sys.sleep(1)
  }

  # output <- paste(paste(readLines(paste0('https://pcats.research.cchmc.org/api/job/',jobid,'/print'),warn=FALSE),sep="\n", collapse = "\n"),"","",sep="\n")
  output
}
