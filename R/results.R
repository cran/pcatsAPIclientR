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

  retry_count <- 5
  while(1) {
    output <- tryCatch({
      res <- GET(
        url = paste0("https://pcats.research.cchmc.org/api/job/", jobid, "/results"),
        add_headers(headers)
      )
      if (res$status_code != 200) {
        return(NULL)
      }
  
      output <- jsonlite::fromJSON(httr::content(res, as = "text"))
      output
    }, error = function(e) {
      return(NULL)  # Catch connection errors
    })
    if (!is.null(output)) break;
    
    retry_count <- retry_count - 1
    if (retry_count == 0) break;
    Sys.sleep(1)
  }


  # output<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/results"))
  output
}
