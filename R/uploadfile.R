#' @title Upload a file
#' @description Upload a file
#' @param filename Filename of a file to upload
#' @param token Authentication token.
#' @return Backend filename reference
#' @export
#' @import httr
#' @import utils
#'
uploadfile <- function(filename, token = NULL) {
  headers <- c()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }

  retry_count <- 5
  while(1) {
    jobid = tryCatch({
      res <- POST(
        url = paste0("https://pcats.research.cchmc.org/api/uploadfile"),
        add_headers(headers),
        encode = "multipart",
        body = list(data = upload_file(filename))
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
