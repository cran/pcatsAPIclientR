#' @title Wait while the job status is pending
#' @description Return when the job status is finished (either successfully or otherwise)
#' @param jobid Job ID of the previously submitted job
#' @param token Authentication token.
#' @return status
#' @export
#' @import httr
#' @import utils
#'
wait_for_result <- function(jobid, token = NULL) {
  status <- NULL
  while (TRUE) {
    status <- job_status(jobid, token)

    if (status != "Pending") {
      break
    }

    Sys.sleep(1)
  }
  status
}
