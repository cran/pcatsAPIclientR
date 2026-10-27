# These tests submit jobs to the live PCATS API, so skip them on CRAN and
# whenever the API is not reachable.
skip_if_api_unavailable <- function() {
  skip_on_cran()
  skip_if_offline("pcats.research.cchmc.org")

  status <- tryCatch(
    httr::status_code(httr::GET("https://pcats.research.cchmc.org/api/__docs__/",
                                httr::timeout(10))),
    error = function(e) NA
  )
  if (is.na(status) || status != 200) {
    skip("PCATS API is not available")
  }
}
