context("staticgp submission process")

run_staticGP_example <- function() {
  jobid <- staticGP(
    # datafile = "tests/testthat/example1.csv",
    datafile = "example1.csv",
    outcome = "Y",
    treatment = "A",
    time = "Time",
    x.explanatory = "X",
    x.confounding = "X",
    burn.num = 500, mcmc.num = 500,
    outcome.type = "Continuous",
    method = "GP",
    tr.type = "Discrete",
    c.margin = "0,1,2"
    # , use.cache = "0"
  )
  jobid
}

test_that("Function to submit staticgp", {
  jobid <- run_staticGP_example()
  expect_true(grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$", jobid, ignore.case = TRUE))
})

test_that("Function can wait for result", {
  jobid <- run_staticGP_example()
  if (grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$", jobid, ignore.case = TRUE)) {
    status <- pcatsAPIclientR::wait_for_result(jobid)
  } else {
    status <- "Failed"
  }
  expect_true(status == "Done")
})

test_that("Function get printed results", {
  jobid <- run_staticGP_example()
  if (grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$", jobid, ignore.case = TRUE)) {
    status <- pcatsAPIclientR::wait_for_result(jobid)
  } else {
    status <- "Failed"
  }
  if (status != "Done") {
    line3 <- ""
  } else {
    result <- pcatsAPIclientR::printgp(jobid)
    result_lines <- strsplit(result, "\n")
    line3 <- result_lines[[1]][3]
  }

  expect_equal(line3, " A=0 - A=1     -5.048 0.198 -5.415 -4.662         0         0         0")
})

test_that("Function getresults", {
  jobid <- run_staticGP_example()
  if (grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$", jobid, ignore.case = TRUE)) {
    status <- pcatsAPIclientR::wait_for_result(jobid)
  } else {
    status <- "Failed"
  }
  if (status != "Done") {
    line <- ""
  } else {
    result <- pcatsAPIclientR::results(jobid)
    line <- result$staticGP$ate$Contrast
  }

  expect_equal(line, "A=0 - A=1")
})
