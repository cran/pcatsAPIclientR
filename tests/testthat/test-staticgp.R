context("staticgp submission process")

test_that("Function to submit staticgp", {
jobid <- staticGP(datafile="example1.csv",
        outcome="Y",
        treatment="A",
        x.explanatory="X",
        x.confounding="X",
        burn.num=500, mcmc.num=500,
        outcome.type="Continuous",
        method="GP",
        tr.type="Discrete",
        pr.values="0,1,2")
expect_true(grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$",jobid,ignore.case = TRUE))
})

test_that("Function can wait for result", {
 jobid <- staticGP(datafile="example1.csv",
        outcome="Y",
        treatment="A",
        x.explanatory="X",
        x.confounding="X",
        burn.num=500, mcmc.num=500,
        outcome.type="Continuous",
        method="GP",
        tr.type="Discrete",
        pr.values="0,1,2")
  if (grepl("^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$",jobid,ignore.case = TRUE))
  {
    status <- pcatsAPIclientR::wait_for_result(jobid)
  } else {
    status <- "Failed"
  }
  expect_true(status == "Done")
})

# if (status=="Done") {
   # cat(pcatsAPIclientR::printgp(jobid))
# }
