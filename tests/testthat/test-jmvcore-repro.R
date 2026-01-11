
test_that("jmvcore Table initialization works", {
  skip_if_not_installed("jmvcore")
  library(jmvcore)
  print(sessionInfo())
  
  
  tryCatch({
    tc1 <- jmvcore::Table$new(
      options = list(), name="t1", title="T1", rows=0,
      columns = list(list(name="c1", title="C1", type="text"))
    )
    print("Table with text column passed initialization")

    tc2 <- jmvcore::Table$new(
      options = list(), name="t2", title="T2", rows=0,
      columns = list(list(name="c2", title="C2", type="number", format="currency"))
    )
    print("Table with currency format passed initialization")
    
    tc3 <- jmvcore::Table$new(
      options = list(), name="t3", title="T3", rows=0,
      columns = list(list(name="c3", title="C3", type="number", format="zto3"))
    )
    print("Table with zto3 format passed initialization")

  }, error = function(e) {
    fail(paste("Table initialization failed:", e$message))
  })
})
