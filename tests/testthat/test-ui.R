test_that("should_ask_confirmation", {
  neg <- list(
    # install direct refs, no deps: no
    data_frame(
      direct = TRUE,
      lib_status = "new"
    ),

    # install direct refs, deps are not updated: no
    data_frame(
      direct = c(TRUE, FALSE, FALSE),
      lib_status = c("new", "no-update", "no-update")
    ),

    # install direct refs, deps are current: no
    data_frame(
      direct = c(TRUE, FALSE, FALSE),
      lib_status = c("new", "no-update", "current")
    ),

    # install direct ref, newly install dep: no
    data_frame(
      direct = c(TRUE, FALSE),
      lib_status = c("new", "new")
    ),

    # keep direct ref, newly install dep: no
    data_frame(
      direct = c(TRUE, FALSE),
      lib_status = c("current", "new")
    ),

    # keep direct ref, update dep: no
    data_frame(
      direct = c(TRUE, FALSE),
      lib_status = c("current", "new")
    )
  )

  for (x in neg) expect_false(should_ask_confirmation(x))

  pos <- list(
    # update direct ref: yes
    data_frame(
      direct = TRUE,
      lib_status = "update"
    ),

    # install direct ref, update deps: yes
    data_frame(
      direct = c(TRUE, FALSE),
      lib_status = c("new", "update")
    )
  )

  for (x in pos) expect_true(should_ask_confirmation(x))
})
