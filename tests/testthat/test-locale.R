test_that("cpb_utf8_locale() spots a locale that cannot draw accents", {
  skip_on_cran()
  withr::with_locale(c(LC_CTYPE = "C"), expect_false(cpb_utf8_locale()))
  # only assert the positive case where a UTF-8 locale is actually available
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")) != "") {
    withr::with_locale(c(LC_CTYPE = "C.UTF-8"), expect_true(cpb_utf8_locale()))
  }
})
