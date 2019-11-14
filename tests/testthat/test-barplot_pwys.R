context("barplot of significance per pathway")

test_that("direction properly specified", {
  expect_error(barplot_pwys(rc))
  expect_error(barplot_pwys(rc, prefix.v = "First3"))
  expect_error(barplot_pwys(tab=rc, direction = "Up"))

  expect_silent(barplot_pwys(tab=rc, prefix.v = "First3", direction = "Up"))
  expect_silent(barplot_pwys(tab=rc, prefix.v = "Last3", direction = "Up"))
  expect_message(barplot_pwys(tab=rc, prefix.v = "Last3vsFirst3", direction = "Down"))
})

test_that("barplot object non-vdiffr", {
  bar_plot<-barplot_pwys(tab=rc, prefix.v = "First3", direction = "Up")
  expect_equal(bar_plot$data["pwy1","Direction"] , rc["pwy1","First3.Direction"])	
  expect_equal(bar_plot$data["pwy1","neglog10p"] , log10(rc["pwy1" ,"First3.p"])*-1)	
})

test_that("red color vdiffr", {
  #verified that bar is of red color 
  redbar <- function() barplot_pwys(tab=rc, prefix.v = "First3", direction = "Up")
  vdiffr::expect_doppelganger(title="red bar", fig=redbar)
})
