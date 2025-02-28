context("multi_covar_pca")

test_that("multi_covar_pca agrees with ezpca", {
  ezp <- ezpca(M, pheno, name=NA, shape="grp", color="covar_num", manual.shape = 1:2, plot=FALSE)
  mcp <- multi_covar_pca(M, pheno, name="covar_pca", grp.var="grp", covars = c("tissue", "covar_num"),
                         manual.shape = 1:2, plot=FALSE)
  expect_equal(ezp, mcp[["covar_num"]])
})
