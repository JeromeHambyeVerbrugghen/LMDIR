
test_that("Zij works as expected", {
  # Set up
  PN <- 9999 # Positive Number
  ANS <- 42 # the answer (modulo the sign)
  # Test with bogus input data.
  expect_error(Zij(v_0i1 = "bad data", v_Ti1 = ANS, X_0ij = 0, X_Tij = PN),
               "Unknown conditions for v_0i1, v_Ti1, X_0ij, and X_Tij in Zij")
  # Test degenerate cases.
  #
  # The cases below are taken from Table 2, p. 492 in
  # B. Ang, F. Zhang, and K.-H. Choi.
  # Factorizing changes in energy and environmental indicators through decomposition.
  # Energy, 23(6):489–495, Jun 1998.
  #
  # Case 1
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = ANS, X_0ij = 0, X_Tij = PN) %>% unlist() %>% unique(), ANS)
  # Case 2
  expect_equal(Zij(v_0i1 = ANS, v_Ti1 = 0, X_0ij = PN, X_Tij = 0) %>% unlist() %>% unique(), -ANS)
  # Case 3
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = PN, X_0ij = PN, X_Tij = PN) %>% unlist() %>% unique(), 0)
  # Case 4
  expect_equal(Zij(v_0i1 = PN, v_Ti1 = 0, X_0ij = PN, X_Tij = PN) %>% unlist() %>% unique(), 0)
  # Case 5
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = 0, X_0ij = PN, X_Tij = PN) %>% unlist() %>% unique(), 0)
  # Case 6
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = 0, X_0ij = 0, X_Tij = 0) %>% unlist() %>% unique(), 0)
  # Case 7
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = 0, X_0ij = PN, X_Tij = 0) %>% unlist() %>% unique(), 0)
  # Case 8
  expect_equal(Zij(v_0i1 = 0, v_Ti1 = 0, X_0ij = 0, X_Tij = PN) %>% unlist() %>% unique(), 0)

  simple <- create_simple_LMDI()

  X_0 <- simple$X[[1]]
  X_T <- simple$X[[2]]

  # Values tested below come from data-raw/LMDI_testing.xslx
  expect_equal(Zij(1, 1, X_0 = X_0, X_T = X_T)$Z_I, 50.47438029)
  expect_equal(Zij(1, 2, X_0 = X_0, X_T = X_T)$Z_I, -25.23719014)
  expect_equal(Zij(1, 3, X_0 = X_0, X_T = X_T)$Z_I, 14.76280986)
  expect_equal(Zij(2, 1, X_0 = X_0, X_T = X_T)$Z_I, 19.31568569)
  expect_equal(Zij(2, 2, X_0 = X_0, X_T = X_T)$Z_I, 15.78206435)
  expect_equal(Zij(2, 3, X_0 = X_0, X_T = X_T)$Z_I, 24.90224996)

  expect_equal(Zij(1, 1, X_0 = X_0, X_T = X_T)$Z_II, 49.65937274)
  expect_equal(Zij(1, 2, X_0 = X_0, X_T = X_T)$Z_II, -24.82968637)
  expect_equal(Zij(1, 3, X_0 = X_0, X_T = X_T)$Z_II, 14.52443543)
  expect_equal(Zij(2, 1, X_0 = X_0, X_T = X_T)$Z_II, 19.52361203)
  expect_equal(Zij(2, 2, X_0 = X_0, X_T = X_T)$Z_II, 15.95195254)
  expect_equal(Zij(2, 3, X_0 = X_0, X_T = X_T)$Z_II, 25.17031363)
})

test_that("Z_byname works as expected", {
  simple <- create_simple_LMDI()

  X_T <- simple$X[[2]]
  X_0 <- simple$X[[1]]

  # Values tested below come from data-raw/LMDI_testing.xslx
  ZI_1 <- matrix(c(50.47438029, -25.23719014, 14.76280986,
                   19.31568569, 15.78206435, 24.90224996), byrow = TRUE, nrow = 2, ncol = 3,
                 dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZI_2 <- matrix(c(42.96021467, -56.79031473, 17.83010006,
                   0, 22.47128816, 32.52871184), byrow = TRUE, nrow = 2, ncol = 3,
                 dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZI_3 <- matrix(c(12.6549815, -39.30996299, 12.6549815,
                   41.35566084, 30.28867832, 41.35566084), byrow = TRUE, nrow = 2, ncol = 3,
                 dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")

  ZII_1 <- matrix(c(49.65937274, -24.82968637, 14.52443543,
                    19.52361203, 15.95195254, 25.17031363), byrow = TRUE, nrow = 2, ncol = 3,
                  dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZII_2 <- matrix(c(43.25676087, -57.18232749, 17.95317786,
                    0, 22.46000707, 32.51238169), byrow = TRUE, nrow = 2, ncol = 3,
                  dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZII_3 <- matrix(c(12.96920685, -40.28603609, 12.96920685,
                    41.48288344, 30.38185551,	41.48288344), byrow = TRUE, nrow = 2, ncol = 3,
                  dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")


  expect_equal(Z_byname(X_0 = X_0, X_T = X_T)$Z_I, ZI_1)
  expect_equal(Z_byname(X_0 = X_0, X_T = X_T)$Z_II, ZII_1)
  expect_equal(purrr::map(Z_byname(X_0 = simple$X[1:3], X_T = simple$X[2:4]), "Z_I"),
               list(ZI_1, ZI_2, ZI_3))
  expect_equal(purrr::map(Z_byname(X_0 = simple$X[1:3], X_T = simple$X[2:4]), "Z_II"),
               list(ZII_1, ZII_2, ZII_3))

  # Now try in the context of a data frame.
  simple2 <- simple %>%
    dplyr::mutate(
      X_0 = list(simple$X[[1]], simple$X[[2]], simple$X[[3]], NULL,
                 simple$X[[5]], simple$X[[6]], simple$X[[7]], NULL),
      X_T = list(simple$X[[2]], simple$X[[3]], simple$X[[4]], NULL,
                 simple$X[[6]], simple$X[[7]], simple$X[[8]], NULL)
    ) %>%
    dplyr::filter(Year != 1974) %>%
    dplyr::mutate(
      Z = Z_byname(X_0 = X_0, X_T = X_T)
    )
  expect_equal(purrr::map(simple2$Z, "Z_I"), list(ZI_1, ZI_2, ZI_3, ZI_1, ZI_2, ZI_3))
  expect_equal(purrr::map(simple2$Z, "Z_II"), list(ZII_1, ZII_2, ZII_3, ZII_1, ZII_2, ZII_3))

  # Try with some degenerate values in the X matrix.
  # When a 0 is present, the row product is zero, making the logarithms blow up.
  # These are the conditions under which we need to refer to
  # Table 2, p. 492 of
  # B.W. Ang and F.Q. Zhang and Ki-Hong Choi, 1998,
  # Factorizing changes in energy and environmental indicators through decomposition,
  # Energy, Volume 23, Number 6, pp. 489-495.
  # We employ the method suggested by
  # R. Wood and M. Lenzen. 2006.
  # Zero-value problems of the logarithmic mean divisia index decomposition method.
  # Energy Policy, volume 34, number 12, pp. 1326–1331.

  # First, set one of the values in X_0 to 0.
  X_0_2 <- X_0
  X_0_2[[1, 1]] <- 0
  # In this situation, the second row of Z remains same as Z_1 above.
  # However, with X_0_11 = 0, we also get v_0_11 = 0.
  # For Z_11, we have Case 0, and Z_11 = v_T_11 = 60.
  # For Z_12 and Z_13, we have Case 3, and both are 0.
  ZI_expected_1 <- matrix(c(60, 0, 0,
                            19.31568569, 15.78206435, 24.90224996), byrow = TRUE, nrow = 2, ncol = 3,
                          dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZII_expected_1 <- matrix(c(60, 0, 0,
                             # VALUES BELOW COME FROM LMDI_testing.xlsx FILE, AND HAVE BEEN COMPUTED MANUALLY
                             24.37368163, 19.91474794, 31.42314086), byrow = TRUE, nrow = 2, ncol = 3,
                           dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  expect_equal(Z_byname(X_0 = X_0_2, X_T = X_T)$Z_I, ZI_expected_1)
  expect_equal(Z_byname(X_0 = X_0_2, X_T = X_T)$Z_II, ZII_expected_1)

  X_T_2 <- X_T
  X_T_2[[2, 3]] <- 0
  # In this situation, the first row of Z remains same as Z_1 above.
  # However, with X_T_23 = 0, we also get v_T_21 = 0.
  # For Z_21 and Z_22, we have Case 4, and both are 0.
  # For Z_23, we have Case 2, and we obtain Z_23 = -v_0_21 = -60.
  ZI_expected_2 <- matrix(c(50.47438029, -25.23719014, 14.76280986,
                            0, 0, -60), byrow = TRUE, nrow = 2, ncol = 3,
                          dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  ZII_expected_2 <- matrix(c(96.37683359, -48.18841679,	28.18841679,
                             0, 0, -60), byrow = TRUE, nrow = 2, ncol = 3,
                           dimnames = list(c("subsubcat 1", "subsubcat 2"), c("factor 1", "factor 2", "factor 3"))) %>%
    matsbyname::setrowtype("subsubcat") %>% matsbyname::setcoltype("factor")
  expect_equal(Z_byname(X_0 = X_0, X_T = X_T_2)$Z_I, ZI_expected_2)
  expect_equal(Z_byname(X_0 = X_0, X_T = X_T_2)$Z_II, ZII_expected_2)

})


test_that("errors are given when grouping errors are present", {
  # Verify that grouping on time fails.
  expect_error(create_simple_LMDI() %>% dplyr::group_by(Country, Year) %>% lmdi(),
               "'Year' is a grouping variable, but you can't group on time in argument .lmdidata of collapse_to_matrices.")
})


test_that("Preserving grouping works as expected", {
  res <- create_simple_LMDI() %>%
    dplyr::mutate(
      groupingcol = paste(Country, "group")
    ) %>%
    dplyr::group_by(Country, groupingcol) %>%
    lmdi()
  expect_equal(group_vars(res), c("Country", "groupingcol"))
})


test_that("First row contains 0s and 1s", {
  res <- create_simple_LMDI() %>%
    dplyr::group_by(Country) %>%
    lmdi()
  expect_equal(res$dV_agg[[1]], 0)
  expect_equal(res$D_agg[[1]], 1)
  expect_equal(res$dV_agg_cum[[1]], 0)
  expect_equal(res$D_agg_cum[[1]], 1)
  dV0 <- matrix(c(0, 0, 0), nrow = 3, ncol = 1,
                dimnames = list(c("factor 1", "factor 2", "factor 3"), c("subsubcat"))) %>%
    matsbyname::setrowtype("factor") %>% matsbyname::setcoltype("subsubcat")
  D0 <- matrix(c(1, 1, 1), nrow = 3, ncol = 1,
               dimnames = list(c("factor 1", "factor 2", "factor 3"), c("subsubcat"))) %>%
    matsbyname::setrowtype("factor") %>% matsbyname::setcoltype("subsubcat")
  expect_equal(res$dV_I[[1]], dV0)
  expect_equal(res$D_I[[1]], D0)
  expect_equal(res$dV_cum_I[[1]], dV0)
  expect_equal(res$D_cum_I[[1]], D0)
  expect_equal(res$dV_II[[1]], dV0)
  expect_equal(res$D_II[[1]], D0)
  expect_equal(res$dV_cum_II[[1]], dV0)
  expect_equal(res$D_cum_II[[1]], D0)
})


test_that("fillrow option works as expected on Z_byname", {
  # Create X_0 and X_T matrices that have different rows.
  # This example comes from Ghana's energy LMDI in the years 2003 and 2004.
  dn <- list(c("HTH.600.C - Electric heaters", "KE - Fans"),
             c("E.ktoe", "eta_ij", "phi_i", "phi_ij"))
  X_0 <- matrix(c(7909.898576, 0.168054745, 0.521283202, 0.009258785,
                  7909.898576, 0.072489945, 0.078700891, 0.036152202), byrow = TRUE, nrow = 2, ncol = 4,
                dimnames = dn) %>%
    matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors")
  X_T <- matrix(c(7962.921168, 0.101321321, 0.059104816, 0.036042366), byrow = TRUE, nrow = 1, ncol = 4,
                dimnames = list("KE - Fans", dn[[2]])) %>%
    matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors")
  # Z1 should be a 2-row matrix formed by assuming small numbers for all of the missing values.
  Z1_I <- Z_byname(X_0 = X_0, X_T = X_T)$Z_I
  expect_equal(nrow(Z1_I), 2)
  expect_equal(rownames(Z1_I), c("HTH.600.C - Electric heaters", "KE - Fans"))
  Z1_II <- Z_byname(X_0 = X_0, X_T = X_T)$Z_II
  expect_equal(nrow(Z1_II), 2)
  expect_equal(rownames(Z1_II), c("HTH.600.C - Electric heaters", "KE - Fans"))
  expect_equal(Z1_I, matrix(c(-2.185092, -1.450439688, -1.527733412, -1.252513979,
                              0.011188553, 0.56076961, -0.479535332, -0.005095744),
                            byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)
  expect_equal(Z1_II, matrix(c(-2.22691886, -1.47820389, -1.55697716,	-1.27648950,
                               0.02692330, 1.34939442, -1.15391827,	-0.01226202),
                             byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)
  # Now try with a fillrow argument.
  # The following fillrow value sets ONLY the allocation from subcategory to subsubcategory to 0.
  # Doing so kicks the algorithm into another state compared to the previous example.
  # In this one, we go to case 2 of Table 2, p. 492 in Ang et al. 1998.
  # These conditions are what you find when an energy type disappears at a later year.
  # In GH, HTH.600.C was present in 2003 but disappeared in 2004 due to
  # shutdown of the VALCO smelters.
  fr <- matrix(c(42, 42, 42, 0), nrow = 1, ncol = 4,
               dimnames = list("row", c("E.ktoe", "eta_ij", "phi_i", "phi_ij"))) %>%
    matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors")
  Z2_I <- Z_byname(X_0 = X_0, X_T = X_T, fillrow = fr)$Z_I
  Z2_II <- Z_byname(X_0 = X_0, X_T = X_T, fillrow = fr)$Z_II
  expect_equal(Z2_I, matrix(c(0, 0, 0, -6.415779079,
                              0.011188553, 0.56076961, -0.479535332, -0.005095744),
                            byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)
  expect_equal(Z2_II, matrix(c(0, 0, 0, -6.41577908,
                               0.02738821, 1.37269571, -1.17384409,	-0.01247376),
                             byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)
  # If we switch the order of X_0 and X_T, we kick to case 1 of of Table 2, p. 492 in Ang et al. 1998.
  # These conditions are what you find when an energy type turns appears in a subsequent year.
  Z3_I <- Z_byname(X_0 = X_T, X_T = X_0, fillrow = fr)$Z_I
  Z3_II <- Z_byname(X_0 = X_T, X_T = X_0, fillrow = fr)$Z_II
  expect_equal(Z3_I, matrix(c(0, 0, 0, 6.415779079,
                              -0.011188553, -0.56076961, 0.479535332, 0.005095744),
                            byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)
  expect_equal(Z3_II, matrix(c(0, 0, 0,	6.41577908,
                               -0.02738822,	-1.37269571, 1.17384409, 0.01247376),
                             byrow = TRUE, nrow = 2, ncol = 4, dimnames = dn) %>%
                 matsbyname::setrowtype("categories") %>% matsbyname::setcoltype("factors"),
               tolerance = 1e-6)

  # Ensure that fillrow works properly from the lmdi method.
  # First using the small values approach.
  DF1 <- data.frame(Year = c(2003, 2004))
  DF1$X <- list(X_0, X_T)
  res1 <- lmdi(DF1, time = "Year", X = "X")
  expect_equal(res1$dV_agg[[1]], 0)
  expect_equal(res1$dV_agg[[2]], -6.328451992, tolerance = 1e-6)
  expect_equal(res1$dV_I[[1]], matrix(0, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res1$dV_I[[2]], matrix(c(-2.173903446, -0.889670079, -2.007268743, -1.257609724),
                                      nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"),
               tolerance = 1e-6)
  expect_equal(res1$dV_II[[1]], matrix(0, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res1$dV_II[[2]], matrix(c(-2.19999555, -0.12880948, -2.71089544, -1.28875152),
                                       nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"),
               tolerance = 1e-6)
  expect_equal(res1$D_agg[[1]], 1)
  expect_equal(res1$D_agg[[2]], 0.213582281)
  expect_equal(res1$D_I[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res1$D_I[[2]], matrix(c(0.588433187, 0.804912278, 0.612844653, 0.7358158),
                                     nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res1$D_I[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res1$D_II[[2]], matrix(c(0.58469983,	0.96906733,	0.51618853,	0.73024729),
                                      nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))

  # Now using a fillrow.
  DF2 <- data.frame(Year = c(2003, 2004))
  DF2$X <- list(X_0, X_T)
  expect_warning(res2 <- lmdi(DF2, time = "Year", X = "X", fillrow = list(fr)))
  expect_equal(res2$dV_agg[[1]], 0)
  expect_equal(res2$dV_agg[[2]], -6.328451992, tolerance = 1e-6)
  expect_equal(res2$dV_I[[1]], matrix(0, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res2$dV_I[[2]], matrix(c(0.011188553, 0.56076961, -0.479535332, -6.420874823),
                                      nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"),
               tolerance = 1e-6)
  expect_equal(res2$dV_II[[1]], matrix(0, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res2$dV_II[[2]], matrix(c(0.02738822, 1.37269571, -1.17384409, -6.42825283),
                                       nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"),
               tolerance = 1e-6)
  expect_equal(res2$D_agg[[1]], 1)
  expect_equal(res2$D_agg[[2]], 0.213582281)
  expect_equal(res2$D_I[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res2$D_I[[2]], matrix(c(1.002733012, 1.146589093, 0.889606884, 0.208820902),
                                     nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res2$D_II[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
  expect_equal(res2$D_II[[2]], matrix(c(1.00670332, 1.39772932,	0.75100568,	0.20844541),
                                      nrow = 4, ncol = 1, dimnames = list(dn[[2]], "categories")) %>%
                 matsbyname::setrowtype("factors") %>% matsbyname::setcoltype("categories"))
})
