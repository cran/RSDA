context("Funciones Estadisticas")
data("example3")


test_that("Media simbolica",{
  expect_equal(mean(example3[,1]), 1.628571, tolerance  = 0.000001)
  expect_equal(mean(example3[,2]), 5, tolerance  = 0.000001)
  expect_equal(mean(example3[,2],"centers"), 5, tolerance  = 0.000001)
  expect_equal(mean(example3[,2],"interval"),
               c(F2 = 1.857143,F2.1=8.142857 ), tolerance  = 0.000001)
  expect_equal(mean(example3[,3],"modal"),
               c(M1 = 0.3714286, M2 = 0.3000000, M3 = 0.3285714),
               tolerance  = 0.000001)
})

test_that("Varianza simbolica",{
  expect_equal(var(example3[,1]), 15.98238, tolerance  = 0.000001)
  expect_equal(var(example3[,2]), 90.66667, tolerance  = 0.000001)
  expect_equal(var(example3[,6]), 1872.358, tolerance  = 0.000001)
  expect_equal(var(example3[,6], method = "centers"),
               1872.358, tolerance  = 0.000001)
  expect_equal(var(example3[,6], method = "billard"),
               1355.143, tolerance  = 0.000001)
  expect_equal(var(example3[,6], method = "interval"),
               c(F6 = 2408.966, F6.1 = 1670.509),
               tolerance  = 0.000001)
  expect_equal(var(example3[,3], method = "modal"),
               c(M1 = 0.10904762, M2 = 0.08666667, M3 = 0.04571429), tolerance  = 0.000001)
  expect_equal(var(example3[,1]), 15.98238, tolerance  = 0.000001)
  expect_equal(var(c(1,4,5,4,3)), 2.3)
  expect_equal(var(c(1,4,5,4,NA)), NA_real_)
  expect_equal(var(c(1,4,5,4,NA), na.rm = T), 3)
})

test_that("Desviacion estandar simbolica",{
  expect_equal(sd(example3[,1]), 3.997797, tolerance  = 0.000001)
  expect_equal(sd(example3[,2]), 6.733003, tolerance  = 0.000001)
  expect_equal(sd(example3[,6]), 30.59704, tolerance  = 0.000001)
  expect_equal(sd(example3[,6], method = "centers"),
               30.59704, tolerance  = 0.000001)
  expect_equal(sd(example3[,6], method = "billard"),
               36.81226, tolerance  = 0.000001)
  expect_equal(sd(example3[,6], method = "interval"),
               c(F6 = 49.08121, F6.1 = 40.87186),
               tolerance  = 0.000001)
  expect_equal(sd(example3[,3], method = "modal"),
               c(M1 = 0.3302236, M2 = 0.2943920, M3 = 0.2138090), tolerance  = 0.000001)
  expect_equal(sd(c(1,4,5,4,3)), 1.516575, tolerance  = 0.000001)
  expect_equal(sd(c(1,4,5,4,NA)), NA_real_)
  expect_equal(sd(c(1,4,5,4,NA), na.rm = T), 1.732051,tolerance  = 0.000001)
})

test_that("Mediana simbolica",{
  expect_equal(median(example3[,1]), 1.4, tolerance  = 0.000001)
  expect_equal(median(example3[,2]), 1.5, tolerance  = 0.000001)
  expect_equal(median(example3[,6], method = "interval"),
               c(F6 = 5, F6.1 = 89),tolerance  = 0.000001)
  expect_equal(median(example3[,3], method = "modal"),
               c(M1 = 0.2, M2 = 0.2, M3 = 0.3), tolerance  = 0.000001)
})


test_that("Correlacion simbolica",{
  expect_equal(cor(example3[,1],example3[,4],method= "centers"), 0.2864553, tolerance  = 0.000001)
  expect_equal(cor(example3[,2],example3[,6], method = "centers"), -0.6693648, tolerance  = 0.000001)
  expect_equal(cor(example3[,2],example3[,6], method = "billard"),
               -0.6020041, tolerance  = 0.000001)

})
