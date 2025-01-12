test_that("selection of numeric/non-numeric columns", {
  # First 10 rows of Iris
  iris_subset <- data.frame(
    Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4),
    Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9),
    Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
    Petal.Width = rep(c(0.2, 0.4), c(5L, 1L)),
    Species = factor(rep("setosa", 6L), levels = c("setosa", "versicolor", "virginica"))
  )

  # Expected result of select_numeric_columns
  iris_subset_numeric <- data.frame(
    Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4),
    Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9),
    Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
    Petal.Width = rep(c(0.2, 0.4), c(5L, 1L))
  )

  # Expected result of select_non_numeric_columns
  iris_subset_non_numeric <- data.frame(
    Species = factor(rep("setosa", 6L), levels = c("setosa", "versicolor", "virginica"))
  )


  # Select only numeric cols
  expect_equal(
    select_numeric_columns(iris_subset, verbose = FALSE),
    iris_subset_numeric
    )

  # Select only non-numeric cols
  expect_equal(
    select_non_numeric_columns(iris_subset, verbose = FALSE),
    iris_subset_non_numeric
  )
})


test_that("annotation of umap data.frame with categorical/numeric or all columns in original dataset works", {
  # First 10 rows of Iris
  iris_subset <- data.frame(
    Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4),
    Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9),
    Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
    Petal.Width = rep(c(0.2, 0.4), c(5L, 1L)),
    Species = factor(rep("setosa", 6L), levels = c("setosa", "versicolor", "virginica"))
  )


  # Annotating with all raw features produces a 7 column data.frame (5 features + 2 umap dimensions)
  expect_equal(ncol(umap(iris_subset, verbose = FALSE, annotate_with = "all")), 7)

  # Annotating with categorical features produces a 3 column data.frame (1 feature + 2 umap dimensions)
  expect_equal(ncol(umap(iris_subset, verbose = FALSE, annotate_with = "categorical")), 3)

  # Annotating with categorical features produces a 3 column data.frame (1 feature + 2 umap dimensions)
  expect_equal(ncol(umap(iris_subset, verbose = FALSE, annotate_with = "categorical")), 3)
})

test_that("umap works even with zero-variance columns", {
  # First 10 rows of Iris
  iris_subset <- data.frame(
    Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4),
    Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9),
    Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
    Petal.Width = rep(c(0.2, 0.4), c(5L, 1L)),
    Species = factor(rep("setosa", 6L), levels = c("setosa", "versicolor", "virginica")),
    NoVariance = 0
  )

  expect_no_error(umap(iris_subset, verbose = FALSE))

})



