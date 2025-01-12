#' Run UMAP
#'
#' Runs a umap (the uwot implementation).
#' Non-numeric variables are removed then added as annotation data for the umap.
#' If you find this function useful, please cite in accordance with `citation("uwot")`.
#'
#' @param dataset a dataframe with 1 observation per row.
#' Categorical variables will be removed before running the UMAP then used to annotate the result.
#' @param normalise Should data be recentered and rescaled before running the UMAP. Defaults to true (flag).
#' @param verbose verbose (flag)
#' @param annotate_with what columns in the original dataset should be added back. resulting UMAP to the original datasets
#' @inheritParams uwot::umap
#' @inheritDotParams uwot::umap
#'
#' @return a dataframe with colnames UMAP_1 ... N depending on `n_components` argument +
#' any categorical columns included in the original dataset.
#'
#' @export
#'
#' @examples
#' umap(iris)
umap <- function(dataset, verbose = TRUE, normalise = TRUE, n_neighbors = 15, annotate_with = c("all", "categorical", "numeric", "none"), ...){

  # Assertions
  assertions::assert_dataframe(dataset)
  assertions::assert_flag(verbose)
  assertions::assert_flag(normalise)
  assertions::assert(nrow(dataset) > 3, msg = "Dataset has too few observations for UMAP creation (dataset should have >3 rows, not [{nrow(dataset)}])")
  annotate_with <- rlang::arg_match(annotate_with)

  # Select a more sensible n_neighbors when dataset is small for the default/supplied n_neighbours
  if(nrow(dataset) <= 15 & n_neighbors >= 15){
    original_n_neighbours <- n_neighbors
    n_neighbors <- max(3, round(nrow(dataset)/5))
    cli::cli_alert_warning(
      "Setting n_neighbors to {n_neighbors} instead of {original_n_neighbours} because of how
      small the dataset is. We highly recommend manually choosing a value that is both < {nrow(dataset)} and whose value reflects how  global / local you want your clustering to be.
      See {.url https://umap-learn.readthedocs.io/en/latest/parameters.html#n-neighbors} for some helpful info."
   )
  }


  # Get numeric columns only
  dataset_numeric <- select_numeric_columns(dataset, verbose = verbose)

  if(verbose){
   cli::cli_alert_info("Running UMAP from [{ncol(dataset_numeric)}] numeric columns")
  }


  ## NORMALISE input data by default
  if(normalise){
    dataset_numeric_preprocessed <- as.data.frame(scale(dataset_numeric, center = TRUE, scale = TRUE))

    ## Drop 0-variance columns (will have all values = NA)
    dataset_numeric_preprocessed <- dataset_numeric_preprocessed[apply(dataset_numeric_preprocessed, MARGIN = 2, FUN = function(x){!all(is.na(x))})]
  }
  else
    dataset_numeric_preprocessed <- dataset_numeric


  # Run UMAP
  result <- uwot::umap(dataset_numeric_preprocessed, n_neighbors = n_neighbors, ...)

  # Convert to UMAP and rename
  result <- as.data.frame(result)
  names(result) <- paste0("UMAP_", seq_len(ncol(result)))

  # Add original categorical/numeric variables back into dataset depending on annotate_with
  if(annotate_with == "all" | annotate_with == "categorical"){
    dataset_non_numeric <- select_non_numeric_columns(dataset, verbose = FALSE)
    result <- cbind(result, dataset_non_numeric)
  }

  if(annotate_with == "all" | annotate_with == "numeric")
    result <- cbind(result, dataset_numeric)

  if(! annotate_with  %in% c("all", "numeric", "categorical")) {
    assertions::assert(annotate_with == "none", msg = "No implementation for {.arg annotate_with = {annotate_with}}. Please open a github issue with this error message.")
  }


  # Return Result
  return(result)
}

identify_numeric_columns <- function(dataset){
  assertions::assert_dataframe(dataset)

  columns <- colnames(dataset)
  is_numeric <- vapply(dataset, is.numeric, FUN.VALUE = logical(1))
  return(columns[is_numeric])
}

is.not.numeric <- function(x){ !is.numeric(x) }

identify_non_numeric_columns <- function(dataset){
  assertions::assert_dataframe(dataset)

  columns <- colnames(dataset)
  is_non_numeric <- vapply(dataset, is.not.numeric, FUN.VALUE = logical(1))

  return(columns[is_non_numeric])
}

select_numeric_columns <- function(dataset, verbose){
  numeric_cols <- identify_numeric_columns(dataset)

  if(verbose){
    non_numeric_cols <- setdiff(colnames(dataset), numeric_cols)
    n_non_numeric_cols <- length(non_numeric_cols)
    cli::cli_alert_info("Dropping {n_non_numeric_cols} categorical columns: [{non_numeric_cols}]")
  }

  dataset[numeric_cols]
}

select_non_numeric_columns <- function(dataset, verbose){
  numeric_cols <- identify_non_numeric_columns(dataset)

  if(verbose){
    non_numeric_cols <- setdiff(colnames(dataset), numeric_cols)
    n_non_numeric_cols <- length(non_numeric_cols)
    cli::cli_alert_info("Dropping {n_non_numeric_cols} numeric columns: [{n_non_numeric_cols}]")
  }

  dataset[numeric_cols]
}

plot_umap <- function(){

}
