#' Visualize Dimensionality Reduction Results
#'
#' This function creates a scatter plot of the results from a dimensionality reduction technique (e.g., UMAP or PCA).
#' The plot can include optional metadata features such as color, fill, and shape for
#' further distinction of sample groups.
#'
#' @param dimred A data frame containing the dimensionality reduction results with at least columns for the two dimensions
#' (`col_dim1`, `col_dim2`) and a sample identifier (`col_sample`).
#' @param col_sample A string specifying the column name in `dimred` that identifies the samples. Default is "sample".
#' @param col_dim1 A string specifying the column name in `dimred` for the first dimension. Default is "dim1".
#' @param col_dim2 A string specifying the column name in `dimred` for the second dimension. Default is "dim2".
#' @param metadata Optional. A data frame with additionalmetadata corresponding to the samples, with columns for the sample
#' identifiers (`col_sample`) and other optional aesthetic mappings (`col_fill`, `col_colour`, `col_shape`).
#' @param col_fill Optional. A string specifying the column name in `metadata` for the fill aesthetic. Default is NULL.
#' @param col_colour Optional. A string specifying the column name in `metadata` for the color aesthetic. Default is NULL.
#' @param col_shape Optional. A string specifying the column name in `metadata` for the shape aesthetic. Default is NULL.
#' @param title A string specifying the title of the plot. Default is "Dimensionality Reduction".
#' @param xlab A string specifying the label for the x-axis. Default is the value of `col_dim1`.
#' @param ylab A string specifying the label for the y-axis. Default is the value of `col_dim2`.
#'
#' @return A ggplot object representing the dimensionality reduction plot.
#' @export
#'
#' @examples
#' # Example UMAP data
#' dimred <- example_umap()
#' metadata <- example_umap_metadata()
#' sig_visualise_dimred(dimred, metadata = metadata, col_colour = "dataset")
umap_plot <- function(
    dimred, col_sample=NULL, col_tooltip=col_sample, col_dim1="UMAP_1", col_dim2 = "UMAP_2",
    metadata = NULL, col_fill = NULL, col_colour = NULL, col_shape = NULL, title = "Dimensionality Reduction", xlab=col_dim1, ylab=col_dim2){

  ## Assertions
  assertions::assert_names_include(dimred, c(col_dim1, col_dim2, col_sample))

  # What to do if no metadata
  if(!is.null(metadata)) {
    assertions::assert_dataframe(metadata)
    assertions::assert(!is.null(col_sample), msg = "{.arg col_sample} is required when {.arg metadata} dataframe is supplied")
    assertions::assert_names_include(metadata, c(col_sample, col_fill = NULL, col_colour = NULL, col_shape = NULL))
    assertions::assert_no_duplicates(metadata[[col_sample]])

    dimred = merge(x = dimred, y = metadata, by = col_sample, all.x = TRUE)
  }

  ggplot(dimred, aes(
    x = .data[[col_dim1]],
    y = .data[[col_dim2]],
    colour = if(is.null(col_colour)) NULL else .data[[col_colour]],
    fill = if(is.null(col_fill)) NULL else .data[[col_fill]],
    shape = if(is.null(col_shape)) NULL else .data[[col_shape]],
    data_id = if(is.null(col_sample)) NULL else .data[[col_sample]],
    tooltip = if(is.null(col_tooltip)) NULL else .data[[col_tooltip]],
    onclick = paste0('navigator.clipboard.writeText("',col_sample,'")')
  )) +
    ggiraph::geom_point_interactive() +
    ggplot2::labs(
      fill=col_fill,
      colour=col_colour,
      shape=col_shape
    ) +
    xlab(xlab) +
    ylab(ylab) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

#' Exemplar UMAP dataframe
#'
#' @return dataframe with UMAP results
#' @export
#'
#' @examples
#' umap <- example_umap()
#' print(umap)
example_umap <- function(){
  data.frame(
    dim1 = c(
      -1.15777099132538, -0.606620407104492, 0.766777420043946, -0.147877311706543,
      0.31230583190918, -0.32458553314209
    ),
    dim2 = c(
      1.04326260089874, 0.689059448242187, -0.428349304199219, -0.563606071472169,
      -0.062956619262696, 0.365852546691894
    ),
    sample = c("COLO829v003T", "DO1000", "DO1001", "DO1002", "DO1003", "DO1004")
  )
}

#' Example Metadata for UMAP
#'
#' This function returns a data frame with example metadata for samples that can be used
#' alongside dimensionality reduction results like UMAP or PCA. The metadata includes sample identifiers
#' and a dataset category.
#'
#' @return A data frame with two columns: `sample` (sample identifiers) and `dataset` (dataset category).
#' @export
#'
#' @examples
#' metadata <- example_umap_metadata()
#' print(metadata)
example_umap_metadata <- function(){
  data.frame(
    sample = c("COLO829v003T", "DO1000", "DO1001", "DO1002", "DO1003", "DO1004"),
    dataset = c("Cell line", "PCAWG", "PCAWG", "PCAWG", "PCAWG","PCAWG")
  )
}

#' Make UMAP visualisation interactive
#'
#' @param gg the plot returned from any sigverse visualisation
#' @inheritParams ggiraph::girafe
#' @inheritParams ggiraph::opts_sizing
#' @return a ggiraph interactive visualisation
#' @export
#'
umap_make_interactive <- function(gg, width_svg = NULL, height_svg = NULL, rescale = FALSE, width = 1){

  # Check if any interactive data was supplied.
  if(!any(c("data_id", "tooltip", "onclick") %in% names(gg$mapping))){
   cli::cli_warn(
     "{.strong umap_make_interactive}: Making a plot interactive without a data_id, tooltip or onclick aesthetic is useless.
     If plot was generated using {.code umap_plot()} please specify the col_sample argument"
   )
  }
  ggi <- ggiraph::girafe(ggobj = gg, width_svg = width_svg, height_svg = height_svg)
  ggi <- ggiraph::girafe_options(
    ggi,
    ggiraph::opts_hover(
      css = "stroke-width: 2",
      nearest_distance = 50
    ),
    ggiraph::opts_hover_inv(
      css = "fill-opacity:0.5; stroke-opacity: 0.5"
    ),
    ggiraph::opts_tooltip(
      css = ggiraph::girafe_css(
        css = "background-color:white;color:black;padding:5px;border-radius:3px;border:2px solid black",line = "color:black"
      ),
      opacity = 1
    ),
    ggiraph::opts_sizing(
      rescale = rescale, width = width
    )
  )
  return(ggi)
}
