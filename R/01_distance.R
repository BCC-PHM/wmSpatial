# Postcode distance functions

#' Join postcode-level geographic information
#'
#' Adds geographic attributes (latitude, longitude, etc.)
#' from the wm_postcodes lookup table to a dataset containing postcodes.
#'
#' @param df A data frame containing at least a postcode column.
#' @param df_pc_col Name of the postcode column in `df`.
#' @param strip Logical. If TRUE, only the postcode column is retained
#'        before joining (useful for lookup-style operations).
#'
#' @return A data frame with added postcode metadata from wm_postcodes.
#'
#' @export
#'
#' @examples
#' df <- data.frame(postcode = "B1 1BB")
#' join_postcode_info(df)
join_postcode_info <- function(
    df,
    df_pc_col = "postcode",
    strip = FALSE
    ) {

  # Clean postcodes
  df[[df_pc_col]]<- clean_postcodes(df[[df_pc_col]])

  # Check all in lookup
  check_postcodes(df[[df_pc_col]])

  if (strip) {
    # Remove all except postcode column
    df <- df %>%
      select(dplyr::all_of(df_pc_col))
  }

  data("wm_postcodes", envir = environment())

  dplyr::left_join(
    df,
    wm_postcodes,
    by = setNames("postcode", df_pc_col)
  )
}

#' Find nearest postcode(s) between two datasets
#'
#' For each postcode in `df1`, finds the nearest postcode(s)
#' in `df2` using Euclidean distance on latitude/longitude.
#'
#' @param df1 A data frame containing input postcodes.
#' @param df2 A data frame containing reference postcodes.
#' @param df1_pc_col Column name of postcodes in `df1`.
#' @param df2_pc_col Column name of postcodes in `df2`.
#' @param n Number of nearest neighbours to return.
#' @param radius (Optional) Not yet implemented.
#'
#' @return A data frame with additional columns:
#' \itemize{
#'   \item pc1, pc2, ... nearest postcodes in `df2`
#' }
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(postcode = "B1 1BB")
#' df2 <- data.frame(postcode = c("B36 9ST", "B96 6DD", "B70 7EJ", "DY8 3YD", "B21 8BQ"))
#' nearest_postcode(df1, df2, n = 2)
nearest_postcode <- function(
    df1,
    df2,
    df1_pc_col = "postcode",
    df2_pc_col = "postcode",
    n = 1,
    radius = NULL # to be implemented
) {

  # get input postcode coordinates
  df1 <- join_postcode_info(
    df1,
    df1_pc_col
    )

  # get output postcode coordinates
  df2 <- join_postcode_info(
    df2,
    df2_pc_col,
    strip = TRUE
  )

  coords1 <- as.matrix(df1[, c("longitude", "latitude")])
  coords2 <- as.matrix(df2[, c("longitude", "latitude")])

  # Find the nearest neighbour in df2 for each row of df1
  nn <- RANN::nn2(data = coords2,
            query = coords1,
            k = n)

  for (i in seq_len(n)) {
    # Index of nearest postcode in df2
    nearest_index <- nn$nn.idx[, i]

    # Add results to df1
    df1[[paste0("nearest_pc_", i)]] <- df2$postcode[nearest_index]

  }

  return(df1)
}
