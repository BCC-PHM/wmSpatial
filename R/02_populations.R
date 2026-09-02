# Population-related functions

#' Calculate the proportion of each LSOA within a given radius of postcodes
#'
#' Estimates the proportion of the population of each Lower Layer Super
#' Output Area (LSOA) that is located within a specified radius of at least
#' one of a set of postcodes.
#'
#' The function identifies postcodes within the specified radius of each
#' input postcode using the postcode coordinates in the package's
#' wm_postcodes dataset. Populations are aggregated to LSOA level, with
#' postcodes that fall within the radius of multiple input postcodes counted
#' only once. The returned proportion is therefore the estimated proportion
#' of the total LSOA population whose postcode centroid falls within the
#' specified radius of at least one input postcode.
#'
#' @param df A data frame containing the input postcodes and their
#' corresponding geographic coordinates. The data frame must contain a
#' postcode column and latitude and longitude columns.
#' @param radius_km Numeric. The radius around each input postcode, in
#' kilometres.
#' @param df_pc_col Character string. The name of the column in df
#' containing postcodes. Defaults to "postcode".
#'
#' @return A data frame containing one row for each LSOA represented by at
#' least one postcode within the specified radius, with the following
#' columns:
#' \describe{
#' \item{lsoa21_code}{The 2021 LSOA code.}
#' \item{pop_inside}{The estimated population within the specified
#' radius.}
#' \item{lsoa_pop}{The total estimated population of the LSOA.}
#' \item{pop_prop}{The estimated proportion of the LSOA population within
#' the specified radius.}
#' }
#' @export
prop_in_radius <- function(
    df,
    radius_km,
    df_pc_col = "postcode"
) {
  # Clean postcodes and join postcode info
  df <- join_postcode_info(
    df,
    df_pc_col
  )
  
  # Load West Mids postcode data
  data("wm_postcodes", envir = environment())
  
  lsoa_21_pop_totals <- wm_postcodes %>%
    dplyr::group_by(lsoa21_code) %>%
    dplyr::summarise(
      lsoa_pop = sum(population, na.rm = T)
    )
  
  all_postcodes <- list()
  # Look over all coordinates to get all overlapping postcodes
  for (i in 1:nrow(df)) {
    # Get all postcodes in each LSOA within 10 minutes walking distance
    postcodes_i <- purrr::map2_dfr(
      df$latitude[i],
      df$longitude[i],
      ~spatialrisk::points_in_circle(wm_postcodes, .y, .x,
                                     lon = longitude,
                                     lat = latitude,
                                     radius = radius_km*1000))
    
    all_postcodes[[i]] <- postcodes_i %>%
      dplyr::select(postcode, lsoa21_code, population)
  }
  
  lsoa_pops_in_radius <- data.table::rbindlist(all_postcodes) %>%
    # Remove double counted postcodes
    dplyr::distinct() %>%
    # calculate population in the radii for each LSOA
    dplyr::group_by(lsoa21_code) %>%
    dplyr::summarise(
      pop_inside = sum(population, na.rm = T)
    ) 
  
  # Join to total number of postcodes in each LSOA
  output_lsoas <- lsoa_21_pop_totals %>%
    dplyr::left_join(
      lsoa_pops_in_radius,
      by = dplyr::join_by("lsoa21_code")
    ) %>%
    # Calculate percentage of LSOA postcodes within 10 minutes walk
    dplyr::mutate(
      # Impute missing inside populations with zero
      pop_inside = ifelse(is.na(pop_inside), 0, pop_inside),
      pop_prop = pop_inside / lsoa_pop
    )
  
  return(output_lsoas)
}