`%>%` = dplyr::`%>%`

#' Clean UK postcodes by removing spaces and standardising case
#'
#' Internal helper function used to standardise postcode formatting
#' before matching to the wm_postcodes lookup table.
#'
#' @param pc A character vector of postcodes.
#' @param lookup Character vector of valid postcodes (default: wm_postcodes).
#'
#' @return A character vector of matched, properly formatted postcodes.
#' @keywords internal
#' @noRd
clean_postcodes <- function(
    pc
) {
  data("wm_postcodes", envir = environment())

  pc_clean <- toupper(gsub("\\s+", "", pc))
  lookup_clean <- toupper(gsub("\\s+", "", wm_postcodes$postcode))

  wm_postcodes$postcode[match(pc_clean, lookup_clean)]
}


#' Validate postcodes against wm_postcodes lookup
#'
#' Checks whether all supplied postcodes exist in the wm_postcodes dataset.
#'
#' @param pc A character vector of postcodes.
#'
#' @return Invisibly returns NULL. Stops if invalid postcodes are found.
#' @keywords internal
#' @noRd
check_postcodes <- function(pc) {

  data("wm_postcodes", envir = environment())

  missing <- pc[!(pc %in% wm_postcodes$postcode)]

  if (length(missing) > 0) {
    stop(
      paste0(
        "The following postcodes were not found in the lookup:\n",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}
