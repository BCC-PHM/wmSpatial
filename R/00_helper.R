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

  match_elems <- match(pc_clean, lookup_clean)
  
  # Check if any missing
  if (any(is.na(match_elems))) {
    missing <- pc[is.na(match_elems)]
    
    stop(
      paste0(
        "The following postcodes were not found in the lookup:\n",
        paste(
          missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  return(wm_postcodes$postcode[match_elems])
}


