#' Subsampled wire cut scan
#'
#' An example part of a wire cut in `x3p` format. The wire cut is part of a CSAFE study
#' involving 1.5 mm Aluminium wires cut by Kaiweet wire-cutters.
#'
#' @format `x3p` object
"wire"

#' Subsampled scan of a land-engraved area
#'
#' LEAs (land-engraved areas) are created on the outside of a bullet during
#' the firing process. Depending on the rifling inside the barrel, multiple lands 
#' exist for each barrel. Striation marks in these land engraved areas are used
#' in forensic labs to determine whether two bullets were fired from the same firearm.
#'
#' @format `x3p` object
#' @examples 
#' data(lea)
#' image(lea)
#' if (interactive()) x3p_image(lea)
"lea"
