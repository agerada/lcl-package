#' Convert organism codes generated from LCL telepath to full name (and vice-versa)
#'
#' @param .data A character vector of antibiotic abbreviations (e.g. c('HAIN', 'STAU')
#' @param .clean_up Clean up and convert ambiguous names for use with AMR package:
#'
#' * Diptheroids converted to Corynebacterium
#' * Lactose fermenting coliform, Mixed coliforms, Coliform converted to E. coli
#' * Mixed anaerobes converted to Bacteroides
#' * Vancomycin resistant Entero- converted to VRE
#' * sp. removed
#' * Beta-haemolytic removed from Group X Strep
#' * Alpha haemolytic Strep converted to Viridans Streptococcus
#' * Anaerobes converted to Bacteroides
#' @param .abbreviate Converts a character vector of full organism names to their LCL abbreviations
#' @keywords org, convert_org
#' @export
#' @examples
#' org_list <- c('HAIN', 'STAU', 'HSB')
#' convert_org(org_list)
#' [1] "Haemophilus influenzae"                "Staphylococcus aureus"
#' [3] "Group B Beta haemolytic streptococcus"
#'
#' convert_org(org_list, .clean_up = T)
#' [1] "Haemophilus influenzae" "Staphylococcus aureus"  "Group B Streptococcus"

convert_org <- function(.data, .clean_up = F, .abbreviate = F) {

  if (is.null(.data)) stop('No organism data provided')
  if (!is.character(.data)) stop('Organism data must be a character vector')
  else

    if(.abbreviate == F) {
      org_codes <- org_data_from_tpath$Expansion

      if(.clean_up == T) {
        org_codes <- org_codes %>% str_replace_all(., c('Diphtheroids' = 'Corynebacterium',
                                                        'Dermobacter' = 'Dermabacter',
                                                        'Lactose fermenting coliform' = 'E. coli',
                                                        'Mixed anaerobes' = 'Bacteroides',
                                                        'Mixed coliforms' = 'E. coli',
                                                        'Vancomycin Resistant Entero.*' = 'VRE',
                                                        'Coliform.*' = 'E. coli',
                                                        ' sp.*' = '',
                                                        'Group A.*' = 'Group A Streptococcus',
                                                        'Alpha.*' = 'Viridans Group Streptococcus',
                                                        'Group G.*' = 'Group G Streptococcus',
                                                        'Group C.*' = 'Group C Streptococcus',
                                                        'Group B.*' = 'Group B Streptococcus',
                                                        'Anaerob.*' = 'Bacteroides'))
      }

      names(org_codes) <- org_data_from_tpath$Code
      return(unname(org_codes[.data]))
    }

  if(.abbreviate == T) {
    org_codes <- NULL
    org_codes <- org_data_from_tpath$Code
    names(org_codes) <- org_data_from_tpath$Expansion
    return(unname(org_codes[.data]))
  }

}
