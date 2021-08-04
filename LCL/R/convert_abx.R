#' Convert antibiotic codes generated from LCL telepath to full name (and vice-versa). This
#'
#' @param .data A character vector of antibiotic abbreviations or names
#' @param .clean_up Clean up and convert ambiguous names for use with AMR package:
#'
#' * 'Amp/Amoxil' = 'Ampicillin',
#' * "Eryth/Clarith." = 'Erythromycin',
#' * Ceftolozane-tazobactam' = 'Ceftolozane/tazobactam',
#' * Caz/Avi' = 'Ceftazidime/avibactam',
#' * Pip/Tazo' = 'Piperacillin/Tazobactam'
#' @param .abbreviate Converts a character vector of full antibiotic names to their LCL abbreviations
#' @param .been_cleaned Set as TRUE if the antibiotic names have been "cleaned up" with .clean_up
#' @keywords abx, convert_abx
#' @export
#' @examples
#' abx_list <- c('AML', 'CIP', 'P/T')
#' convert_abx(abx_list)
#' [1] "Amp/Amoxil"    "Ciprofloxacin" "Pip/Tazo"
#'
#' convert_abx(abx_list, .clean_up = T)
#' [1] "Ampicillin"              "Ciprofloxacin"           "Piperacillin/Tazobactam"

convert_abx <- function(.data, .clean_up = F, .abbreviate = F, .been_cleaned = F) {

if (is.null(.data)) stop('No antibiotic data provided')
if (!is.character(.data)) stop('Antibiotic data must be a character vector')
  else

if(.abbreviate == F) {
  abx_codes <- abx_data_from_tpath$Antibiotic_Exp

    if(.clean_up == T) {
      abx_codes <- abx_codes %>% stringr::str_replace_all(c('Amp/Amoxil' = 'Ampicillin',
                                                 "Eryth/Clarith." = 'Erythromycin',
                                                 'Ceftolozane-tazobactam' = 'Ceftolozane/tazobactam',
                                                 'Caz/Avi' = 'Ceftazidime/avibactam',
                                                 'Pip/Tazo' = 'Piperacillin/Tazobactam'))
    }

names(abx_codes) <- abx_data_from_tpath$Antibiotic_Code
return(unname(abx_codes[.data]))
}

if(.abbreviate == T) {
  abx_codes <- NULL
  abx_codes <- abx_data_from_tpath$Antibiotic_Code

  if (.been_cleaned == T) {
  .data <- .data %>% stringr::str_replace_all(c('Ampicillin' = 'Amp/Amoxil',
                                                               'Erythromycin' = "Eryth/Clarith.",
                                                               'Ceftolozane/tazobactam' = 'Ceftolozane-tazobactam',
                                                               'Ceftazidime/avibactam' = 'Caz/Avi',
                                                               'Piperacillin/Tazobactam' = 'Pip/Tazo'))
  }
  names(abx_codes) <- abx_data_from_tpath$Antibiotic_Exp
  return(unname(abx_codes[.data]))
}

}

