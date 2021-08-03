#' Clean up data imported using LCL::pull_data.
#'
#' This function uses the AMR package and additionally performs the following data cleaning steps:
#' * Copy values of AML into AMP, and CTX into CRO to simplify data
#' * Converts telepath antibiotic and organism codes into full names
#' * Removes suppression lowercase "s" from antibiotic columns, if present
#' * Converts antibiotic columns to AMR::RSI format
#' * Creates AMR_org using AMR::as.mo to facilitate further processing
#' * Filters rows based on first isolate rules (see AMR::first_isolate)
#' * Applies full EUCAST rules
#'
#' @param x A data frame or tibble containing data pulled using LCL::pull_data
#' @keywords clean_up, data cleaning
#' @export
#' @examples
#' db <- RODBC::odbcConnectAccess2007('list_of_lists_db.accdb')
#' oxa <- pull_data(db, where_clause = "com_code Like '%OXA%'", limit = 0)
#' odbcClose(db)
#' oxa_clean <- clean_up(oxa)

clean_up <- function(x){
  if (!"data.frame" %in% class(x)) stop('Please provide a dataframe or tibble')

  # Copy values of AML into AMP and CTX into CRO
  if(("AML" %in% names(x)) && ("AMP" %in% names(x))){
    message("Merging AML into AMP")
    x$AMP <- if_else(is.na(x$AMP), x$AML, x$AMP)
    x <- subset(x, select = -AML)
  }

  if(("CRO" %in% names(x)) && ("CTX" %in% names(x))){
    message("Merging CTX into CRO")
    x$CRO <- if_else(is.na(x$CRO), x$CTX, x$CRO)
    x <- subset(x, select = -CTX)
  }


  if ("org_code" %in% names(x)) {
    message("Converting organism codes into full names")
    x$org_name <- LCL::convert_org(x$org_code,
                                   .clean_up = T)
  }

  for(i in seq_along(x)){
    if(!is.na(convert_abx(names(x)[i]))){
      message(paste("Converting", names(x)[i]))
      x[[i]] <- as.character(x[[i]]) # convert to char in case incorrect import
      names(x)[i] <- convert_abx(names(x)[i], .clean_up = T)
    }
  }

  # remove suppression lowercase 's' and convert to RSI format

  abx_columns <- AMR::is.rsi.eligible(x) # used to index abx columns

  # convert abx columns to string, in case imported incorrectly
  x[abx_columns] <- lapply(x[abx_columns], function(x) gsub('s', '', x))

  x[abx_columns] <- lapply(x[abx_columns], AMR::as.rsi)

  x$AMR_org <- AMR::as.mo(x$org_name)

  # run first isolates
  x$first <- AMR::first_isolate(x,
                                col_patient_id = 'num_hospital',
                                col_date = 'date_collected',
                                col_mo = 'AMR_org',
                                info = TRUE)

  x <- subset(x, first == TRUE, select = -first)

  x <- AMR::eucast_rules(x, rules = "all")

  return(x)
}
