#' Pull data from Access database in list-of-lists format
#'
#' @param database An RODBC Access database object, remember to close after open
#' @param where_clause An SQL WHERE clause. Use single quotes for strings and # for dates
#' @param select_clause An SQL SELECT clause, which can be any of the Access database column names, e.g.
#' tPatients.num_hospital, tSamples.num_spec, date_collected, org_code, spec_type,
#' antibiotic codes e.g. MER, ETP
#' @param limit Limit the number of rows returned (useful to test large queries), defaults to 10. Use 0 to get all.
#' @keywords pull_data, data gather, access
#' @export
#' @examples
#' db <- RODBC::odbcConnectAccess2007('list_of_lists_db.accdb')
#' oxa <- pull_data(db, where_clause = "com_code Like '%OXA%'", limit = 0)
#' odbcClose(db)


pull_data <- function(database = NULL,
                      where_clause = NULL, # SQL WHERE clause, use single quotes for strings, # for dates
                      # NULL = no filter
                      select_clause = NULL, # SQL SELECT clause, any of the column names, e.g.
                      # tPatients.num_hospital, tSamples.num_spec, date_collected, org_code, spec_type,
                      # abx codes e.g. MER, ETP
                      limit = 10){

  if(is.null(database)) stop('Please provide a database connection')

  # Default query (gets everything)
  query <- "SELECT *
FROM (tPatients INNER JOIN tSamples ON tPatients.[num_hospital] = tSamples.[num_hospital]) INNER JOIN tSens ON tSamples.[num_spec] = tSens.[num_spec] "

  # Build select part
  if(is.null(select_clause)) {
    select_clause <- "*"
  }

  # Limit query using TOP
  if(!is.null(limit) && limit > 0){
    query <- glue::glue("SELECT TOP {limit} {select_clause}
FROM (tPatients INNER JOIN tSamples ON tPatients.[num_hospital] = tSamples.[num_hospital]) INNER JOIN tSens ON tSamples.[num_spec] = tSens.[num_spec] ")
  }
  else{
    query <- glue::glue("SELECT {select_clause}
FROM (tPatients INNER JOIN tSamples ON tPatients.[num_hospital] = tSamples.[num_hospital]) INNER JOIN tSens ON tSamples.[num_spec] = tSens.[num_spec] ")
  }

  # Check if any filters applied
  if(!is.null(where_clause)){
    query <- paste0(query, "WHERE ", where_clause)
  }

  return(sqlQuery(database, query))
}
