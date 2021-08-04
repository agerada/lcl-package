#' Create CPE summary table
#'
#' This function takes data imported using LCL::pull_data and cleaned using LCL::clean_up and
#' uses it to generate a CPE summary table.
#'
#' @param cpe_data A data frame or tibble containing data pulled using LCL::pull_data and cleaned using LCL::clean_up
#' @param genotypes A list of genotypes (string format) of interest. Defaults to the big five: OXA48, KPC, NDM, VIM, IMP
#' @keywords cpe, data cleaning
#' @export
#' @examples
#' db <- RODBC::odbcConnectAccess2007('list_of_lists_db.accdb')
#' cpe <- pull_data(db, where_clause = "com_code Like '%OXA%' OR com_code Like '%KPC%'", limit = 0)
#' RODBC::odbcClose(db)
#' cpe_clean <- clean_up(cpe)
#' cpe_table <- make_cpe_table(cpe_clean, genotypes = list('OXA48', 'KPC'))

make_cpe_table <- function(cpe_data, genotypes = list('OXA48', 'KPC', 'NDM', 'VIM', 'IMP')){
  cpe_data_na_dropped <- cpe_data %>% dplyr::select_if(~ !all(is.na(.x)))
  results <- dplyr::tibble(antibiotic = names(cpe_data_na_dropped[,AMR::is.rsi.eligible(cpe_data_na_dropped)]))
  for (gen in genotypes) {
    filtered_temp_data <- cpe_data_na_dropped %>%
      dplyr::filter(stringr::str_detect(com_code, gen)) %>%
      dplyr::mutate_if(AMR::is.rsi.eligible, AMR::as.rsi)

    temp_out <-
      filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x)), ~ AMR::susceptibility(.x, as_percent = T, minimum = 1)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_susceptibility'))

    results <- dplyr::left_join(results, temp_out)

    temp_out <- filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x)), ~ paste0(AMR::count_SI(.x), '/', AMR::count_all(.x))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_proportion'))

    results <- dplyr::left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x)), AMR::count_all) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_n'))

    results <- dplyr::left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x)), AMR::count_SI) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_n_susceptible'))

    results <- dplyr::left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x), AMR::count_SI(.x)), function(.x) paste0(round(stats::prop.test(AMR::count_SI(.x), AMR::count_all(.x), p = 0.95)$conf.int[[1]] * 100, 1), '%')) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_lower_bound'))

    results <- dplyr::left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x), AMR::count_SI(.x)), ~ paste0(round(stats::prop.test(AMR::count_SI(.x), AMR::count_all(.x), p = 0.95)$conf.int[[2]] * 100, 1), '%')) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_upper_bound'))

    results <- dplyr::left_join(results, temp_out)

    # add merged column of susceptible/n
    temp_out <- filtered_temp_data %>%
      dplyr::summarise_if(~ all(AMR::is.rsi(.x), AMR::count_all(.x)), ~ paste0(AMR::count_SI(.x), '/', AMR::count_all(.x))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'antibiotic', values_to = paste0(gen, '_proportion'))

    results <- dplyr::left_join(results, temp_out)
  }

  return(results)
}
