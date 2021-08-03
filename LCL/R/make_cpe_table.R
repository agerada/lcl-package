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
#' odbcClose(db)
#' cpe_clean <- clean_up(cpe)
#' cpe_table <- make_cpe_table(cpe_clean, genotypes = list('OXA48', 'KPC'))

make_cpe_table <- function(cpe_data, genotypes = list('OXA48', 'KPC', 'NDM', 'VIM', 'IMP')){
  cpe_data_na_dropped <- cpe_data %>% select_if(~ !all(is.na(.x)))
  results <- tibble(antibiotic = names(cpe_data_na_dropped[,is.rsi.eligible(cpe_data_na_dropped)]))
  for (gen in genotypes) {
    filtered_temp_data <- cpe_data_na_dropped %>%
      filter(str_detect(com_code, gen)) %>%
      mutate_if(is.rsi.eligible, as.rsi)

    temp_out <-
      filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x)), ~ susceptibility(.x, as_percent = T, minimum = 1)) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_susceptibility'))

    results <- left_join(results, temp_out)

    temp_out <- filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x)), ~ paste0(count_SI(.x), '/', count_all(.x))) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_proportion'))

    results <- left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x)), count_all) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_n'))

    results <- left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x)), count_SI) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_n_susceptible'))

    results <- left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x), count_SI(.x)), function(.x) paste0(round(prop.test(count_SI(.x), count_all(.x), p = 0.95)$conf.int[[1]] * 100, 1), '%')) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_lower_bound'))

    results <- left_join(results, temp_out)

    temp_out <-
      filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x), count_SI(.x)), ~ paste0(round(prop.test(count_SI(.x), count_all(.x), p = 0.95)$conf.int[[2]] * 100, 1), '%')) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_upper_bound'))

    results <- left_join(results, temp_out)

    # add merged column of susceptible/n
    temp_out <- filtered_temp_data %>%
      summarise_if(~ all(is.rsi(.x), count_all(.x)), ~ paste0(count_SI(.x), '/', count_all(.x))) %>%
      pivot_longer(cols = everything(), names_to = 'antibiotic', values_to = paste0(gen, '_proportion'))

    results <- left_join(results, temp_out)
  }

  return(results)
}
