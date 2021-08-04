#' Create CPE summary table formatted using Flextable package
#'
#' This function takes data imported using LCL::pull_data that has undergone the following cleanup steps:
#' LCL::clean_up
#' LCL::make_cpe_table
#'
#' @param x A data frame or tibble in the format created using LCL::make_cpe_table
#' @param drop_abx If specified, a character vector of antibiotics to drop. Default NULL.
#' @param genotypes A list of genotypes (string format) of interest. Defaults to the big five: OXA48, KPC, NDM, VIM, IMP. Ensure that this matches the genotypes in the data input.
#' @keywords cpe, data cleaning, cpe table
#' @export
#' @examples
#' db <- RODBC::odbcConnectAccess2007('list_of_lists_db.accdb')
#' cpe <- pull_data(db, where_clause = "com_code Like '%OXA%' OR com_code Like '%KPC%'", limit = 0)
#' RODBC::odbcClose(db)
#' cpe_clean <- clean_up(cpe)
#' cpe_table <- make_cpe_table(cpe_clean, genotypes = list('OXA48', 'KPC'))
#' cpe_flex_table <- make_cpe_flextable(cpe_table,
#'          drop_abx = c('Cefuroxime', 'Ceftriaxone', 'Doxycycline', 'Meropenem',
#'          'Ertapenem', 'Teicoplanin', 'Vancomycin', 'Clindamycin', 'Erythromycin',
#'          'Linezolid', 'Rifampicin', 'Fusidic acid'),
#'           genotypes = list('OXA48', 'KPC'))

make_cpe_flextable <- function(x, drop_abx = NULL, genotypes = list('OXA48', 'KPC', 'NDM', 'VIM', 'IMP')){ # only takes in OXA48,KPC,NDM,VIM
  # drop unwanted rows
  x <- x %>% dplyr::select(!dplyr::ends_with('_n') & !dplyr::ends_with('_n_susceptible'))

  if(!is.null(drop_abx)){
    x <- x %>% dplyr::filter(!antibiotic %in% drop_abx)
  }

  flex_cpe <- flextable::flextable(x)

  flex_cpe <- flextable::delete_part(flex_cpe, part = 'header')
  flex_cpe <- flextable::add_header_row(flex_cpe, values = c('', rep(c('Susceptibility', 'Susceptible/Tested',
                                                            'Lower 95% CI', 'Upper 95% CI'), length(genotypes))))
  flex_cpe <- flextable::add_header_row(flex_cpe, values = unlist(c('', genotypes)), colwidths = c(1,rep(4, times = length(genotypes))))
  flex_cpe <- flextable::align(flex_cpe, align = 'center', part = 'header')
  flex_cpe <- flextable::align(flex_cpe, align = 'center', part = 'body')
  flex_cpe <- flextable::bold(flex_cpe, part = 'header')

  flex_cpe <- flextable::add_footer_row(flex_cpe, values = c('Green', '=> 50% susecptible with good certainty (=> 50% lower 95% confidence interval)'), colwidths = c(1, 4 * length(genotypes)), top = FALSE)
  flex_cpe <- flextable::add_footer_row(flex_cpe, values = c('Yellow', '=> 50% susceptible with poor certainty (< 50% lower 95% confidence interval)'), colwidths = c(1, 4 * length(genotypes)), top = FALSE)

  # add caveats
  flex_cpe <- flextable::compose(flex_cpe, i = flex_cpe$body$dataset$antibiotic == 'Tigecycline', j = 1, as_paragraph(as_chunk('Tigecycline *')), part = 'body')
  flex_cpe <- flextable::add_footer_lines(flex_cpe, values = ' * breakpoints only reported for E. coli')
  flex_cpe <- flextable::bg(flex_cpe, i = 1, j = 1, bg = 'green', part = 'footer')
  flex_cpe <- flextable::bg(flex_cpe, i = 2, j = 1, bg = 'yellow', part = 'footer')
  flex_cpe <- flextable::align(flex_cpe, i = c(1,2), j = 1, align = 'right', part = 'footer')
  flex_cpe <- flextable::add_footer_lines(flex_cpe, values = glue::glue("Date range {min(lubridate::date(cpe_cleaned$date_collected))} to {max(lubridate::date(cpe_cleaned$date_collected))}"))

  for(gen in genotypes){
    susc <<- paste0(gen, '_susceptibility')
    lower <<- paste0(gen, '_lower_bound')
    flex_cpe <- flextable::bg(flex_cpe, ~ as.numeric(stringr::str_replace(eval(parse(text = susc), envir = environment()),'%', '')) >= 50 &
                     as.numeric(stringr::str_replace(eval(parse(text = lower), envir = environment()), '%', '')) >= 50, susc, bg = 'green')
    flex_cpe <- flextable::bg(flex_cpe, ~ as.numeric(stringr::str_replace(eval(parse(text = susc), envir = environment()),'%', '')) >= 50 &
                     as.numeric(stringr::str_replace(eval(parse(text = lower), envir = environment()), '%', '')) < 50, susc, bg = 'yellow')
  }

  flex_cpe <- flextable::border_outer(flex_cpe, part = 'body')
  flex_cpe <- flextable::border_inner_h(flex_cpe, border = fp_border(color="gray", width = 1), part = 'body')
  flex_cpe <- flextable::vline(flex_cpe, j = seq(from = 1, to = length(genotypes) * 4, by = 4), part = 'body')

  flex_cpe

}
