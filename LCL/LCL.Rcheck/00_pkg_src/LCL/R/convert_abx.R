## this script aims to create two objects of antibiotic and organism names for use as lookups for codes ##
convert_abx <- function(.data) {

abx <- read_csv('data/abx.csv')

abx_codes <- abx$Antibiotic_Exp
abx_codes <- abx_codes %>% str_replace_all(c('Amp/Amoxil' = 'Ampicillin',
                                             "Eryth/Clarith." = 'Erythromycin',
                                             'Ceftolozane-tazobactam' = 'Ceftolozane/tazobactam',
                                             'Caz/Avi' = 'Ceftazidime/avibactam'))
names(abx_codes) <- abx$Antibiotic_Code
unname(abx_codes[.data])
}

#orgs <- read_csv('orgs.csv')
#orgs$Expansion <- str_remove_all(orgs$Expansion, ' sp.*| \\(.*')
#orgs$Expansion <- str_replace_all(orgs$Expansion, c('Dermobacter' = 'Dermabacter'))

#org_codes <- orgs$Expansion
#names(org_codes) <- orgs$Code
