#git config --global user.email "you@example.com"
#git config --global user.name "Your Name"

abx_data_from_tpath <- read_csv('data/antibiotic_codes.csv')
org_data_from_tpath <- read_csv('data/organism_codes.csv')

save.image('R/sysdata.rda')
