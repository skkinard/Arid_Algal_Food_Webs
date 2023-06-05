# # Create master site by community dataframe to be used in conjunction with stable isotope data from 3-site pilot collections during the summer of 2019
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

# load data 
d_i <- read_csv('Rdat/tidy_invert2.csv')
d_f <- read_csv('Rdat/tidy_fish.csv')

#----------------------------------------------------------------------------
# Merge Datasets
#----------------------------------------------------------------------------

d_i <- d_i %>%
  mutate(guild = 'invertebrate')

d_f <-  d_f %>%
  mutate(guild = 'fish',
         lowest_taxon = paste(substr(genus, 1, 1), species, sep='.'))

mster <- full_join(d_f, d_i) %>%
  mutate(order = str_to_title(order),
         family = str_to_title(family),
         genus = str_to_title(genus),
         lowest_taxon = str_to_title(lowest_taxon))
#----------------------------------------------------------------------------
# Filter Site and Dates
#----------------------------------------------------------------------------
mster <- mster %>%
  filter(site == 'SFC' | site == 'AR' | site == 'GC') %>%
  filter(date > as_date('2018-04-29') &  date < as_date('2018-07-20')) %>%
  mutate(site = ifelse(site=='SFC', 'Semi-Arid',
                       ifelse(site == 'AR', 'Transition', 
                              ifelse(site=='GC', 'Sub-Humid', NA)))) %>%
  mutate(site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  mutate(month = ifelse(as_date(date) < '2018-06-01', 'May',
                        ifelse(as_date(date) < '2018-06-30', 'June', 'July' ))) %>%
  mutate(month = fct_relevel(month, 'July', 'June', 'May'),
         site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  filter(!is.na(order)) %>%
  filter(! order %in% c('Hymenoptera', 'Lepidoptera', 'Cladocera')) %>%
  filter(! is.na(lowest_taxon))

#----------------------------------------------------------------------------
# Export
write_csv(mster, 'Rdat/communities.csv')

#----------------------------------------------------------------------------
# End















