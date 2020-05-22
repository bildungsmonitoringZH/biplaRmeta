# prepare package data
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 22.05.2020
###############################################################################

# fields
fields <- read.csv2(system.file('def', 'fields.csv', package = 'biplaRmeta'),
                    encoding = 'UTF-8',
                    as.is = TRUE) %>%
    mutate_all(~replace(.x, which(nchar(.x) == 0), NA)) %>%
    mutate_at(vars('field':'type'), fct_inorder)

langs <- read.csv2(system.file('def', 'langs.csv', package = 'biplaRmeta'),
                    encoding = 'UTF-8',
                    as.is = TRUE) %>%
    mutate_at(vars('code'), fct_inorder)

usethis::use_data(fields, langs, overwrite = TRUE, internal = TRUE)
