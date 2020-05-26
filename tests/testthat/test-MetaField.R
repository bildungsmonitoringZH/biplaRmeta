# test MetaField-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 26.05.2020
###############################################################################

context('test MetaField-class')

test_that("MetaField$new(), argument parsing", {
    expect_error(MetaField$new(name = 22), regexp = 'name is not a string')
    expect_error(MetaField$new(name = letters[1:5]), regexp = 'name is not a string')
    expect_error(MetaField$new(name = NA_character_), regexp = 'name contains . missing values')
    expect_error(MetaField$new(name = 'hubi'), regexp = 'is not a valid MetaField name')

    expect_error(MetaField$new(name = 'title', data = list(a = list(b = 'c'))), regexp = 'argument with depth <= 1 expected for field title')
    expect_error(MetaField$new(name = 'labels', data = list(a = list(b = list(c = list(d = 'e'))))), regexp = 'argument with depth <= 3 expected for field labels')
    expect_error(MetaField$new(name = 'title', data = 22), regexp = 'argument with class LanguageString expected for field title')
    expect_error(MetaField$new(name = 'issued', data = 'hubi'), regexp = 'argument with class POSIXct expected for field issued')
})

test_that("MetaField$new(), return value", {
    res <- list()
    res$a <- MetaField$new(name = 'title', data = LanguageString$new(de = 'Titel'))
    res$b <- MetaField$new(name = 'source_name', data = LanguageString$new(de = 'Bildungsplanung Kanton Zürich'))
    res$c <- MetaField$new(name = 'source_url', data = 'https://bi.zh.ch/bildungsplanung')
    res$d <- MetaField$new(name = 'issued', data = lubridate::ymd_hm('2020-05-25 2023'))
    res$e <- MetaField$new(name = 'modified', data = Sys.time())
    res$f <- MetaField$new(name = 'units', data = list('all' = LanguageString$new(en = 'Count')))
    res$g <- MetaField$new(name = 'aggregate', data = list(m1 = 'mean', m2 = 'sum'))
    res$h <- MetaField$new(name = 'dim_order', data = c('m1', 'm3', 'm2'))
    res$i <- MetaField$new(name = 'hierarchy', data = list(ma = 'm1', mb = list('m2', mc = 'm3')))
    res$j <- MetaField$new(name = 'labels', data = list(dimnames = list(m1 = LanguageString$new(de = 'erstes Merkmal'),
                                                                    m2 = LanguageString$new(en = 'second feature')),
                                                    m1 = list(v1 = LanguageString$new(de = 'erster Wert des ersten Merkmals'))))
    res$k <- MetaField$new(name = 'description', data = list(all = LanguageString$new(de = 'Beschreibung des ganzen Datensatzes'),
                                                         dim = list(m1 = LanguageString$new(en = 'Description of the first feature'),
                                                                    m2 = LanguageString$new(fr = 'Description de la deuxieme caracteristique')),
                                                         m1 = list(v1 = LanguageString$new(de = c('Beschreibung eines Wertes von m1', 'Hier muss man besonders aufpassen')))))
    res$l <- MetaField$new(name = 'description', data = list(all = 'Beschreibung des ganzen Datensatzes',
                                                         dim = list(m1 = 'Beschreibung des ersten Merkmals',
                                                                    m2 = 'Beschreibung des ersten Merkmals'),
                                                         m1 = list(v1 = c('Beschreibung eines Wertes von m1', 'Hier muss man besonders aufpassen'))))
    purrr::walk(res, ~expect_is(.x, 'MetaField'))
    expect_identical(unname(purrr::map_chr(res, ~.x$name)), levels(fields$field)[c(seq_along(levels(fields$field)), nlevels(fields$field))])
    purrr::walk(res, ~expect_is(.x$list, 'list'))
    expect_identical(unname(purrr::map_chr(res, ~listClass(.x$data) %>% purrr::keep(~.x %in% fields$class))), as.character(fields$class)[c(seq(1, nrow(fields)), nrow(fields))])
    expect_identical(unname(purrr::map_dbl(res, ~listDepth(.x$data)) + 1), c(1,1,1,1,1,2,2,1,3,3,3,3))
})

tbl <- mtcars
attr(tbl, 'meta') <- res$a
