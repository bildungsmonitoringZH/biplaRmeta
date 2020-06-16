# test MetaField-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 16.06.2020
###############################################################################

context('test MetaField-class')

test_that("MetaField$new(), argument parsing", {
    expect_error(MetaField$new(name = 22), regexp = 'name is not a string')
    expect_error(MetaField$new(name = letters[1:5]), regexp = 'name is not a string')
    expect_error(MetaField$new(name = NA_character_), regexp = 'name contains . missing values')
    expect_error(MetaField$new(name = 'hubi'), regexp = 'is not a valid MetaField name')

    expect_error(MetaField$new(name = 'units', data = list(a = list(b = 'c'))), regexp = 'Data with depth <= 2 expected for field units')
    expect_error(MetaField$new(name = 'labels', data = list(a = list(b = list(c = list(d = 'e'))))), regexp = 'Data with depth <= 3 expected for field labels')
    expect_error(MetaField$new(name = 'title', data = 22), regexp = 'Data with class LanguageString expected for field title')
    expect_error(MetaField$new(name = 'issued', data = 'hubi'), regexp = 'Data with class POSIXct expected for field issued')
    expect_error(MetaField$new(name = 'dim_order', data = c(22, 33)), regexp = 'Data with class character expected for field dim_order')
})

test_that("MetaField$new(), return value", {
    purrr::walk(res, ~expect_is(.x, 'MetaField'))
    expect_identical(unname(purrr::map_chr(res, ~.x$name)), levels(fields$field))
    purrr::walk(res, ~expect_is(.x$list, 'list'))
    purrr::walk(res, ~expect_is(.x$printf, 'character'))
    expect_identical(unname(purrr::map_chr(res, ~listClass(.x$data) %>% purrr::keep(~.x %in% fields$class))), as.character(fields$class))
    expect_identical(unname(purrr::map_dbl(res, ~listDepth(.x$data)) + 1), c(1,1,1,1,1,2,2,1,3,3,3))
    
    a <- MetaField$new(name = 'source_name', data = LETTERS[1:7])
    expect_length(a$list$source_name$de, 7)
    a$head(4)
    expect_length(a$list$source_name$de, 4)
    
    b <- MetaField$new(name = 'title', data = LETTERS[1:7])
    expect_length(b$list$title$de, 1)
})

test_that("MetaField$check(), argument parsing", {
    a <- MetaField$new(name = 'title', data = 'Titel')
    expect_error(a$check(silent = 22), regexp = 'silent is not a flag')
    expect_error(a$check(silent = as.logical(NA)), regexp = 'silent contains . missing values')
})

test_that("MetaField$check(), return value", {
    a <- MetaField$new(name = 'title', data = 'Titel')
    expect_true(a$check())
    expect_true(a$check(silent = TRUE))
    expect_invisible(b <- a$check(silent = FALSE))
    expect_identical(a, b)

    a$data <- 'neuer Titel'
    expect_false(a$check())
    expect_false(a$check(silent = TRUE))
    expect_error(a$check(silent = FALSE), regexp = 'Data with class LanguageString expected for field title')
})

test_that("MetaField$sanitize(), return value", {
    a <- MetaField$new(name = 'title', data = 'Titel')
    expect_identical(a$list, res$a$list)

    a <- MetaField$new(name = 'description', data = list(all = LanguageString$new(de = 'Beschreibung des ganzen Datensatzes'),
                                                             dim = list(m1 = LanguageString$new(de = 'Beschreibung des ersten Merkmals'),
                                                                        m2 = LanguageString$new(de = 'Beschreibung des zweiten Merkmals')),
                                                             m1 = list(v1 = LanguageString$new(de = c('Beschreibung eines Wertes von m1', 'Hier muss man besonders aufpassen')))))
    b <- MetaField$new(name = 'description', data = list(all = 'Beschreibung des ganzen Datensatzes',
                                                         dim = list(m1 = 'Beschreibung des ersten Merkmals',
                                                                    m2 = 'Beschreibung des zweiten Merkmals'),
                                                         m1 = list(v1 = c('Beschreibung eines Wertes von m1', 'Hier muss man besonders aufpassen'))))
    expect_identical(a$list, b$list)

    a <- MetaField$new(name = 'title', data = list('Titel in Liste'))
    expect_is(a$data, 'LanguageString')
    expect_identical(a$data$list, list(de = 'Titel in Liste'))
    expect_is(MetaField$new(name = 'source_url', data = list('urls' = c('a', 'b')))$data, 'character')
    expect_is(MetaField$new(name = 'issued', data = Sys.time())$data, 'POSIXct')

    b <- MetaField$new(name = 'issued', data = Sys.Date())
    c <- MetaField$new(name = 'issued', data = Sys.time())
    expect_true(all.equal(b$data, c$data, tolerance = 24*60^2))

    b <- MetaField$new(name = 'modified', data = as.POSIXct('2020-03-04'))
    c <- MetaField$new(name = 'modified', data = lubridate::ymd('2020-03-04'))
    expect_true(all.equal(b$data, c$data, tolerance = 24*60^2))
})

test_that("MetaField$clear(), return value", {
    res_clr <- purrr::map(res, ~.x$clear())
    expect_equal(res_clr$a$data, LanguageString$new(de = 'leer'))
    expect_equal(res_clr$b$data, LanguageString$new(de = 'leer'))
    expect_equal(res_clr$c$data, NA_character_)
    expect_equal(res_clr$d$data, as.POSIXct(NA))
    expect_equal(res_clr$e$data, as.POSIXct(NA))
    expect_equal(res_clr$f$data, list(LanguageString$new(de = 'leer')))
    expect_equal(res_clr$g$data, list(NA_character_))
    expect_equal(res_clr$h$data, NA_character_)
    expect_equal(res_clr$i$data, list(NA_character_))
    expect_equal(res_clr$j$data, list(LanguageString$new(de = 'leer')))
    expect_equal(res_clr$k$data, list(LanguageString$new(de = 'leer')))
})

test_that("MetaField$head(), return value, triggered within $initialize()", {
    a <- MetaField$new(name = 'title' , data = 'Titel')
    b <- MetaField$new(name = 'title' , data = c('Titel', 'Untertitel'))
    expect_identical(a$list, b$list)


})

test_that("MetaField$is.na, return value", {
    a <- MetaField$new(name = 'title', data = 'test')
    expect_false(a$is.na)
    a$data$edit(de = 'leer')
    expect_true(a$is.na)

    b <- MetaField$new(name = 'labels', data = list(dimnames = list(f1 = 'erste Spalte', f2 = 'zweite Spalte')))
    expect_false(b$is.na)
    b$data$dimnames$f1$clear()
    expect_false(b$is.na)
    b$data$dimnames$f2$clear()
    expect_true(b$is.na)

    c <- MetaField$new(name = 'source_url', data = '')
    expect_true(c$is.na)
    c$data <- NA_character_
    expect_true(c$is.na)
    c$data <- c('', NA_character_)
    expect_true(c$is.na)
    c$data <- character(0)
    expect_true(c$is.na)
    c$data <- numeric(0)
    expect_true(c$is.na)
    c$data <- list(character(0), '', list(NA_character_))
    expect_true(c$is.na)
    c$data <- list(character(0), '', list(NA_character_, 22))
    expect_false(c$is.na)

    d <- MetaField$new(name = 'issued', data = Sys.time())
    expect_false(d$is.na)
    d$data <- NA
    expect_true(d$is.na)
    d$data <- as.POSIXct(NA)
    expect_true(d$is.na)
    d$data <- list(logical(0), as.POSIXct(NA))
    expect_true(d$is.na)
})

