# test MetaInfo-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 11.06.2020
###############################################################################

context('test MetaInfo-class')

test_that("MetaInfo$new(), return value", {
    m <- list(MetaInfo$new(title = 'Titel'),
           MetaInfo$new(title = 'Titel', source_name = LanguageString$new('fr' = 'Data')),
           MetaInfo$new(title = 'Titel', source_url = 'https://bi.zh.ch/kindergartenbericht'),
           MetaInfo$new(title = 'Titel', issued = as.Date('2020-06-11')),
           MetaInfo$new(title = 'Titel', modified = as.POSIXct('2020-06-11 14:20:00')),
           MetaInfo$new(title = 'Titel', units = list('m1' = 'Grad Celsius')),
           MetaInfo$new(title = 'Titel', aggregate = list('m1' = 'sum')))
    
    m2 <- MetaInfo$new()
    m2$modified$edit(data = as.POSIXct(NA))
    
    purrr::walk(m, ~assert_that(is(.x, 'MetaInfo')))
    purrr::walk(m, ~assert_that(rlang::is_false(.x$is.na)))
    purrr::walk(m, ~assert_that(is(.x$printf, 'character')))
    expect_true(m2$is.na)
    purrr::walk(m, ~assert_that(is(.x$list, 'list')))
    expect_identical(purrr::map_dbl(m, ~length(.x$list)), c(2,3,3,3,2,3,3))
})
