# test LanguageString-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 25.05.2020
###############################################################################

context('test LanguageString-class')

test_that("LanguageString$new(), parse arguments", {
    expect_error(LanguageString$new(), regexp = 'No arguments given')
    expect_error(LanguageString$new('a'), regexp = 'No named arguments given')
    expect_error(LanguageString$new(de = 'a', 'b'), regexp = 'Argument 2 is not named')
    expect_error(LanguageString$new(de = FALSE), regexp = 'Argument .de. is not a character')
    expect_error(LanguageString$new(de = list('a' = list('b', 'c'))), regexp = 'Argument .de. is not a character')
    expect_error(LanguageString$new(de = NA_character_), regexp = 'Argument 1 contains empty value')
    expect_error(LanguageString$new(de = 'a', fr = ''), regexp = 'Argument 2 contains empty value')
    expect_error(LanguageString$new(de = c('a', NA, 'c')), regexp = 'Argument 1 contains empty value')
    expect_error(LanguageString$new(cz = c('a', 'c')), regexp = '.cz. is not a valid language code')

    expect_warning(LanguageString$new(it = 'a', de = c('c', 'd', 'e'))$list, regexp = 'Not all language strings have length 3')
})

test_that("LanguageString$edit(), parse arguments", {
    a <- LanguageString$new(de = 'a')
    expect_error(a$edit(), regexp = 'No arguments given')
    expect_error(a$edit('a'), regexp = 'No named arguments given')
    expect_error(a$edit(de = 'a', 'b'), regexp = 'Argument 2 is not named')
    expect_error(a$edit(de = FALSE), regexp = 'Argument .de. is not a character')
    expect_error(a$edit(de = list('a' = list('b', 'c'))), regexp = 'Argument .de. is not a character')
    expect_error(a$edit(de = NA_character_), regexp = 'Argument 1 contains empty value')
    expect_error(a$edit(de = 'a', fr = ''), regexp = 'Argument 2 contains empty value')
    expect_error(a$edit(de = c('a', NA, 'c')), regexp = 'Argument 1 contains empty value')
    expect_error(a$edit(cz = c('a', 'c')), regexp = '.cz. is not a valid language code')

    expect_warning(a$edit(it = 'a', de = c('c', 'd', 'e'))$list, regexp = 'Not all language strings have length 3')
})

test_that("LanguageString$new(), return value", {
    a <- expect_warning((list(LanguageString$new(de = 'a'),
              LanguageString$new(it = 'c', de = 'a'),
              LanguageString$new(en = letters[3:5], de = 'a'))))
    purrr::walk(a, ~expect_is(.x, 'LanguageString'))
    purrr::walk(a, ~expect_identical(names(.x$list)[1], 'de'))
    expect_identical(purrr::map_dbl(a[[3]]$list, ~length(.x)), c('de' = 1, 'en' = 3))
    purrr::walk(a, ~expect_true(str_detect(.x$json, stringr::fixed('{"de":["a"]'))))
    purrr::walk(a, ~expect_true(str_detect(.x$xml[1], stringr::fixed('<xml_tag xml:lang="de">a</xml_tag>'))))
    expect_true(str_detect(a[[3]]$xml[2], stringr::fixed('c <br />d <br />e')))

    expect_is(do.call(LanguageString$new, args = list('de' = 'a')), 'LanguageString')
})
