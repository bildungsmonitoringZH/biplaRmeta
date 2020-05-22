# test languageString-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 22.05.2020
###############################################################################

context('test languageString-class')

test_that("new() parse arguments", {
  expect_error(LanguageString$new(), regexp = 'No arguments given')
})
