# test MetaInfo-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 28.05.2020
###############################################################################

context('test MetaInfo-class')

test_that("MetaInfo$new(), return value", {
    m <- MetaInfo$new(title = 'Titel')
})
