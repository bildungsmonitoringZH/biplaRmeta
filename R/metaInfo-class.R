# class for meta information
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 28.05.2020
###############################################################################

#' R6 class representing the information of one meta field
MetaInfo <- R6::R6Class(
    classname = 'MetaInfo',
    public = list(
        #' @description
        #' Create a MetaInfo object
        #' @param ... Informations to populate the meta fields
        #' @examples
        #' m <- MetaInfo$new(title = 'Titel')
        #' @return A new `MetaInfo` object.
        initialize = function(...)
        {
            # parse arguments
            args <- rlang::list2(...)
        },
        #' @field title Title of the dataset
        title = NA
        #' @field source_name Publishers of the dataset.
        # source_name = MetaField$new(name = 'source_name', data = 'Kanton Zürich, Bildungsplanung'),
        #' @field source_url URL where the dataset can be found, or URL for the publishers
        # source_url = MetaField$new(name = 'source_url', data = ''),
        #' @field issued Date/Time of the first publication of this dataset
        # issued = MetaField$new(name = 'issued', data = as.POSIXct(NA)),
        #' @field modified Date/Time of the last modification of this dataset
        # modified = MetaField$new(name = 'modified', data = as.POSIXct(NA)),
        #' @field units Labels of the datasets' values
        # units = NULL,
        #' @field aggregate Information on the aggregation of the datasets' values
        # aggregate = MetaField$new(name = 'aggregate', data = ''),
        #' @field dim_order Ordering of the dataset
        # dim_order = MetaField$new(name = 'dim_order', data = ''),
        #' @field hierarchy Sorting and hierarchical structure of the values within features
        # hierarchy = MetaField$new(name = 'hierarchy', data = ''),
        #' @field labels Labels of features and of values within features
        # labels = NULL,
        #' @field description In depth information on the dataset, on features and on values within features
        # description = NULL
    ))
