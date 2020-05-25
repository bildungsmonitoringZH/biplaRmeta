# class for meta fields
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 25.05.2020
###############################################################################

#' R6 class representing the information of one
MetaField <- R6::R6Class(classname = 'MetaField',
                         public = list(
                             #' @description
                             #' Create a MetaField object
                             #' @param name (string) Name of the field
                             #' @param data Value of the field
                             #' @examples
                             #' f <- MetaField$new(name = 'source_url', data = 'https://bi.zh.ch/bildungsplanung')
                             #' f$list
                             #' @return A new `MetaField` object.
                             initialize = function(name, data)
                             {
                                 # parse arguments
                                 assert_that(is.string(name))
                                 assert_that(noNA(name))
                                 assert_that(name %in% fields$field)

                                 idx <- which(fields$field == name)
                                 private$depth <- fields$depth[idx]
                                 private$class <- fields$class[idx]

                                 assert_that(listDepth(list(data)) <= private$depth, msg = sprintf('Data argument with depth <= %d expected for field %s.', private$depth, name))
                                 private$depth <- listDepth(list(data))
                                 assert_that(private$class %in% listClass(list(data)), msg = sprintf('Data argument with class %s expected for field %s.', private$class, name))

                                 # assign
                                 self$data <- data
                                 self$name <- name
                             },
                             #' @field name Actual name of the field
                             name = 'none',
                             #' @field data Actual data of the field
                             data = list()
                             ),
                         private = list(
                             # @field depth Max depth of field
                             depth = 0,
                             # @field class Class of meta information
                             class = 'none'
                         ),
                         active = list(
                             #' @field list The data of a MetaField in list format
                             list = function() { setNames(object = list(self$data), self$name) }
                         )

)

# aux functions

#' get max depth of a nested list
#' @keywords internal
#' @param this object to check
#' @param thisdepth current depth
#' @return integer
listDepth <- function(this, thisdepth = 0)
{
    if( !is.list(this) ) {
        return(thisdepth)
    } else if( rlang::is_empty(this) ) {
        return(thisdepth)
    } else {
        return(as.integer(max(unlist(unname(lapply(this, listDepth, thisdepth = thisdepth + 1))))))
    }
}

#' @rdname listDepth
#' @keywords internal
#' @return character
listClass <- function(this)
{
    if( !is.list(this) )
    {
        return(class(this))
    } else if( rlang::is_empty(this) ) {
        return(class(this))
    } else {
        return(unique(unname(unlist(lapply(this, listClass)))))
    }
}


# TODO next: Wrapper für einfache Eingabe von LanguageString$new, siehe test-MetaField.R:26
# a <- jsonlite::read_json('C:\\Users\\B251PFI\\Documents\\Rworkspace\\swissdata_demo\\data\\ch_adecco_sjmi.json')
