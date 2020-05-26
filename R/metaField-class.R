# class for meta fields
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 26.05.2020
###############################################################################

#' R6 class representing the information of one
MetaField <- R6::R6Class(
    classname = 'MetaField',
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
            assert_that(name %in% fields$field, msg = sprintf('\'%s\' is not a valid MetaField name.', name))

            idx <- which(fields$field == name)
            private$depth <- fields$depth[idx]
            private$class <- fields$class[idx]

            data_depth <- listDepth(list(data))
            data_class <- listClass(list(data))

            assert_that(data_depth <= private$depth, msg = sprintf('Data argument with depth <= %d expected for field %s.', private$depth, name))
            private$depth <- data_depth

            if( private$class == 'LanguageString' & 'character' %in% data_class) data <- listDefLanguage(data)
            assert_that(private$class %in% listClass(list(data)), msg = sprintf('Data argument with class %s expected for field %s.', private$class, name))

            # assign
            self$data <- data
            self$name <- name
        },
        #' @description
        #' Create a text representation
        #' @return string
        print = function()
        {
            str_oneline <- sprintf('%s: list with class %s and depth %.0f', self$name, private$class, private$depth)
            cat(str_oneline, "\n")
            invisible(self)
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
        list = function() {
            list_raw <- list(self$data)
            names(list_raw) <- self$name
            output <- listOutput(list_raw)
        }
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

#' @rdname listDepth
#' @keywords internal
#' @return character
listDefLanguage <- function(this)
{
    if( is.character(this) )
    {
        ls <- do.call(LanguageString$new, args = setNames(list(this), as.character(langs$code[1])))
        return(ls)
    } else if(is.list(this) ) {
        return(lapply(this, listDefLanguage))
    } else {
        return(this)
    }
}

listOutput <- function(this)
{
    if( is.list(this) )
    {
        return(lapply(this, listOutput))
    } else if( is(this, 'LanguageString') ) {
        return(this$list)
    } else {
        return(this)
    }
}

# a <- jsonlite::read_json('C:\\Users\\B251PFI\\Documents\\Rworkspace\\swissdata_demo\\data\\ch_adecco_sjmi.json')
