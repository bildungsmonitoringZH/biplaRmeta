# class for meta fields
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 29.05.2020
###############################################################################

#' R6 class representing the information of one meta field
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

            # assign $name
            self$name <- name
            private$read_fields()

            # assign data
            self$data <- data
            self$sanitize()

            # check object
            self$check(silent = FALSE)
        },

        #' @description
        #' Format data to fit field definition
        sanitize = function()
        {
            # update $depth and $class from fields
            private$read_fields()

            # unlist if depth = 1
            if( private$depth == 1 & 'list' %in% class(self$data) ) self$data <- unname(unlist(self$data))

            # wrap in list if depth > 1
            if( private$depth > 1 & !'list' %in% class(self$data) ) self$data <- list(self$data)

            # transform simple characters to LanguageString
            if( private$class == 'LanguageString' & 'character' %in% listClass(self$data)) self$data <- listDefLanguage(self$data)

            # set correct na value, using $is.na and $clear() # TODO

            # crop length of data if neccesary # TODO

            # update $depth to data, return
            private$update_depth()
            invisible(self)
        },

        #' @description
        #' Check values given in $data, update $depth
        #' @param silent (flag) in silent mode, the function returns a logical; in non-silent mode, the function throws errors and returns the object itself
        #' @return flag/self
        check = function(silent = TRUE)
        {
            # parse argument
            assert_that(is.flag(silent))
            assert_that(noNA(silent))

            # update $depth and $class from fields
            private$read_fields()

            # read depth of data, check
            data_depth <- listDepth(list(self$data))
            chk_1 <- data_depth <= private$depth
            assert_that(is.flag(chk_1))
            assert_that(noNA(chk_1))

            if( silent & !chk_1 ) return(FALSE)
            assert_that(chk_1, msg = sprintf('Data with depth <= %.0f expected for field %s.', private$depth, self$name))

            # read class of data, check
            data_class <- listClass(self$data)
            chk_2 <- identical(private$class, data_class)
            assert_that(is.flag(chk_2))
            assert_that(noNA(chk_2))

            if( silent & !chk_2 ) return(FALSE)
            assert_that(chk_2, msg = sprintf('Data with class %s expected for field %s.', private$class, self$name))

            # read top class of data, check
            data_top_class_expected <- switch(as.character(private$depth), '1' = private$class, 'list')
            chk_3 <- data_top_class_expected %in% class(self$data)
            assert_that(is.flag(chk_3))
            assert_that(noNA(chk_3))

            if( silent & !chk_3 ) return(FALSE)
            assert_that(chk_3, msg = sprintf('Data with top class %s expected for field %s.', data_top_class_expected, self$name))

            # update $depth to data, return
            private$update_depth()
            if( silent ) return(TRUE)
            invisible(self)
        },

        #' @description
        #' Remove any data of a MetaField object,
        clear = function()
        {
            private$read_fields()

            self$data <- switch(private$class,
                                'character' = NA_character_,
                                'POSIXct' = as.POSIXct(NA),
                                'LanguageString' = LanguageString$new(en = 'empty'))

            if( private$depth > 1 ) self$data <- list(self$data)
            private$update_depth()
            invisible(self)
        },

        #' @description
        #' Create a text representation
        print_str = function()
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
        # @description
        # Read depth and class from fields table
        read_fields = function()
        {
            idx <- which(fields$field == self$name)
            private$depth <- fields$depth[idx]
            private$length <- fields$length[idx]
            private$class <- as.character(fields$class[idx])
            invisible(self)
        },
        # @description
        # Update actual depth of data
        update_depth = function()
        {
            data_depth <- listDepth(list(self$data))
            if( !identical(data_depth, private$depth) ) private$depth <- data_depth
            invisible(self)
        },
        # @field depth Max depth of field
        depth = 0,
        # @field length Length of field data entries
        length = 0,
        # @field class Class of meta information
        class = NA_character_
    ),
    active = list(
        #' @field list The data of a MetaField in list format
        list = function() {
            list_raw <- list(self$data)
            names(list_raw) <- self$name
            output <- listOutput(list_raw)
        },
        #' @field is.na Check whether the MetaField object contains any data
        is.na = function() { # TODO
            data_flat <- unlist(list(self$data))

            if( identical(self$data, list()) ) return(TRUE)
            if( rlang::has_length(self$data, 0) ) return(TRUE)
            if( nchar(self$data) < 1 ) return(TRUE)
            return(FALSE)
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
        return(max(unlist(unname(lapply(this, listDepth, thisdepth = thisdepth + 1)))))
    }
}

#' @rdname listDepth
#' @keywords internal
#' @return character
listClass <- function(this)
{
    if( length(this) < 1 ) return(intersect(class(this), levels(fields$class)))

    this_wrapped <- switch(class(this)[1],
                           'POSIXct' = this,
                           list(this))
    this_classes <- unique(unlist(purrr::map(unname(unlist(this_wrapped)), ~class(.x))))
    this_classes_intersect <- intersect(this_classes, levels(fields$class))
    if( rlang::is_empty(this_classes_intersect) ) return(this_classes[1])
    return(this_classes_intersect)
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
