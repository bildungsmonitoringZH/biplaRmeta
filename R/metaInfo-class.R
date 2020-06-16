# class for meta information
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 16.06.2020
###############################################################################

#' R6 class representing the information of one data entity
#' @export
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
            idx <- which(names(args) %in% fields$field)
            args <- args[idx]
            
            # setup of empty fields
            for(nm in as.character(fields$field))
            {
                self[[nm]] <- MetaField$new(name = nm, data = NA)
            }
            
            # apply default values
            self$modified$edit(data = Sys.time())

            # apply arguments
            for(nm in names(args)) {
                self[[nm]] <- MetaField$new(name = nm, data = get(nm, args))
            }
        },
        
        #' @description 
        #' Print representation of the MetaField object
        print = function() {
            cat(self$printf, sep = "\n")
            invisible(self)
        },
        
        #' @field title Title of the dataset
        title = NA,
        #' @field source_name Publishers of the dataset.
        source_name = NA,
        #' @field source_url URL where the dataset can be found, or URL for the publishers
        source_url = NA,
        #' @field issued Date/Time of the first publication of this dataset
        issued = NA,
        #' @field modified Date/Time of the last modification of this dataset
        modified = NA,
        #' @field units Labels of the datasets' values
        units = NA,
        #' @field aggregate Information on the aggregation of the datasets' values
        aggregate = NA,
        #' @field dim_order Ordering of the dataset
        dim_order = NA,
        #' @field hierarchy Sorting and hierarchical structure of the values within features
        hierarchy = NA,
        #' @field labels Labels of features and of values within features
        labels = NA,
        #' @field description In depth information on the dataset, on features and on values within features
        description = NA
    ),
    active = list(
        #' @field list The data of the MetaInfo object in list format
        list = function() {
            lst <- list()
            for(nm in as.character(fields$field) ) {
                if( !self[[nm]]$is.na ) {
                    lst <- lst %>% append(self[[nm]]$list)
                }
            }
            return(lst)
        },
        #' @field json The data of the MetaInfo object in json format
        json = function() { if( self$is.na ) { NA_character_ } else { toJSON(self$list) } },
        #' @field is.na Check whether the MetaInfo object contains any data
        is.na = function() {
            all(purrr::map_lgl(as.character(fields$field), ~self[[.x]]$is.na))
        },
        #' @field printf representation of the MetaInfo Object
        printf = function() {
            if( self$is.na ) { return(NA_character_) }
            str <- character(0)
            for(nm in as.character(fields$field) ) {
                if( !self[[nm]]$is.na ) {
                    str <- c(str, self[[nm]]$printf)
                }
            }
            return(str)
        }
            
    ))
