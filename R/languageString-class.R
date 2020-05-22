# class for language coded strings
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 22.05.2020
###############################################################################

#' R6 Class representing a string in multiple languages
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr '%>%'
#' @importFrom jsonlite toJSON
#' @importFrom rlang ':=' '.data'
LanguageString <- R6::R6Class(classname = "LanguageString",
                              public = list(
                                  #' @description
                                  #' Create a LanguageString object
                                  #' @param ... Strings named with two letter language code
                                  #' @return A new `LanguageString` object.
                                  initialize = function(...) {
                                      self$edit(...)
                                  },

                                  #' @description
                                  #' Edit the data of a LanguageString object
                                  #' @param ... Strings named with two letter language code
                                  edit = function(...) {
                                      args <- parseLangArgs(...)
                                      purrr::iwalk(args, ~private$edit_single(lang = .y, str = .x))
                                      private$data <- orderLangArgs(private$data)
                                      invisible(self)
                                  }
                              ),
                              private = list(
                                  data = list(),
                                  edit_single = function(lang, str) {
                                      private$data[[lang]] <- str
                                  },
                                  xml_single = function(lang, str) {
                                      sprintf('<xml_tag xml:lang="%s">%s</xml_tag>', lang, str)
                                  }),
                              active = list(
                                  #' @field list The data of a LanguageString in list format
                                  list = function() { return(private$data) },
                                  #' @field json The data of a LanguageString in json format
                                  json = function() { return(toJSON(self$list)) },
                                  #' @field xml The data of a LanguageString in xml format
                                  xml = function() { return(unname(purrr::imap_chr(self$list, ~private$xml_single(.y, .x)))) }
                              ))

# aux functions

#' parse language string arguments
#' @keywords internal
#' @param ... arguments
#' @return list of language strings
parseLangArgs <- function(...)
{
    # parse arguments
    args <- rlang::list2(...)
    assert_that(length(args) > 0, msg ='No arguments given.')
    assert_that(!rlang::is_null(names(args)), msg = 'No named arguments given.')
    purrr::walk2(names(args), seq_along(names(args)), ~assert_that(nchar(.x) > 0, msg = sprint('Argument %d is not named.', .y)))
    purrr::walk(args, ~assert_that(is.string(.x), msg = sprintf('%s is not a string (a length one character vector).', .x)))

    # keep only non empty arguments
    args <- replace(args, which(nchar(args) < 1), NA_character_) %>%
        purrr::keep(~noNA(.x))
    purrr::walk2(args, seq_along(names(args)), ~assert_that(noNA(.x), msg = sprint('Argument %d is empty.', .y)))
    purrr::walk(names(args), ~assert_that(.x %in% langs$code, msg = sprintf('%s is not a valid language code.', .x)))

    # return
    return(args)
}

#' @rdname parseLangArgs
#' @keywords internal
orderLangArgs <- function(...)
{
    # parse arguments
    args <- rlang::list2(...)[[1]]

    # get common langs
    langs <- intersect(langs$code, names(args))

    # return
    return(args[langs])
}
