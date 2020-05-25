# class for language coded strings
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 25.05.2020
###############################################################################

#' R6 Class representing a string or a character vector in multiple languages
#'
#' @importFrom assertthat assert_that noNA
#' @importFrom dplyr '%>%'
#' @importFrom jsonlite toJSON
#' @importFrom rlang ':=' '.data'
#' @importFrom stringr str_c str_detect
LanguageString <- R6::R6Class(classname = "LanguageString",
                              public = list(
                                  #' @description
                                  #' Create a LanguageString object
                                  #' @param ... character arguments, named with two letter language code
                                  #' @examples
                                  #' s <- LanguageString$new(de = 'Hallo Welt', en = 'Hello World')
                                  #' s$list
                                  #' @return A new `LanguageString` object.
                                  initialize = function(...) {
                                      self$edit(...)
                                  },

                                  #' @description
                                  #' Edit the data of a LanguageString object
                                  #' @examples
                                  #' s <- LanguageString$new(de = 'Hallo Welt')
                                  #' s$edit(en = 'Hello World')
                                  #' s$list
                                  #' @param ... Strings named with two letter language code
                                  edit = function(...) {
                                      args <- parseLangArgs(...)
                                      purrr::iwalk(args, ~private$edit_single(lang = .y, chr = .x))
                                      private$data <- orderLangArgs(private$data)
                                      private$length <- lengthLang(private$data)
                                      invisible(self)
                                  }
                              ),
                              private = list(
                                  # @field data Object data
                                  data = list(),

                                  # @description
                                  # Edit the value of a single language value
                                  # @param lang (string) language code
                                  # @param chr (character) value
                                  edit_single = function(lang, chr) {
                                      private$data[[lang]] <- chr
                                  },

                                  # @field length Max length of language strings in `$data`
                                  length = 0,

                                  # @description
                                  # Create a single xml element with language value
                                  # @param lang (string) language code
                                  # @param chr (character) value
                                  xml_single = function(lang, chr) {
                                      sprintf('<xml_tag xml:lang="%s">%s</xml_tag>', lang, str_c(chr, collapse = " <br />"))
                                  }),
                              active = list(
                                  #' @field list The data of a LanguageString in list format
                                  list = function() { return(private$data) },
                                  #' @field json The data of a LanguageString in json format
                                  json = function() { return(toJSON(self$list)) },
                                  #' @field xml The data of a LanguageString in xml format
                                  xml = function() { return(unname(purrr::imap_chr(self$list, ~private$xml_single(lang = .y, chr = .x)))) }
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
    purrr::walk2(names(args), seq_along(names(args)), ~assert_that(nchar(.x) > 0, msg = sprintf('Argument %d is not named.', .y)))
    purrr::iwalk(args, ~assert_that(is.character(.x), msg = sprintf('Argument \'%s\' is not a character.', .y)))

    args <- purrr::map(args, ~replace(.x, which(nchar(.x) < 1), NA_character_))
    purrr::walk2(args, seq_along(names(args)), ~assert_that(noNA(.x), msg = sprintf('Argument %d contains empty value(s)', .y)))

    purrr::walk(names(args), ~assert_that(.x %in% langs$code, msg = sprintf('\'%s\' is not a valid language code.', .x)))

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

#' @rdname parseLangArgs
#' @keywords internal
lengthLang <- function(...)
{
    # parse arguments
    args <- rlang::list2(...)[[1]]

    # get length
    length_all <- purrr::map_int(args, ~length(.x))
    length_max <- max(length_all, na.rm = TRUE)

    # throw warning, return
    if( !rlang::has_length(unique(length_all), 1) ) { warning(sprintf('Not all language strings have length %d', length_max)) }
    return(length_max)
}
