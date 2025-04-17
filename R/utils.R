
#' @import ragnar
#' @import stringi
#' @importFrom dplyr if_else mutate group_by summarise arrange
NULL

last <- function(x) x[[length(x)]]
drop_last <- function(x) x[-length(x)]
drop_first <- function(x) x[-1L]
drop_nulls <- function(x) x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)]


replace_val <- function (x, old, new) {
  if (!is_scalar(new))
    stop("Unexpected length of replacement value in replace_val().\n",
         "`new` must be length 1, not ", length(new))
  x[x %in% old] <- new
  x
}

seq_along0 <- function(x) 0L:(length(x) - 1L)

is_scalar <-  function(x) identical(length(x), 1L)


stri_flatten_lines <- function(...) {
  stri_flatten(unlist(c(...)), "\n", na_empty = TRUE)
}

timestamp_filename <-
  function (prefix = "", ext = NULL, format = "%Y-%m-%d_%Hh%Mm%OS3s")
  {
    ts <- sub(".", "-", format(Sys.time(), format = format),
              fixed = TRUE)
    if (!is.null(ext))
      ext <- sub("\\.?(.*)$", ".\\1", ext, perl = TRUE)
    paste0(prefix, ts, ext)
  }
