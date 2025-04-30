## This generic is currently unused after the latest refactor,
## but we might want to bring it back to allow extenders
format_for_sheet <- S7::new_generic("format_for_sheet", "x")

S7::method(format_for_sheet, S7::class_list) <- function(x) {
  purrr::map_chr(x, format_for_sheet)
}

S7::method(format_for_sheet, ellmer::ContentToolResult) <- function(x) {
  if (is.null(x@error)) {
    stri_flatten(
      c(
        "```tool-call-result",
        stri_c("#| call-id: ", x@id),
        x@value,
        "```"
      ),
      "\n"
    )
  } else {
    stri_flatten(
      c(
        "```tool-call-error",
        stri_c("#| call-id: ", x@id),
        x@error,
        "```"
      ),
      "\n"
    )
  }
}

S7::method(format_for_sheet, ellmer::ContentText) <- function(x) {
  x@text
}

S7::method(format_for_sheet, ellmer::ContentToolRequest) <- function(x) {
  cl <-
    rlang::call2(x@name, !!!x@arguments) |>
    rlang::expr_deparse(width = 30) |>
    stri_flatten("\n")
  stri_flatten_lines(
    "```tool-call",
    stri_c("#| call-id: ", x@id),
    cl,
    "```"
  )
}
