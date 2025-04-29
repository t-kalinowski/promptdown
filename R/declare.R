#' @import rlang
NULL


# get_current_weather <- function(location = "") {
#   declare(tool(
#     r"(Get the current weather)",
#     location = type_string(
#       "If missing, defaults to the current user location",
#       required = TRUE
#     )
#   ))
#   "sunny"
# }

#' Find the first tool() declaration in a function's top-level declare() calls
#'
#' @param fn A function to inspect
#' @return The tool(...) call if found, otherwise NULL
find_tool_declaration <- function(fn) {
  if (!is.function(fn)) return()
  body <- fn_body(fn)
  exprs <- if (is_call(body, "{")) {
    call_args(body)
  } else {
    stop("body must start with {")
  }

  # Iterate over each top-level expression
  for (expr in exprs) {
    if (is_call(expr, "declare")) {
      for (declare_arg in call_args(expr)) {
        if (is_call(declare_arg, "tool")) {
          return(declare_arg)
        }
      }
    }
  }

  # No tool() declaration found
  NULL
}


if (FALSE) {
  f = get_current_weather
  cl <- find_tool_declaration(f)
  cl$.fun <- f
  call_modify(cl, .fun = )
  tool <- NULL
  # for(expr in )
}


chat_sheet <- function(...) {
  browser()
}
