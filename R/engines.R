on_load({
  on_package_load("knitr", {
    knitr::knit_engines$set(btw = eng_btw, tool = eng_tool)
  })
})

eng_btw <- function(options) {
  args <- unlist(stri_split_regex(options$code, "[, \\s]", omit_empty = TRUE))
  args <- lapply(args, function(x) {
    if (
      startsWith(x, "@") || # @whatever
        startsWith(x, "./") || # ./file/path
        startsWith(x, "?") || # ?dplyr::across
        stri_detect_regex(x, "^\\{[a-zA-Z0-9.]+\\}$") # {dplyr}
    ) {
    } else {
      x <- str2lang(x)
    }
    x
  })
  res <- do.call(
    btw::btw,
    c(args, clipboard = FALSE),
    envir = knitr::knit_global()
  )
  res <- ellmer::contents_markdown(res)
  res <- stri_flatten(res, "\n")

  # ## remove heading
  # res <- stri_replace_first_regex(res, "^## Context\\s*", "")
  # TODO: support option: {btw, heading = FALSE} or {heading = "my heading"}

  options$results <- 'asis'
  options$echo <- FALSE
  knitr::engine_output(options, NULL, res)
}


eng_tool <- function(options) {
  # # TODO: tool calling code is duplicated in a few places.
  # # refactor the engine to use:
  #   request <- parse_tool_request(options$code)
  calls <- str2expression(options$code)
  if (length(calls) != 1) stop("only one tool call permitted per chunk")

  tool_call <- calls[[1]]

  if (!is_call_simple(tool_call, ns = FALSE))
    stop(
      "tool call must be a simple R call, received: ",
      deparse1(tool_call)
    )
  env <- knitr::knit_global()

  # get arguments
  # ensure tool call receives evaluated args
  tool_name <- as.character(tool_call[[1]])
  get_args_call <- tool_call
  get_args_call[[1]] <- quote(base::list)
  args <- eval(get_args_call, env)

  # get the ToolDef
  tool_def <- find_tool(env, tool_name)

  result <- get_tool_result(tool_def, args, env)
  string <- ellmer:::tool_string(result)

  # some checks
  if (inherits(string, "AsIs")) {
    stop("I() wrapped tool result not supported by chat sheet")
  }
  if (!is_string(string)) {
    stop("Failed to format tool result as a string.")
  }
  options$echo <- TRUE
  # append(options$class.output) <- "content-tool-result"
  if (!is.null(result@error)) {
    append(options$class.output) <- "error"
  } else {
    if (inherits(string, "json")) {
      append(options$class.output) <- "json"
    }
  }

  knitr::engine_output(options, deparse1(tool_call), string)
}
