eng_btw <- function(options) {
  args <- unlist(stri_split_regex(options$code, "[, \\s]", omit_empty = TRUE))
  args <- lapply(args, function(x) {
    if (
      stri_startswith_fixed(x, "@") || # @whatever
        stri_startswith_fixed(x, "./") || # ./file/path
        stri_startswith_fixed(x, "?") || # ?dplyr::across
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
  # res <- stri_replace_first_regex(res, "^## Context\\s*", "")
  options$results <- 'asis'
  options$echo <- FALSE
  knitr::engine_output(options, NULL, res)
}


eng_tool <- function(options) {
  calls <- str2expression(options$code)
  if (length(calls) != 1) stop("only one tool call permitted per chunk")
  # lapply(calls, function(cl) { })
  tool_call <- calls[[1]]
  # browser()

  if (!is_call_simple(tool_call, ns = FALSE))
    stop(
      "tool call must be a simple R call, received: ",
      deparse1(tool_call)
    )
  env <- knitr::knit_global()

  tool_name <- as.character(tool_call[[1]])
  get_args_call <- tool_call
  get_args_call[[1]] <- quote(base::list)
  # browser()
  args <- eval(get_args_call, env) # ensure tool call receives evaluated args
  tool_def <- find_tool(env, tool_name)
  result <- get_tool_result(tool_def, args, env)
  string <- ellmer:::tool_string(result)
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
  # browser()

  knitr::engine_output(options, deparse1(tool_call), string)
}

text = r"(``` tool
get_current_weather(location = "new york")
```)"


# ``
# ` error
# Tool calling failed with error Unknown tool
# `
# ``

parse_tool_chunk <- function(text, env = baseenv()) {
  # browser()
  if (stri_startswith_fixed(text, "```")) {
    text <- stri_match_first_regex(
      text,
      "^```[^\n]*\n(.*)```$",
      dot_all = TRUE
    )[, 2L]
  }
  tool_call <- str2lang(text)

  name <- as.character(tool_call[[1]])
  get_args_call <- tool_call
  get_args_call[[1]] <- quote(base::list)
  args <- eval(get_args_call, env) # ensure tool call receives evaluated args
  ContentToolRequest(id = uuid::UUIDgenerate(), name = name, arguments = args)
}


get_tool_result <- function(tool_def, args, env) {
  if (!inherits(tool_def, "ellmer::ToolDef"))
    return(ContentToolResult(error = "Unknown tool"))

  tryCatch(
    {
      result <- do.call(tool_def@fun, args, envir = env)
      ContentToolResult(value = result)
    },
    error = function(e) {
      ellmer::ContentToolResult(error = e)
    }
  )
}


as_tool_def <- function(fun, name = deparse1(substitute(fun))) {
  if (!is.function(fun)) return()

  tool_cl <- find_tool_declaration(fun)
  if (is.null(tool_cl)) return()

  tool_cl[[".fun"]] <- fun
  tool_cl[[".name"]] <- name
  withr::with_package("ellmer", {
    eval(tool_cl, environment(fun))
  })
}


find_tool <- function(env, name) {
  # browser()
  chat <- find_chat(env)
  chat$get_tools()[[name]] %||% as_tool_def(env[[name]], name)
}

find_chat <- function(env) {
  for (chat_name_candidate in c("chat", "ch", ".chat", ".ch", names(env))) {
    chat <- env[[chat_name_candidate]]
    if (inherits(chat, "Chat")) return(chat)
  }

  stop("Chat sheet must define a 'Chat' object")
}

.onLoad <- function(...) {
  rlang::on_package_load("knitr", {
    knitr::knit_engines$set(btw = eng_btw, tool = eng_tool)
    # knitr::knit_engines$set(btw = eng_btw, `` = eng_tool)
  })
}
