trace_tool <- function(tool, emit) {
  browser
  stopifnot(is.function(emit))
  og_fun <- tool@fun
  tool2 <- tool

  tool2@fun <- function(...) {
    display_call <- stri_flatten_lines(
      deparse(as.call(list(as.symbol(tool@name), ...))),
      end_with_newline = TRUE
    )
    emit("``` tool\n", display_call, "```\n")

    result <- get_tool_result(tool, list(...))
    string <- ellmer:::tool_string(result)

    fence <- "```"
    while (grepl(fence, string, fixed = TRUE)) {
      fence <- paste0(fence, "`")
    }

    # some checks
    if (inherits(string, "AsIs")) {
      stop("I() wrapped tool result not supported by chat sheet")
    }
    if (!is_string(string)) {
      stop("Failed to format tool result as a string.")
    }
    emit(
      fence,
      if (!is.null(result@error)) " error",
      "\n",
      string,
      "\n",
      fence,
      "\n\n"
    )
    #
    #     withCallingHandlers(
    #       {
    #         result <- og_fun(...)
    #         if (inherits(result, "ellmer::Content"))
    #           stop("tool returning content not yet implemented")
    #
    #         display_result <- capture.output(print(result)) |>
    #           stri_flatten_lines(end_with_newline = TRUE)
    #
    #         emit("```\n", display_result, "```\n\n")
    #         return(result)
    #       },
    #       error = function(e) {
    #         display_error <- capture.output(print(e)) |>
    #           stri_flatten_lines(end_with_newline = TRUE)
    #
    #         emit("```error\n", display_error, "```\n\n")
    #       }
    #     )
    result
  }
  tool2
}

find_tools <- function(env) {
  calls <- drop_nulls(lapply(names(env), function(name) {
    obj <- get(name, env)
    if (!is.function(obj)) return()

    tool_cl <- find_tool_declaration(obj)
    if (is.null(tool_cl)) return()

    tool_cl[[".fun"]] <- obj
    tool_cl[[".name"]] <- name
    tool_cl
  }))

  withr::with_package("ellmer", {
    lapply(calls, eval, env)
  })
}

parse_tool_request <- function(text, env = baseenv()) {
  if (startsWith(text, "```")) {
    # remove code fence
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
  # TODO: parse out id from block, and generate a more user-friendly id if
  # id not found
  ContentToolRequest(id = uuid::UUIDgenerate(), name = name, arguments = args)
}


get_tool_result <- function(tool_def, args, env = parent.frame()) {
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
  chat <- find_chat(env)
  chat$get_tools()[[name]] %||% as_tool_def(env[[name]], name)
}


parse_tool_result <- function(x, request) {
  matches <- stri_match_first_regex(
    x,
    "^```([^\n]*)\n(.*)\n```$",
    dot_all = TRUE
  )
  type <- matches[, 2]
  value <- matches[, 3]

  if (type == "error") {
    return(ContentToolResult(error = value))
  }
  if (type == "json") {
    class(value) <- "json"
  } else if (type == "yaml") {
    value <- yaml::yaml.load(value)
  }

  ContentToolResult(value = value, request = request)
}
