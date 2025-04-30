#' Parse a prompt sheet and returns a dataframe with columns
#' - role: one of 'system' 'user' or assistant'
#' - turn: integer, the turn number in the sequence. System prompt if present is always 0.
#'   user turns are always odd, llm turns are even.
#' - text: single string
parse_chat_sheet <- function(file, text = readLines(file)) {
  text <- text |> stri_split_lines() |> unlist() |> stri_trim_right()

  # drop yaml frontmatter
  if (text[1] == "---") {
    text <- text[-seq_len(which(text == "---")[2])]
  }

  # segment
  df <- text |> markdown_frame("h1")

  # Content not under a heading is assumed to be part of the first User turn
  df$h1[is.na(df$h1)] <- "# User"

  # heading with no content is NA; replace with ""
  df$text[is.na(df$text)] <- ""

  df <- df |>
    mutate(
      role = if_else(h1 %in% c("# System", "# User", "# LLM"), h1, NA) |>
        stri_replace_first_fixed("# ", "") |>
        stri_trans_tolower() |>
        replace_val("llm", "assistant") |>
        vctrs::vec_fill_missing(),
      turn = vctrs::vec_identify_runs(role),
      turn = if_else(role == "system", 0L, turn)
    ) |>
    group_by(role, turn) |>
    summarise(
      text = {
        # If there are consecutive headings belonging to the same turn role,
        # combine them.
        # If there are other h1 headings (from the user or llm ContentText),
        # then merge them back into the text.
        turn_start <- which(h1 == turn_heading(role[1]))
        h1[turn_start] <- ""
        if (last(turn_start) == length(text))
          turn_start <- drop_last(turn_start)
        text[turn_start + 1] <- stri_trim_left(text[turn_start + 1])

        stri_c(h1, text, collapse = "")
      },
      .groups = "drop"
    ) |>
    arrange(turn)

  # combine same-role consecutive turns
  df <- df |>
    group_by(turn = vctrs::vec_identify_runs(df$role)) |>
    summarise(
      .groups = "drop",
      role = unique(role),
      text = text |>
        stri_trim_both() |>
        stri_collapse_lines() |>
        stri_trim_both()
    )

  # drop an empty last turn
  if (stri_trim_both(last(df$text)) == "") {
    df <- df[-nrow(df), ]
  }

  if (df$role[1] == "system" && df$text[1] == "") {
    # drop empty "system" prompt
    df <- df[-1, ]
    df$turn <- seq_along(df$turn)
  } else {
    # else system is always turn 0
    df$turn <- seq_along0(df$turn)
  }

  df
}

turn_heading <- function(role) {
  switch(role, assistant = "# LLM", user = "# User", system = "# System")
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


sheet_into_turns <- function(x) {
  turns <- purrr::pmap(x, function(role, text, ...) {
    if (identical(text, "")) {
      return(ellmer::Turn(role))
    }
    text <- markdown_segment(text, "pre") %error% browser()

    # first convert images into ellmer Content blocks
    text <- unlist(imap(text, function(segment, name) {
      if (name == "pre") return(segment)
      img_link_locations <- stringi::stri_locate_all_regex(
        segment,
        "!\\[(?<alt>[^\\]]*)\\]\\((?<path>[^\\)]*)\\)",
        capture_groups = TRUE
      )[[1]]

      if (all(is.na(img_link_locations))) return(segment)
      cg <- attr(img_link_locations, "capture_groups")
      paths <- stri_sub(segment, cg$path)
      img_contents <- lapply(paths, ellmer::content_image_file)
      # alts <- stri_sub(segment, cg$alt)
      # stri_sub(segment, img_link_locations) <- sprintf(
      #   '(image with alt text: "%s")',
      #   alts
      # )
      c(segment, img_contents)
    }))

    i <- 0L
    contents <- list()
    while (i < length(text)) {
      block <- text[[i <- i + 1L]]
      if (inherits(block, "ellmer::Content")) {
        append1(contents) <- block
        next
      }
      stopifnot(is.character(block))
      if (names(text)[i] == "pre") {
        if (startsWith(block, "``` tool")) {
          tool_request <- parse_tool_request(block)
          append1(contents) <- tool_request

          while (stri_trim_both(block <- text[i <- i + 1L]) == "") NULL
          tool_result <- parse_tool_result(block, tool_request)
          append1(contents) <- tool_result
          next
        }

        if (startsWith(block, "``` ellmer::Content")) {
          block <- stri_match_first_regex(
            block,
            "^```([^\n]*)\n(.*)```$",
            dot_all = TRUE
          )

          # .x <- stri_split_fixed(.x, "\n")[[1L]]
          constructor <- eval(str2lang(block[, 2]))
          args <- yaml::yaml.load(block[, 3])
          append1(contents) <- do.call(constructor, args)
          next
        }
      }

      append1(contents) <- ContentText(block)
    }
    ellmer::Turn(role, contents = contents)
  })

  # move ContentToolResult out of the LLM turn and into a new user turn
  turns <- turns |>
    lapply(function(turn) {
      if (turn@role == "assistant") {
        is_res <- map_lgl(turn@contents, inherits, "ellmer::ContentToolResult")
        if (any(is_res)) {
          user_turn <- Turn("user", turn@contents[is_res])
          turn@contents <- turn@contents[!is_res]
          return(list(turn, user_turn))
        }
      }
      list(turn)
    }) |>
    unlist(recursive = FALSE)
  turns
}

submit_turns <- function(
  chat,
  turns,
  ...,
  stream = FALSE
) {
  chat$set_turns(drop_last(turns))

  do_it <- if (stream) chat$stream else chat$chat
  do.call(do_it, last(turns)@contents)
}


# run
# render
# submit
# post
# get
# knit_chat
# knit_turns

#' @export
run_chat_sheet <- function(
  path = NULL,
  ## TODO: rethink this signature here. maybe?: output = list(path, stdout(), viewer())
  ## TODO: if last turn is LLM, remove the last llm response from the sheet before rerunning it.
  update = TRUE,
  stream = TRUE,
  echo = !update,
  ...,
  encoding = "UTF-8"
) {
  if (is.null(path)) {
    id <- rstudioapi::documentId(allowConsole = FALSE)
    path <- rstudioapi::documentPath(id)
  }

  # if (isTRUE(echo)) echo <- "all"

  if (!endsWith(path, ".chat.Rmd"))
    stop("`path` must have extension '.chat.Rmd'")

  envir <- new.env(parent = parent.frame())

  fs::dir_create("_chats")
  md_path <- timestamp_filename(prefix = "_chats/", ext = ".md")

  knitr::render_markdown()
  knitr::opts_chunk$set(render = function(x, ...) {
    if (inherits(x, "ellmer::Content")) {
      if (inherits(x, "ellmer::ContentText")) {
        return(knitr::asis_output(x@text))
      }

      yaml <- yaml::as.yaml(S7::props(x))
      x <- stri_join("``` ", class(x)[1], "\n", yaml, "```")
      return(knitr::asis_output(x))
    }
    knitr::knit_print(x, ...)
  })
  knitr::opts_knit$set(root.dir = getwd())

  knitr::knit(path, output = md_path, envir = envir, quiet = F)
  chat <- find_chat(envir)
  # .GlobalEnv$chat <- chat

  emitter <- new_emitter(
    md_path,
    if (update) path,
    if (echo) stdout()
  )
  emit <- emitter$emit
  on.exit(emitter$close())

  tools <- find_tools(envir)
  tools <- tools |> modifyList(chat$get_tools())
  tools <- lapply(tools, trace_tool, emit)
  chat$set_tools(tools)

  turns <- md_path |> parse_chat_sheet() |> sheet_into_turns()

  emit("\n\n# LLM\n\n")
  response <- submit_turns(chat, turns, stream = stream)

  if (stream) {
    coro::loop(for (output in response) emit(output))
  } else emit(response)

  emit("\n\n# User\n\n")
  invisible(chat)
}
