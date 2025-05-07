segment_turns <- function(text) {
  # splits text into a character vector of turns. Returns a character vector
  # with names of turn role.
  lines <- text |> stri_split_lines() |> unlist() |> stri_trim_right()

  # drop yaml front matter
  if (lines[1L] == "---") {
    # lines[seq_len(which(lines == "---")[2L])] <- ""
    lines <- lines[-seq_len(which(lines == "---")[2L])]
  }

  lines <- c("# User", "", lines)

  doc <- lines |>
    commonmark::markdown_xml(sourcepos = TRUE, extensions = TRUE) |>
    xml2::read_xml()

  headings <- doc |>
    xml2::xml_find_all(
      "//d1:heading[
        ./d1:text[
          . = 'User' or
          . = 'System' or
          . = 'LLM' or
          . = 'Assistant'
        ]
      ]"
    )

  roles <- headings |>
    xml2::xml_find_first("./d1:text") |>
    xml2::xml_text() |>
    replace_val("LLM", "assistant") |>
    tolower()

  starts <- headings |>
    xml2::xml_attr("sourcepos") |>
    stri_extract_first_regex("^[^:]+") |>
    as.integer()

  ends <- c(starts[-1] - 1L, length(lines))
  sizes <- ends - starts + 1L
  turns <- vctrs::vec_chop(lines, sizes = sizes)
  names(turns) <- roles

  # flatten each segment
  turns <- map_chr(turns, \(x) {
    x[-1L] |> #drop heading
      stri_flatten(collapse = "\n") |>
      stri_trim_both()
  })

  # reorder: bring all system prompts to the front
  turns <- turns[order(names(turns) == "system", decreasing = TRUE)]

  # drop empty turns
  turns <- turns[nzchar(turns)]

  # consolidate consecutive same-role turns
  turns <- vctrs::vec_chop(
    turns,
    sizes = vctrs::vec_run_sizes(names(turns))
  ) |>
    lapply(\(x) {
      role <- names(x)[1]
      flattened <- stri_flatten(x, collapse = "\n\n", omit_empty = TRUE)
      `names<-`(flattened, role)
    }) |>
    unlist()

  # drop empty last turn
  if (last(turns) == "") turns <- drop_last(turns)

  turns
}


knit_chat_sheet <- function(input_path, output_path, envir, quiet = TRUE) {
  knitr::render_markdown()
  knitr::opts_chunk$set(render = function(x, options, ...) {
    if (inherits(x, "ellmer::Content")) {
      if (inherits(x, "ellmer::ContentText")) {
        return(knitr::asis_output(x@text))
      }

      yaml <- yaml::as.yaml(S7::props(x))
      x <- stri_join("``` ", class(x)[1], "\n", yaml, "```")
      return(knitr::asis_output(x))
    }

    if (inherits(x, "help_files_with_topic")) {
      pkgname <- attr(x, "topic", TRUE)
      cl <- deparse(attr(x, "call"))

      txt <- utils::capture.output(tools::Rd2txt(
        asNamespace("utils")$.getHelpFile(x),
        options = list(
          code_quote = FALSE,
          underline_titles = FALSE,
          itemBullet = "* ",
          sectionIndent = 0L
        ),
        package = pkgname
      ))

      knitr::asis_output(txt)
    }

    knitr::knit_print(x, ...)
  })
  knitr::opts_knit$set(root.dir = getwd())

  knitr::knit(
    input = input_path,
    output = output_path,
    envir = envir,
    quiet = quiet
  )
}


read_turns_sheet <- function(file, text = readLines(file)) {
  # reads a knitted markdown file, returns a list of ellmer::Turns
  turns <- text |> segment_turns() |> lapply(as_contents_list)
  turns <- unname(imap(turns, function(contents, role) {
    ellmer::Turn(role, contents)
  }))

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


as_contents_list <- function(text) {
  # parses a turns text into a list of ellmer::Contents
  # markdown images are converted to the correct ellmer::Content type
  # chunks with a ellmer::Content* type are deserialized
  # aall other text is wrapped in ContentText

  if (identical(text, "")) {
    return(list())
  }

  segments <- markdown_segment(text, "pre")

  # first convert images into ellmer Content blocks
  segments <- unlist(imap(segments, function(segment, name) {
    if (name == "pre") {
      return(segment)
    }
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
  while (i < length(segments)) {
    block <- segments[[i <- i + 1L]]
    if (inherits(block, "ellmer::Content")) {
      append1(contents) <- block
      next
    }
    stopifnot(is.character(block))
    if (names(segments)[i] == "pre") {
      if (startsWith(block, "``` tool")) {
        tool_request <- parse_tool_request(block)
        append1(contents) <- tool_request

        # consume whitespace
        while (stri_trim_both(block <- segments[i <- i + 1L]) == "") NULL

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

        constructor <- eval(str2lang(block[, 2L]))
        args <- yaml::yaml.load(block[, 3L])
        append1(contents) <- do.call(constructor, args)
        next
      }
    }

    append1(contents) <- ContentText(block)
  }

  contents
}


find_chat <- function(env) {
  for (chat_name_candidate in c("chat", "ch", ".chat", ".ch", names(env))) {
    chat <- env[[chat_name_candidate]]
    if (inherits(chat, "Chat")) return(chat)
  }

  stop("Chat sheet must define a 'Chat' object")
}

timestamp_filename <-
  function(prefix = "", ext = "", format = "%Y-%m-%d_%Hh%Mm%OS3s") {
    ts <- gsub(".", "-", strftime(Sys.time(), format = format), fixed = TRUE)
    if (nzchar(ext) && !startsWith(ext, ".")) ext <- paste0(".", ext)
    paste0(prefix, ts, ext)
  }

new_turns_sheet_path <- function() {
  fs::dir_create("_chats")
  timestamp_filename(prefix = "_chats/", ext = ".md")
}


#' Read a chat prompt sheet
#'
#' Reads a prompt sheet and returns an `ellmer::Chat()` along with contents for
#' the next user turn. Dynamic chunks in the prompt sheet are evaluated, but
#' no request to the LLM provider is made.
#'
#' This function is suitable for programatic use in a function.
#'
#' @param path Path to the input markdown file.
#' @param output Path to write the knitted markdown output. Defaults to a
#'   timestamped file in the `_chats/` directory.
#' @param envir Environment in which to evaluate the code chunks. Defaults to a
#'   new environment inheriting from the calling one.
#'
#' @return A list of two elements: an `ellmer::Chat` object and a list of
#'   contents for the next user turn.
#'
#' @export
read_chat_sheet <- function(
  path,
  output = new_turns_sheet_path(),
  envir = new.env(parent = parent.frame())
) {
  # knits and parses a prompt sheet, returns an ellmer::Chat() along with
  # a list of contents for the next user turn
  # with turns and tools set.

  knit_chat_sheet(
    input_path = path,
    output_path = output,
    envir = envir
  )

  chat <- find_chat(envir)
  next_user_turn_contents <- list()
  turns <- read_turns_sheet(output)
  if (last(turns)@role == "user") {
    next_user_turn_contents <- last(turns)@contents
    turns <- drop_last(turns)
  } else {
    next_user_turn_contents <- NULL
  }

  chat$set_turns(turns)
  # todo: merge turns if Chat object comes with other turns

  tools <- find_tools(envir)
  tools <- modifyList(tools, chat$get_tools())
  chat$set_tools(tools)

  list(chat, next_user_turn_contents)
}
