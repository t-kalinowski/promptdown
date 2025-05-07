# promptdown <img src="man/figures/logo.png" alt="The package logo, a small cute elephant holding a quill and writing promptdown" align="right" height="240"/>

The goal of **promptdown** is to make it easy to compose prompts for
LLMs. Think [knitr](https://yihui.org/knitr/) +
[ellmer](https://ellmer.tidyverse.org). Key features:

-   Full visibility and control over what’s sent to the model.
-   Compose dynamic prompts using markdown and code chunks.
-   Chat interactively or compose deployable agents.

<br>

## Example chat session

![intro demo
video](https://github.com/user-attachments/assets/ec77daf8-975a-4f06-83da-f9572deeedf3)

## Install

``` r
pak::pak("t-kalinowski/promptdown")
```

------------------------------------------------------------------------

## Why

Coaxing desirable output from an LLM is the act of iterating on the
prompt. "Prompt" here meaning *all* the text sent to the model. That
includes: the system prompt, tool definitions, tool calls and results,
added files, prior messages, even stray formatting.

Most interfaces don’t want you to think about any of that.

The most common interface to an LLM, the "chat", frustrates any attempt
at prompt management. It is an append-only log of turns where you are
encouraged to compose in a comically small input box without the basic
features of a typical text editor.

Worse, the interaction starts with a large hidden component that only
grows while you engage in a pretend conversation with the autocomplete.
The system prompt, available tool definitions, tool calls and results,
and even external content like added files, invite no opportunity to
observe--let alone refine--the assembled prompt.

The append-only nature of a chat interface means that all interactions
that go on long enough end the same way: going off a cliff in usefulness
as the context gets too large or too cluttered with irrelevant text that
confuses the LLM.

Other interface styles, like ghost text completion, coding agents, or
task-focused assistants invoked inline, all similarly limit visibility
or control over the actual prompt sent to the LLM.

LLMs *are* magical, but the interface to them does not have to be.

**promptdown** invites you to work directly with the prompt; to see and
shape all of it, every turn.

------------------------------------------------------------------------

## How it works.

You write in a prompt markdown document. This looks like a conventional
Quarto or Rmarkdown document with code chunks where you can evaluate R
or Python or [any
other](https://bookdown.org/yihui/rmarkdown/language-engines.html) type
of code. You can also include verbatim content from other files via the
`verbatim` or `embed` engine, or from other dynamic documents via the
`knit_child` option.

Because this is a Quarto markdown document, all the features and tooling
of dynamic documents are available. You get see inline previews of code
chunk evaluations (including images and data plots), support for precise
control over the output with options like `echo: false` or
`results: asis`. You get live access to the current R and Python session
at the REPL. And you can use all the standard tools of text composition,
like the Visual Editor in RStudio or VS Code (or Positron), the spell
checker, text snippets, and the full set of keyboard shortcuts and
conveniences you've configured your text editor with.
<https://quarto.org/docs/visual-editor/>
<https://quarto.org/docs/visual-editor/vscode/>

Within the promptdown document you can have specific headings:
`# System` `# User` and `# LLM`. (Notice, no attempt is made to obscure
the fact that you're dealing with an autocomplete - the responder is an
"LLM", not an "Assistant"). These headings delineate chat `Turn`s, when
assembling an LLM completion request.

You can also equip the LLM with tools (functions) it can call.
Promptdown provides an affordance for auto-registering functions defined
with a `declare(tool(...))` annotation as tools, or you can register
tools using the conventional `ellmer::Chat$register_tool()` interface.

Tool calls and tool call results are fully visible in the LLM response,
(needless to say, as plain text). You can edit the tool calls and
results before submitting subsequent turns. You can convert a tool call
into a dynamic code chunks to iterate on tool call, or you can just
delete them if they're no longer relevant.

To submit a context for completion, you can run
`promptdown::run_chat_sheet()` (Recommended to bind to keyboard shortcut
cmd/ctrl + shift + R). This will:

1)  Evaluate dynamic code chunks. Code chunks can do anything, but will
    mostly be used used to:

-   Create the `ellmer::Chat` object, which details which provider and
    model to use.
-   Inject context (emit output).
-   Register tools

2)  The document is rendered into a concrete (non-dynamic) chat turns
    sheet, which is then parsed into a sequence of `ellmer::Turn`s.

3)  The bundle of `Turn`s and `ToolDef`s are assembled and submitted to
    the LLM provider as a request for completion.

4)  The LLM response is then streamed back into the original text
    document under a new `## LLM` heading.

5)  For subsequent turns, you can choose to either continue by appending
    a new `## User` turn to the document, or rerun to replace the
    previous LLM response. In between turns you are free to inspect and
    edit the full context--it's all plain markdown text.

------------------------------------------------------------------------

## Why (redux)

In my opinion, today, the most genuinely helpful interactions with LLMs
tend to be one-off rubber-ducking sessions; ephemeral interactions where
the output is quickly discarded, similar to a web search or an EDA
session at the REPL. For this reason the first focus in promptdown is on
the interactive experience: to let you quickly and easily compose and
refine a context using mature text composition tools seeped in the
literate programming paradigm; to be a viable alternative to the chat
interface.

However, "agentic" LLM workflows are just around the corner (if not here
already). Data professionals can bundle a prompt with some tool defs and
deploy them to deliver value, similar in category to a dashboard, an
automated email report, or an API. An "agent" meaning A carefully
crafted prompt and set of tools, deployed on a trigger, schedule, or as
a self-service endpoint for data consumers.

To enable this usecase, a promptdown document is also a deployable
artifact, similar to an Rmarkdown document that gets deployed as a
dashboard or an automated email report.

Creating a reliable LLM workflow is blend of coding and writing; it's
assembling a text that has both code and prose, and where the code
functions as prose and the prose functions as code. In this light,
prompt composition, is yet another incarnation of the mature practice of
literate programming.


It’s literate programming for LLMs. The code is prose. The prose is
code.

------------------------------------------------------------------------

## Additional conveniences

Along with standard knitr engines (`r`, `python`, `julia`, `verbatim`,
`embed`, etc.), promptdown adds two more:

### `tool`

This simulates the LLM calling a tool. You can use it to quickly test
tool definitions or generate the prompt dynamically.

Example:

```` markdown
```{r}
library(promptdown)
knitr::opts_chunk$set(comment = '', echo = FALSE)
chat <- ellmer::chat_ollama(model = "qwen3:4b")
```

## System

You are a helpful New York trip planning assistant.

```{r}
get_current_weather <- function(location = "") {
  declare(tool(
    "Get the current weather",
    location = type_string()
  ))

  list(temp = 65, conditions = "sunny")
}
```

## User

Should I bring an umbrella?

## LLM

```{tool}
get_current_weather(location = "New York")
```
```
{"temp":65,"conditions":"sunny"}
```
````

### `btw`

The `{btw}` package offers helpers for assembling context quickly. Use
the `{btw}` engine for fast previews using standard `btw::btw()` syntax:

```` markdown
```{btw}
mtcars
```

````markdown
## Context

mtcars
```json
{"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{...}}
```
````
`````
