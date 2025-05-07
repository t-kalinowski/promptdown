# promptdown <img src="man/figures/logo.png" alt="The package logo, a small cute elephant holding a quill and writing promptdown" align="right" height="240"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of promptdown is to make it easy to compose prompts for llms.\
You can think of it as knitr + ellmer.

<br>

![intro demo video](https://github.com/user-attachments/assets/e56c5920-2b97-4058-b75f-3917a5acbfd7)

## Install

``` r
pak::pak("t-kalinowski/promptdown")
```

------------------------------------------------------------------------

## Why

Coaxing desirable output from an LLM is an act of iterating on the
context. "Context" here meaning "all the text" sent to the model to
auto-complete.

The most common interface to an LLM, the "chat", frustrates any attempt
at context management. It is an append-only log of turns where you are
encouraged to compose your text in a comically small input box without
the standard features of a typical text editor.

Worse, the interaction starts pre-seeded with a large hidden context
that only grows while you engage in a pretend conversation with the
autocomplete. The system prompt, available tool definitions, tool calls,
tool call results, and even external content like added files, invite no
opportunity to observe let alone refine the context that is assembled.

The append-only nature of a chat interface means that all interactions
that go on long enough end the same way: going off a cliff in usefulness
as the context gets too large or too full of irrelevant text that
confuses the LLM.

Other interface styles, like ghost text completion, coding agents, or
task-focused assistants invoked inline, all similarly limit visibility
or control over the actual context sent to the LLM.

<!-- Using LLMs like this (to me) often feels like trying to play piano with oven mitts.  -->

Curtains and smoke all in service of the illusion you're interacting
with a sentience and not an autocomplete. In the short-term these impede
your ability to get a useful response; in the longer term they impede
your ability to develop an intuition about how LLMs respond, and how to
build workflows ontop of them.

LLMs *are* somewhat magical, but the interface to them does not have to
be. This package is another imagining of what a useful interface to an
LLMs is.

Prompdown facilitates a more intentional mode of interacting with the
LLM: instead of iterating by adding to an append-only ever-growing
context full of potentially irrelevant messages and hidden context, you
can are invited to observe and refine the *full context* on *each turn*.

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

Effective prompt composition requires the author to not only have full
control over the context sent for completion, but more importantly, to
have developed a strong intuition about LLM responses. Developing this
intuition requires many LLM interactions with full visibility, ideally
with a tight feedback loop with frequent ad-hoc controlled interaction
experiments. A vibe intuition.

In other words, promptdown facilitates is a seamless transition from an
interactive ephemeral experience to composing a deployed agent.

## Additional conveniences:

In additional to the standard knitr engines like `r`, `python`, `julia`,
`verbatim`, `embed`, etc, promptdown install two additional engines for
convenience:

### `tool`

This is a dynamic code chunk that is equivalent to the LLM calling a
tool. It will typically be found under an `## LLM` turn. It invokes the
tool like an LLM would and outputs the result like the LLM will see. You
can use this to quickly iterate on tool defs, or as a dynamic
composition tool. Here is an example:

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

{btw} is a package full of convenience functions for quickly assembling
a prompt context.

You can use the `{btw}` engine to quickly preview and assemble context.
All the standard syntax of `btw::btw()` is valid.

````` markdown
```{btw}
mtcars
```

```` markdown
## Context

mtcars
```json
## Context

mtcars
```json
{"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
```
````
`````
