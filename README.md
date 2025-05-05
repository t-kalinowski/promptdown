# promptdown

<!-- badges: start -->

<!-- badges: end -->

The goal of promptdown is to make it easy to compose prompts for llms.
You can think of it as knitr + ellmer.

------------------------------------------------------------------------

<!-- Conjuring? Compelling? Drawing?  -->

## Why

Coaxing desirable output from an LLM is an act of iterating on the
context. "Context" here meaning "all the text" sent to the model to
auto-complete.

The most common interface to an LLM, the "chat", frustrates any attempt
at context management. It is an append-only log of a "conversation"
where you are encouraged to compose your text in a comically small input
box, absent the standard set of text-editing facilities in a typical
text composition environment.

You engage in a "conversation" with an autocomplete. A conversation
where the context often begins with a large hidden component which then
grows over time. The system prompt, available tool definitions, tool
calls, tool call results, and even external content like added files,
invite no opportunity to observe or refine how the context is assembled.
Curtains and smoke all in service of maintaining the illusion you're
interacting with a sentience and not an autocomplete.

The append-only nature of a chat interface means that all interactions
that go on long enough end the same way: going off a cliff in usefulness
as the context gets too large or too full of irrelevant text that
confuses the LLM.

Other interface styles, like ghost text completion, coding agents, or
task-focused assistants invoked inline, all similarly limit visibility
or control over the actual context sent to the LLM.

Using LLMs like this feels like trying to play piano with oven mitts and
a blindfold.

LLMs *are* somewhat magical, but the interface to them does not have to
be. This package is another imagining of what a useful interface to an
LLMs is.

Prompdown facilitates a more intentional mode of interacting with the
LLM: instead of iterating by adding to an append-only ever-growing
context full of potentially irrelevant messages and a boatload of hidden
context, you can are invited to observe and refine the *full context* on
*each turn*.

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
`## System` `## User` and `## LLM`. (notice, no attempt is made to
obscure the fact that you're dealing with an autocomplete - the
responder is an "LLM", not an "Assistant"). These headings deliniate
chat `Turn`s, when submitting the context for LLM completion.

You can also equip the LLM with tools (functions) it can call.
Promptdown provides an affordance for auto-registering functions defined
with a `declare(tool(...))` annotation as tools, or you can register
tools using the conventional `ellmer::Chat$register_tool()` interface.

Tool calls and tool call results are fully visible in the LLM response,
(needless to say, as plain text). You can edit the tool calls and
results before submitting subsequent turns, including re-evaluating tool
calls by converting them to dynamic code chunks, or just deleting them.

To submit a context for completion, you can run
`promptdown::run_chat_sheet()` (Recommended to bind to keyboard shortcut
cmd/ctrl + shift + R). This will:

1)  Evaluate dynamic code chunks. Code chunks can do anything, but will
    mostly be used used to:

-   inject context (emit output)
-   register tools

2)  The document is rendered into a concrete (non-dynamic) chat turns
    sheet, which is then parsed into a sequence of `ellmer::Turn`s.

3)  The bundle of `Turns` and `ToolDef` are assembled and submitted to
    the LLM provider as a request for completion.

4)  The LLM response is then streamed back into the original text
    document under a new `## LLM` heading.

5)  For subsequent turns, you can choose to either continue by appending
    a new `## User` turn to the document, or rerun to replace the
    previous LLM response. In between turns you can inspect and edit the
    full context.

------------------------------------------------------------------------

## Why (redux)

In my opinion, today, the most genuinely helpful interactions with LLMs
tend to be one-off rubber-ducking sessions; ephemeral interactions
quickly discarded, similar to a web search or an EDA. For this reason
the first focus in promptdown is on the interactive experience: to let
you quickly and easily build up and refine a context using mature text
composition tools seeped in the literate programming paradigm; to be a
viable alternative to the chat interface.

However, "agentic" workflows are just around the corner (if not here
already). Today at least, crafting a reliable LLM workflow means
currating a context and a refined prompt. Composition of code and prose,
where prose is also code and code is also prose, and they're meant to be
interleaved.

Successful composition like this requires the author to not only have
full control over prompt composition, but more importantly to have
developed a strong intuition about LLM responses given a context.
Developing this intuition is as much part of the workflow as is using a
debugger or testing small snippets at the REPL. It's a workflow with
tight feedback loops and frequent controlled "experiments".

a Afterall, an llm "agent" is not much more than a bundle of a text and
tools defs.

An "agent" then is another artifact that data professionals can produce
to deliver value, similar in category to a dashboard or an automated
email report. A carefully crafted prompt and set of tools, deployed on a
trigger, schedule, or for self-service chat by data consumers.

To enable this usecase, the goal is to make a promptdown document a
deployable artifact, similar to an Rmarkdown document that gets deployed
as a dashboard or an automated email report.

In other words, there is a seamless transition from treating the
promptdown document as an interactive experience to a deployable agent.

## Additional conveniences:

In additional to the standard knitr engines like `r`, `python`, `julia`,
`verbatim`, `embed`, etc, promptdown install two additional engines for
convenience:

### `tool`

This is a dynamic code chunk that is equivalent to the LLM calling a
tool. It will typically be found under an `## LLM` turn. It invokes the
tool as-if the llm did and output the result formatted as what the llm
will see. You can use this to quickly iterate on tool defs, or as a
composition tool. Here is an example:

````         

```{r}
knitr::opts_chunk$set(comment = '', echo = FALSE)
chat <- ellmer::chat_ollama(model = "qwen3:4b")
```

## System

You are a helpful New Yorker and trip planning assistant.

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
get_current_weather("New York")
```
````

### `btw`

{btw} is a package full of convenience functions for quickly assembling
a prompt context.

You can use the `{btw}` engine to quickly previow and assemble context.
All the standard syntax of `btw::btw()` is valid.

```{btw}
?plot.default
mtcars
```
