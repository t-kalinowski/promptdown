test_that("parse_tool_request", {
  text <- trim(
    r"---(
    ``` tool
    get_current_weather(location = "new york")
    ```
    )---"
  )

  request <- parse_tool_request(text)
  request@id <- "someuuid"
  expect_identical(
    request,
    ellmer::ContentToolRequest(
      id = "someuuid",
      name = "get_current_weather",
      arguments = list(location = "new york")
    )
  )
})
