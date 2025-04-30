.onLoad <- function(...) {
  run_on_load()
  S7::methods_register()
}
