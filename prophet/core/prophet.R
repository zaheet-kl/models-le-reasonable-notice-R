prophetPath <- function(...) {
  here::here(...)
}
prophetSource <- function(...) {
  source(prophetPath(...))
}
prophetSourceDirRecursive <- function(...) {
  sources <- list.files(prophetPath(...), full.names=TRUE, recursive=TRUE)
  sapply(sources, source)
}
