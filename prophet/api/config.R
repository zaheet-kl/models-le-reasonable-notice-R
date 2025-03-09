get_app_config <- function() {
  list(
    appName='prophet',
    environment=get_with_default(Sys.getenv("PROPHET_ENVIRONMENT"), "local"),
    port=as.numeric(get_with_default(Sys.getenv("PORT"), 8080)),
    loggingIgnorePaths=c('/healthz'),
    logLevel=get_with_default(Sys.getenv("LOG_LEVEL"), LOG_LEVEL$INFO)
  )
}
