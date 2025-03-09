# Title     : Logger
# Objective : utility for bunyan style logs in R
# Created by: darrenmce
# Created on: 2020-10-23

LOG_LEVEL <- list(
  TRACE=10,
  DEBUG=20,
  INFO=30,
  WARN=40,
  ERROR=50
)

create_logger <- function(app_name, log_level_config = LOG_LEVEL$INFO) {
  log_level <- strtoi(log_level_config);
  if (is.na(log_level)) {
    stop(c('Invalid Log Level (must be an integer): ', log_level_config));
  }

  create_base_message <- function(level, message) {
    list(
      name=app_name,
      hostname=Sys.info()['nodename'],
      level=level,
      time=format(Sys.time(), usetz=TRUE),
      msg=message
    )
  }

  create_log_message <- function(level) {
    log_fn <- function(message, data = list()) {
      if (level >= log_level) {
        print(toJSON(c(
          data,
          create_base_message(level, message)
        ), auto_unbox = TRUE))
      }
    }
    return(log_fn)
  }

  trace <- create_log_message(LOG_LEVEL$TRACE)
  debug <- create_log_message(LOG_LEVEL$DEBUG)
  info <- create_log_message(LOG_LEVEL$INFO)
  warn <- create_log_message(LOG_LEVEL$WARN)
  error <- create_log_message(LOG_LEVEL$ERROR)

  # Request log serializer
  requestLogger <- function(req, res, endTicToc, defaultLogFn = info) {
    status_code <- res$status
    log_fn <- defaultLogFn
    if (status_code >= 500) {
      log_fn <- error
    } else if (status_code >= 400) {
      log_fn <- warn
    }
    log_fn(
      paste(convert_empty(req$REMOTE_ADDR), '<-', convert_empty(req$REQUEST_METHOD), convert_empty(req$PATH_INFO)),
      list(
        remote_address=convert_empty(req$REMOTE_ADDR),
        user_agent=convert_empty(req$HTTP_USER_AGENT),
        host=convert_empty(req$HTTP_HOST),
        method=convert_empty(req$REQUEST_METHOD),
        path=convert_empty(req$PATH_INFO),
        status=convert_empty(res$status),
        response_time_seconds=round(endTicToc$toc - endTicToc$tic, digits = getOption("digits", 5))
      )
    )
  }

  return(list(
    trace=trace,
    debug=debug,
    info=info,
    warn=warn,
    error=error,
    requestLogger=requestLogger
  ));
}
