source(here::here('core', 'prophet.R'))

prophetSource('api', 'libraries.R')
prophetSource('api', 'util.R')
prophetSource('api', 'config.R')
prophetSource('core', 'logger.R')

config <- get_app_config()

log <- create_logger(paste(config$appName, config$environment, sep = '-'), config$logLevel)

pr <- plumber::plumb(prophetPath('api', 'routes.R'))

pr$registerHooks(
  list(
    preroute = function() {
      # Start timer for log info
      tictoc::tic()
    },
    postroute = function(req, res) {
      endTicToc <- tictoc::toc(quiet = TRUE)
      # Log details about the request and the response
      if (!is.element(req$PATH_INFO, config$loggingIgnorePaths)) {
        log$requestLogger(req, res, endTicToc)
      } else {
        log$requestLogger(req, res, endTicToc, log$trace)
      }
    }
  )
)

log$info('starting api server', list(port=config$port))
pr$run(host='0.0.0.0', port=config$port, swagger = TRUE)
