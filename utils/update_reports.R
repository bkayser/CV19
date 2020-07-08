rmarkdown::render('reports/CleanData.Rmd')
rsconnect::deployApp(appDir = "~/Projects/Covid19", 
                     appFiles = c('server.R',
                                  'ui.R',
                                  'www/bootstrap.css',
                                  'utils/labels.R', 
                                  'utils/utils.R', 
                                  'data/cvdata.us.by_state.RDS',
                                  'data/cvdata.us.RDS',
                                  'data/states.status.RDS',
                                  'data/orders.events.RDS'),
                     account = "surprise", 
                     server = "shinyapps.io", 
                     appName = "COVID19Demo",      
                     lint = FALSE,
                     metadata = list(asMultiple = FALSE, asStatic = FALSE,ignoredFiles = "_app.R|.gitmodules|.RData|CleanData.nb.html|CleanData.pdf|CleanData.Rmd|LICENSE|README.md"),     
                     logLevel = "verbose") 

system("open https://surprise.shinyapps.io/COVID19Demo")

rmarkdown::render('reports/States.Rmd')
system("open reports/States.nb.html")
rmarkdown::render('reports/International.Rmd')
system("open reports/International.nb.html")
rmarkdown::render('reports/Predict.Rmd')
system("open reports/Predict.nb.html")
