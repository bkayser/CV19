rmarkdown::render('reports/CleanData.Rmd')
rmarkdown::render('reports/States.Rmd')
rmarkdown::render('reports/International.Rmd')

rsconnect::deployApp(appDir = "~/Projects/Covid19", 
                     appFiles = c('server.R',
                                  'ui.R',
                                  'utils/labels.R', 
                                  'data/cvdata.us.by_state.RDS',
                                  'data/cvdata.us.RDS', 
                                  'data/orders.events.RDS'),
                     account = "surprise", 
                     server = "shinyapps.io", 
                     appName = "COVID19Demo",      
                  #   appId = 2054587, 
                     lint = FALSE,
                     metadata = list(asMultiple = FALSE, asStatic = FALSE,ignoredFiles = "_app.R|.gitmodules|.RData|CleanData.nb.html|CleanData.pdf|CleanData.Rmd|LICENSE|README.md"),     
                     logLevel = "verbose") 

#rmarkdown::render('Predict.Rmd')
#system("open Predict.nb.html")
system("open https://surprise.shinyapps.io/COVID19Demo")
system("open reports/States.nb.html")
system("open reports/International.nb.html")
