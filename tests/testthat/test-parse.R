# Locate project root
root <- app_test_root()
app <- app_test_path()
files <- c(
  file.path(app, 'app.R'),
  file.path(app, 'global.R'),
  file.path(app, 'helpers.R'),
  list.files(file.path(app, 'params'), pattern='\\.R$', full.names=TRUE),
  list.files(file.path(app, 'stats'), pattern='\\.R$', full.names=TRUE),
  list.files(file.path(app, 'graficos'), pattern='\\.R$', full.names=TRUE),
  list.files(file.path(app, 'server'), pattern='\\.R$', full.names=TRUE),
  list.files(file.path(app, 'ui'), pattern='\\.R$', full.names=TRUE)
)
for (f in files) {
  testthat::test_that(paste('parse', f), {
    testthat::expect_error(parse(f), NA)
  })
}
