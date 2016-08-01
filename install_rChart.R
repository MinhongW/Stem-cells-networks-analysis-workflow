install_github('rCharts','ramnathv')
install.packages('downloader',"C:/Data/Software/R-3.2.2/library")
library(rCharts)
library(downloader)
tf <- tempfile(fileext = ".zip")
download(
  url = "https://github.com/rcharts/parcoords/archive/gh-pages.zip",
  tf
)
# unzip to tempdir and set as working directory
td <- tempdir()
unzip(tf, exdir = td)
setwd(file.path(td, "parcoords-gh-pages"))