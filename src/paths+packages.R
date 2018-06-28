## paths+packages.R

require(RCurl)
require(lubridate)
require(reshape2)
require(dplyr)
require(ggplot2)

## site data - from Stations.docx (John)
df.site.metadata <- 
  data.frame(site = c("BBW", "08NL007", "08NL024", "08NL038", "08NL070"),
             area.km2 = c(14.5, 408, 1780, 1810, 5580),
             BFI_CMB = c(0.54, 0.32, 0.27, 0.27, 0.36))

## conversion factors
km2.to.mi2 <- 0.386102

## ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank())
}

theme_set(theme_scz())

## functions etc
source_https <- function(u, unlink.tmp.certs = FALSE) {
  # from: https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
  
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}
