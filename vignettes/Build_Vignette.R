library(knitr)
library(rmarkdown)

setwd("C:/Github/MERA/vignettes")
setwd("C:/Users/tcarruth/Documents/Github/MERA/vignettes")

render(input=paste0(getwd(),"/MERA.Rmd"), output_file=paste("MERA_User_Guide_4_4.html",sep=""))



