
local({
  # set a stable mirror
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.microsoft.com/snapshot/2020-01-01"
  options(repos = r)
})

if ( ! require(rmarkdown))
  install.packages('rmarkdown')

# Release dates
#current_time_UTC = as.POSIXlt(Sys.time(), tz = "UTC")
#current_time_UTC = as.POSIXlt(Sys.time(), tz = "UTC") - 1*60*60*24*21

#release_time_AEST_txt = 
#  c("2020-03-03 09:50", # W2 - Lab 1
#    "2020-03-10 09:50", # W3 - Lab 2
#    "2020-03-17 09:50", # W4 - Lab 3
#    "2020-03-24 09:50", # W5 - Lab 4
#    "2020-03-31 09:50", # W6 - Lab 5
#    "2020-04-07 09:50", # W7 - Lab 6
#    # Break
#    "2020-05-05 09:50", # W8 - Lab 7
#    "2020-05-12 09:50", # W9 - Lab 8
#    "2020-05-19 09:50", # W10 - Lab 9
#    "2020-05-26 09:50", # W11 - Lab 10
#    "2020-06-02 09:50", # W12 - Lab 11
#    "2020-06-09 09:50"  # W13 - Lab 12    
#  )
#release_time = as.POSIXct(release_time_AEST_txt, tz="Australia/Sydney")  
#attributes(release_time)$tzone <- "UTC"
#print_sol = current_time_UTC > release_time


# stri_rand_strings(12, sample(4:6, 12, replace=TRUE))
rand_sol_code = c("wqib",   "UnxCQQ", "sdLi",   "0ySXQ",  "e3nP",   "80WX",   
                  "zxOl",   "zoZt",   "ycV8Ky", "SJwwO",  "iKKpMc", "DwDx")

# Loop for builds
#Rmds <- list.files(pattern='.*\\.Rmd')
Rmds <- list.files(pattern='Lab_.*\\.Rmd')

for (i in 1:length(Rmds)) {
  Rmd = Rmds[i]
  match <- regexec('^(.*)\\.Rmd$', Rmd)
  name <- substring(Rmd, 1, nchar(Rmd)-4)
  output_q <- paste(name, '_Q.html', sep="")
  #  output_s <- paste(name, '_S.html', sep="")
  output_s <- paste(name, '_S_',rand_sol_code[Rmd_index[Rmd == Rmds]],'.html', sep="")
  
  
#  rmarkdown::render(
#    input=Rmd,
#    output_format='html_document',
#    output_file=output_q,
#    params=list(inc_solu=print_sol[i]))

  
  rmarkdown::render(
    input=Rmd,
    output_format='html_document',
    output_file=output_q,
    params=list(inc_solu=FALSE))
  
  rmarkdown::render(
    input=Rmd,
    output_format='html_document',
    output_file=output_s,
    params=list(inc_solu=TRUE))
}

#files <- list.files(pattern='Lab*.*')
#files <- c(files, 'all.zip')
#links <- sapply(files, function(x) paste0(' - [', x, '](', URLencode(x), ')'))

# tweaky sort
#links <- gsub('-', '~', links, fixed=TRUE)
#links <- sort(links)
#links <- gsub('~', '-', links, fixed=TRUE)

#index <- paste0('# index\n\n', paste0(links, collapse='\n'))

#writeLines(index, 'index.Rmd')

#rmarkdown::render(
#  input='index.Rmd',
#  output_format='html_document',
#  output_file='index.html')
