
local({
  # set a stable mirror
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.microsoft.com/snapshot/2020-01-01"
  options(repos = r)
})

if ( ! require(rmarkdown))
  install.packages('rmarkdown')

Rmds <- list.files(pattern='.*\\.Rmd')

# stri_rand_strings(12, sample(4:6, 12, replace=TRUE))
rand_sol_code = c("wqib",   "UnxCQQ", "sdLi",   "0ySXQ",  "e3nP",   "80WX",   
              "zxOl",   "zoZt",   "ycV8Ky", "SJwwO",  "iKKpMc", "DwDx")

Rmd_index = 1:length(Rmds)

for (Rmd in Rmds) {
  match <- regexec('^(.*)\\.Rmd$', Rmd)
  name <- substring(Rmd, 1, nchar(Rmd)-4)
  output_q <- paste(name, '_Q.html', sep="")
#  output_s <- paste(name, '_S.html', sep="")
  output_s <- paste(name, '_S_',rand_sol_code[Rmd_index[Rmd == Rmds]],'.html', sep="")

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
