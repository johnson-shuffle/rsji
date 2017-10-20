# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(SAScii)
library(LaF)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# files
fln <- list.files('~/Desktop/nibrs', pattern = '^ICPSR_')
fln <- file.path('~/Desktop/nibrs', fln[13:16])

# ------------------------------------------------------------------------------
# download the raw data
# ------------------------------------------------------------------------------
nibr_get <- function(series, email = NULL, password = NULL, dest_dir = NULL) {
  
  pag <- GET(
    'http://www.icpsr.umich.edu/cgi-bin/bob/zipcart2',
    query = list(
      path = 'ICPSR',
      study = series,
      bundle = 'all',
      ds = '',
      dups = 'yes'
    ))
  pag <- pag$request$url
  htm <- html_session(pag)
  frm <- html_form(htm)[[3]] %>% set_values(email = email, password = password)
  suppressMessages(trm <- submit_form(htm, frm) %>%  jump_to(pag))
  suppressMessages(
    out <- submit_form(trm, html_form(trm)[[3]]) %>%
      follow_link("download your files here")
  )
  
  # write to disk
  fln <- str_c('ICPSR_', sprintf('%05d', series),  '.zip')
  writeBin(content(out$response, "raw"), file.path(dest_dir, fln))
  
}

# nibr_get(
#   ..., 
#   email = 'j4johnson@ucsd.edu',
#   password = 'Econ,grey2',
#   dest_dir = '/Volumes/32GB_BRO/nibrs/'
  )

# ------------------------------------------------------------------------------
# locate segment
# ------------------------------------------------------------------------------
nibr_seg <- function(file, segment_name) {
  
  fls <- unzip(file, list = T)$Name
  manifest <- grep('manifest', fls) ; unzip(file, files = fls[manifest])
  manifest_txt <- readLines(unzip(file, fls[manifest]))
  segment <- manifest_txt[grep('DS\\d{4}', manifest_txt)]
  
  # remove directory
  unlink(str_c('ICPSR_', str_extract(file, '\\d{5}')), recursive = T)
  
  return(grep(segment_name, segment, ignore.case = T)[1])
  
}

admi_seg <- map_int(fln, nibr_seg, segment_name = 'administrative')
offs_seg <- map_int(fln, nibr_seg, segment_name = 'offense')
offd_seg <- map_int(fln, nibr_seg, segment_name = 'offender')
arrs_seg <- map_int(fln, nibr_seg, segment_name = 'arrestee segment')

# ------------------------------------------------------------------------------
# extract the data
# ------------------------------------------------------------------------------
nibr_zip <- function(file, segment, ori = NULL, pb = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # series and segment
  number <- str_extract(file, '\\d{5}')
  segment <- str_pad(segment, 4, pad = '0')
  
  # extract setup and data files
  seg_d <- str_c("/DS", segment, '/')
  set_f <- str_c(number , segment , "Setup.sas", sep = "-")
  dat_f <- str_c(number , segment , "Data.txt", sep = "-")
  set <- unzip(file, str_c("ICPSR_", number, seg_d, set_f))
  txt <- unzip(file, str_c("ICPSR_", number, seg_d, dat_f))
  
  # read via SAScii and LaF
  set_parse <- parse.SAScii(set)
  ct <- set_parse[, "char"]
  ct[ct == T] <- "string"
  ct[ct == F] <- "double"
  cw <- set_parse[, "width"]
  cn <- set_parse[, "varname"]
  fwf <- laf_open_fwf(
    txt,
    column_names = cn,
    column_types = ct,
    column_widths = cw
    )
  dat <- fwf[, ]
  
  # labels for each column
  lab <- readLines(set)
  lab_pos <- grep('^LABEL$', str_trim(lab))
  lab_vec <- lab[(lab_pos + 1):(lab_pos + ncol(dat))]
  labs <- str_extract(lab_vec, "\"[^\\\"]+\"|\'[^\\\']+\'")
  labs <- str_replace_all(labs, "\"", '')
  labs <- str_replace_all(labs, "'", '')
  attr(dat, 'variable.labels') <- labs
  
  # filter based on optional ori
  if (!is.null(ori)) { dat <- filter_(dat, str_c(names(dat)[3], ' %in% ori')) }
  
  # remove directory
  unlink(str_c("ICPSR_", number), recursive = T)
  
  # return
  return(dat)
  
}

# ------------------------------------------------------------------------------
# adminstrative segment
# ------------------------------------------------------------------------------
pb <- progress_estimated(length(fln))
admi <- map2(fln, admi_seg, ~nibr_zip(.x, .y, ori = 'WASPD0000', pb = pb))
nibr1 <- do.call(rbind, admi)

# ------------------------------------------------------------------------------
# offense segment
# ------------------------------------------------------------------------------
pb <- progress_estimated(length(fln))
offs <- map2(fln, offs_seg, ~nibr_zip(.x, .y, ori = 'WASPD0000', pb = pb))
nibr2 <- do.call(plyr::rbind.fill, offs)

# fix labels
nibr2 <- nibr2[names(offs[[4]])]
attr(nibr2, 'variable.labels') <- attr(offs[[4]], 'variable.labels')

# ------------------------------------------------------------------------------
# offender segment
# ------------------------------------------------------------------------------
pb <- progress_estimated(length(fln))
offd <- map2(fln, offd_seg, ~nibr_zip(.x, .y, ori = 'WASPD0000', pb = pb))
nibr5  <- do.call(plyr::rbind.fill, offd)

# fix labels
nibr5 <- nibr5[names(offd[[4]])]
attr(nibr5, 'variable.labels') <- attr(offd[[4]], 'variable.labels')

# ------------------------------------------------------------------------------
# arrestee segment
# ------------------------------------------------------------------------------
pb <- progress_estimated(length(fln))
arrs <- map2(fln, arrs_seg, ~nibr_zip(.x, .y, ori = 'WASPD0000', pb = pb))
nibr6  <- do.call(rbind, arrs)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, nibr1, temporary = F, overwrite = T)
copy_to(db, nibr2, temporary = F, overwrite = T)
copy_to(db, nibr5, temporary = F, overwrite = T)
copy_to(db, nibr6, temporary = F, overwrite = T)
