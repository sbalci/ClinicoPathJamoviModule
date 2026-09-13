# Run from the repository root: Rscript development-scripts/review_survivalPower_static.R
source('R/survivalPower.h.R'); source('R/survivalPower_distributions.R'); source('R/survivalPower.b.R')
out <- 'development-ideas/survivalPower-review-2026-09-13'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
src <- readLines('R/survivalPower.b.R')
ast <- parse('R/survivalPower.b.R', keep.source=TRUE)
scalar <- character(); translations <- character()
walk <- function(x) {
 if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
 if (is.call(x)) {
  head <- paste(deparse(x[[1]]), collapse='')
  if (head %in% c('jmvcore::.', '.')) translations <<- c(translations, as.character(x[[2]]))
  if (head %in% c('if','while')) {
   cond <- paste(deparse(x[[2]]), collapse=' ')
   if (grepl('(?<![&])&(?![&])|(?<![|])\\|(?![|])',cond,perl=TRUE)) scalar <<- c(scalar,cond)
  }
 }
 for (i in seq_along(x)) {
   if (!is.symbol(x[[i]]) || nzchar(as.character(x[[i]]))) walk(x[[i]])
 }
}
walk(ast)
usage <- list()
for (nm in names(survivalPowerClass$private_methods)) {
 f <- survivalPowerClass$private_methods[[nm]]
 if (!is.function(f)) next
 messages <- character()
 codetools::checkUsage(f, name=nm, report=function(msg) messages <<- c(messages,msg), all=TRUE)
 if(length(messages)) usage[[nm]] <- messages
}
jsonlite::write_json(usage,file.path(out,'r6-usage-review.json'),pretty=TRUE,auto_unbox=TRUE)
a <- yaml::read_yaml('jamovi/survivalPower.a.yaml'); r <- yaml::read_yaml('jamovi/survivalPower.r.yaml')
registry <- yaml::read_yaml('jamovi/00refs.yaml')$refs
# Items can be unnamed lists; recurse independently to include every nested entry.
get_refs <- function(x) {
 if(!is.list(x)) return(character())
 own <- if (!is.null(names(x)) && 'refs' %in% names(x)) unlist(x[['refs']]) else character()
 c(own, unlist(lapply(x,get_refs),use.names=FALSE))
}
refs <- unique(c(get_refs(a),get_refs(r)))
checks <- do.call(rbind,lapply(refs,function(key) {
 e <- registry[[key]]
 data.frame(key=key,defined=!is.null(e),author_ok=length(e$author)>0 && any(nzchar(e$author)),
            year_ok=length(e$year)>0 && any(nzchar(e$year)))
}))
write.csv(checks,file.path(out,'reference-validation.csv'),row.names=FALSE)
imports <- trimws(sub('\\s*\\(.*','',strsplit(read.dcf('DESCRIPTION')[1,'Imports'],',')[[1]]))
pkgs <- unique(sub('::$','',unlist(regmatches(src,gregexpr('[A-Za-z][A-Za-z0-9.]*::',src)))))
used <- unique(sub('self$options$','',unlist(regmatches(src,
 gregexpr('self\\$options\\$[A-Za-z0-9_]+',src))),fixed=TRUE))
options <- vapply(a$options,`[[`,'','name')
# Parse complete PO msgids, including continuations, before exact matching.
po_msgids <- function(path) {
  ids <- character()
  current <- NULL
  reading_id <- FALSE
  for (line in readLines(path, warn = FALSE)) {
    if (grepl('^msgid ', line)) {
      if (!is.null(current)) ids <- c(ids, current)
      current <- jsonlite::fromJSON(sub('^msgid ', '', line))
      reading_id <- TRUE
    } else if (reading_id && grepl('^"', line)) {
      current <- paste0(current, jsonlite::fromJSON(line))
    } else if (grepl('^msgstr|^msgid_plural', line)) {
      reading_id <- FALSE
    }
  }
  if (!is.null(current)) ids <- c(ids, current)
  ids
}
tr_ids <- po_msgids('jamovi/i18n/tr.po')
jsonlite::write_json(list(scalar_vector_logic=scalar,
  namespace_packages=pkgs,imports_missing=setdiff(pkgs,imports),
  options_missing=setdiff(used,options),options_unread=setdiff(options,used),
  raw_non_ascii=grep('[^\x01-\x7f]',src,perl=TRUE),
  translated_messages=translations,
  translation_whitespace=translations[trimws(translations)!=translations],
  messages_missing_from_tr=setdiff(translations, tr_ids),
  refs=checks,menu_group=a$menuGroup),file.path(out,'static-checks.json'),pretty=TRUE,auto_unbox=TRUE)
cat('Static review complete; packages:',paste(pkgs,collapse=', '),'\n')

# The explicit review set includes non-default bug linters and suppresses repo style noise.
review_linters <- lintr::linters_with_defaults(
  commented_code_linter = NULL, line_length_linter = NULL,
  trailing_whitespace_linter = NULL, indentation_linter = NULL,
  object_name_linter = NULL, return_linter = NULL,
  sprintf_linter = lintr::sprintf_linter(),
  unreachable_code_linter = lintr::unreachable_code_linter(),
  duplicate_argument_linter = lintr::duplicate_argument_linter(),
  missing_argument_linter = lintr::missing_argument_linter()
)
lint <- as.data.frame(lintr::lint('R/survivalPower.b.R', linters = review_linters))
write.csv(lint, file.path(out, 'lintr.csv'), row.names = FALSE)
print(table(lint$linter))

baseline <- read.csv('development-ideas/survivalPower-fixes-2026-09-13/source-fingerprints.csv')
baseline$current_md5 <- unname(tools::md5sum(baseline$path))
baseline$unchanged <- baseline$md5 == baseline$current_md5
write.csv(baseline, file.path(out, 'source-verification.csv'), row.names = FALSE)
stopifnot(all(baseline$unchanged))
