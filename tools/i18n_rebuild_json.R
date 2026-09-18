# Rebuild <module>/inst/i18n/<code>.json from <module>/jamovi/i18n/*.po exactly as
# `jmvtools::install()` does (jamovi-compiler i18n.load() + createTranslationJSON()), without
# building or installing. prepare() never writes these files; mirror_to_submodule.R --regen
# restores the snapshot copy, which is stale after a catalog change.
#
#   Rscript --vanilla tools/i18n_rebuild_json.R <module dir>
#
# Verified 2026-09-17: on OncoPath's untrimmed catalogs the output was byte-identical to the
# json jmvtools::install() had written.
dir <- normalizePath(commandArgs(TRUE)[1], mustWork = TRUE)
i18n_js <- file.path(dirname(gsub('"', "", jmvtools:::jmcPath())), "i18n.js")
node <- Sys.which("node")
if (!nzchar(node)) stop("node is not on PATH")
script <- tempfile(fileext = ".mjs")
writeLines(c(
  "import fs from 'fs'; import path from 'path';",
  sprintf("import i18n from %s;", deparse(paste0("file://", i18n_js))),
  "const [src, out] = process.argv.slice(2);",
  "i18n.load(path.join(src, 'jamovi', 'i18n'));",
  "fs.mkdirSync(out, { recursive: true });",
  "for (const code in i18n.translations) {",
  "  if (code === 'c') continue;",
  "  fs.writeFileSync(path.join(out, code + '.json'), JSON.stringify(i18n.createTranslationJSON(code), null, 4));",
  "  console.log('wrote: inst/i18n/' + code + '.json');",
  "}"), script)
status <- system2(node, c(script, shQuote(dir), shQuote(file.path(dir, "inst", "i18n"))))
if (!identical(status, 0L)) stop("json rebuild failed (exit ", status, ")")
