# GUI enable conditions have a different grammar from result visibility expressions.
sp_ui_bindings <- function() {
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "survivalPower.u.yaml"))
  bindings <- list()
  walk <- function(control) {
    if (!is.null(control$enable)) bindings[[control$name]] <<- control$enable
    for (child in control$children) walk(child)
  }
  walk(ui)
  bindings
}

test_that("survivalPower UI bindings use supported operators and existing controls", {
  bindings <- sp_ui_bindings()
  schema <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "survivalPower.a.yaml"))
  options <- vapply(schema$options, `[[`, "", "name")
  expect_length(bindings, 18)
  for (name in names(bindings)) {
    expression <- bindings[[name]]
    expect_false(grepl("[=<>\"']", expression), info = name)
    operands <- strsplit(gsub("[()!]", "", expression), "&&|\\|\\|")[[1]]
    for (operand in trimws(operands)) {
      parts <- strsplit(operand, ":", fixed = TRUE)[[1]]
      expect_true(parts[1] %in% options, info = paste(name, operand))
      if (length(parts) == 2L) {
        option <- schema$options[[match(parts[1], options)]]
        if (option$type == "List") {
          expect_true(parts[2] %in% vapply(option$options, `[[`, "", "name"), info = name)
        }
      }
    }
  }
})

test_that("the installed jamovi client enables the intended survivalPower controls", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is unavailable")
  dist <- Sys.getenv("JAMOVI_CLIENT_DIST",
    "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist")
  client <- list.files(file.path(dist, "assets"), "^analysisui-.*[.]js$", full.names = TRUE)
  skip_if(length(client) != 1L, "Set JAMOVI_CLIENT_DIST to test the installed jamovi parser")
  schema <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "survivalPower.a.yaml"))
  defaults <- setNames(lapply(schema$options, `[[`, "default"),
    vapply(schema$options, `[[`, "", "name"))
  cases <- list()
  add <- function(controls, values, expected) {
    for (control in controls) {
      cases[[length(cases) + 1L]] <<- list(
        control = control, values = values, expected = expected)
    }
  }
  add(c("effect_size_type", "effect_size"), list(), TRUE)
  add(c("effect_size_type", "effect_size"),
    list(test_type = "non_inferiority", analysis_type = "effect_size"), FALSE)
  add("effect_size", list(effect_size_type = "rmst_difference"), FALSE)
  for (mode in c("sample_size", "power", "effect_size", "duration")) {
    add("power_level", list(analysis_type = mode), mode != "power")
    add("sample_size_input", list(analysis_type = mode), mode != "sample_size")
    add(c("sensitivity_analysis", "run_simulation_validation"),
      list(analysis_type = mode), mode %in% c("sample_size", "power"))
  }
  add("weibull_shape", list(), FALSE)
  add("weibull_shape", list(survival_distribution = "weibull"), TRUE)
  add(c("ni_margin", "ni_type"), list(), FALSE)
  add(c("ni_margin", "ni_type"), list(test_type = "non_inferiority"), TRUE)
  add(c("rmst_tau", "rmst_difference"), list(), FALSE)
  add(c("rmst_tau", "rmst_difference"), list(effect_size_type = "rmst_difference"), TRUE)
  add(c("rmst_tau", "rmst_difference"), list(test_type = "rmst_test"), TRUE)
  add(c("rmst_tau", "rmst_difference"), list(test_type = "non_inferiority",
    analysis_type = "effect_size", effect_size_type = "rmst_difference"), FALSE)
  add(c("number_of_arms", "multiple_comparisons"), list(), FALSE)
  add(c("number_of_arms", "multiple_comparisons"), list(study_design = "multi_arm"), TRUE)
  add(c("cluster_size", "icc"), list(), FALSE)
  add(c("cluster_size", "icc"), list(study_design = "cluster_randomized"), TRUE)
  for (looks in c(0L, 1L, 5L)) add("alpha_spending", list(interim_analyses = looks), looks > 0)
  for (simulate in c(FALSE, TRUE)) {
    add(c("simulation_runs", "simulation_seed"),
      list(run_simulation_validation = simulate), simulate)
  }
  expect_setequal(vapply(cases, `[[`, "", "control"), names(sp_ui_bindings()))

  inputs <- tempfile(fileext = ".json")
  script <- tempfile(fileext = ".cjs")
  on.exit(unlink(c(inputs, script)), add = TRUE)
  jsonlite::write_json(list(client = client, defaults = defaults,
    bindings = sp_ui_bindings(), cases = cases), inputs, auto_unbox = TRUE)
  writeLines(c(
    "const fs = require('fs'), vm = require('vm');",
    "const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));",
    "const source = fs.readFileSync(input.client, 'utf8');",
    "const start = source.indexOf('_resolveBindCode(e,t){');",
    "const end = source.indexOf('bindingsToActions(){', start);",
    "if (start < 0 || end < 0) throw Error('Installed client parser layout changed');",
    "const helper = source.match(/const Es=[^;]+;/)[0];",
    "const warnings = [];",
    "const parser = vm.runInNewContext(helper + 'new (class {' + source.slice(start, end) + '})()',",
    "  {console: {log: message => warnings.push(message)}});",
    "const results = input.cases.map(c => {",
    "  parser._resources = Object.fromEntries(Object.entries({...input.defaults, ...c.values})",
    "    .map(([key, value]) => [key, {value: () => value}]));",
    "  const binding = parser._resolveBinding(input.bindings[c.control], 0);",
    "  let actual = binding.bindFunction(parser._resources);",
    "  if (binding.inverted) actual = !actual;",
    "  return {control: c.control, expected: c.expected, actual: Boolean(actual)};",
    "});",
    "process.stdout.write(JSON.stringify({results, warnings}));"
  ), script)
  output <- system2(node, c(shQuote(script), shQuote(inputs)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  result <- jsonlite::fromJSON(paste(output, collapse = "\n"))
  expect_length(result$warnings, 0)
  expect_equal(result$results$actual, result$results$expected,
    info = paste(result$results$control[result$results$actual != result$results$expected],
      collapse = ", "))
})
