# Self-check for the release_gate.py rules taken from dev.jamovi.org (Distributing Modules,
# Module Datasets; added 2026-10-08): builds a throwaway module that breaks each rule once and
# asserts the gate notices. Run: python3 tools/test_release_gate.py
import os, sys, subprocess, tempfile, importlib.util

GATE = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'release_gate.py')


def write(root, path, text=''):
    p = os.path.join(root, path)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    open(p, 'w').write(text)


with tempfile.TemporaryDirectory() as m:
    subprocess.run(['git', 'init', '-q', m], check=True)
    write(m, 'DESCRIPTION', 'Package: fake\nLicense: AGPL-3\n')
    write(m, 'build/R4.6.0-x/badpkg/DESCRIPTION', 'License: AGPL-3 | file LICENSE\n')
    write(m, 'build/R4.6.0-x/okpkg/DESCRIPTION', 'License: GPL-2\n')
    write(m, 'dist/out.zip', 'x')
    for f in ('a.omv', 'b.omv', 'c.xlsx'):
        write(m, 'data/' + f)
    write(m, 'jamovi/0000.yaml', 'name: fake\ndatasets:\n'
          '  - {name: A, path: a.omv, description: first}\n'
          '  - {name: A, path: b.omv, description: second}\n'
          '  - {name: C, path: c.xlsx, description: third}\n'
          '  - {path: a.omv}\n')

    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    rg.check_license(); rg.check_artifacts(); rg.check_dataset_descriptions()
    fails, warns = '\n'.join(rg.FAIL), '\n'.join(rg.WARN)

    assert 'does not accept AGPL-3' in fails, fails                   # module's own licence
    assert 'badpkg' in warns and 'okpkg' not in warns, warns          # shipped AGPL dependency
    assert 'dist/out.zip' in fails, fails                             # dist/ is a build artifact
    assert "name 'A' used twice" in fails, fails                      # dataset name is unique
    assert 'c.xlsx is not .omv or .csv' in fails, fails               # dataset format
    assert 'lacks name/description' in fails, fails                   # required dataset fields
    print('OK: dev.jamovi.org distribution rules all fire')

# Round 8 (2026-10-23 meddecide) checks: one analysis breaks each rule, a second is the clean twin.
with tempfile.TemporaryDirectory() as m:
    write(m, 'jamovi/bad.a.yaml', '''name: bad
menuGroup: fake
options:
  - name: refNegative
    type: String
  - name: range
    type: String
    default: '0.8,1.0'
  # number-list: open-ended
  - name: cutoffs
    type: String
    default: '1,2,3'
  - name: showGuidance
    type: Bool
    default: true
  - name: plotTheme
    type: List
    options: [a, b]
  - name: lookTheme
    type: List
    options:
      - name: jamovi
      - name: classic
    default: jamovi
''')
    write(m, 'jamovi/bad.u.yaml', '''children:
  - type: CollapseBox
    label: Options
    children:
      - type: VariableSupplier
        children: []
  - type: VariableSupplier
    children: []
  - type: ComboBox
    name: method
    options: [a, b]
  - type: Label
    label: Plot Style
  - type: Label
    label: 'Explain the warning'
  - type: TargetLayoutBox
    label: Patient identifier
  - type: TargetLayoutBox
    label: Outcome Variable (Optional)
''')
    write(m, 'jamovi/bad.r.yaml', '''items:
  - name: instructions
    type: Html
  - name: someHeading
    type: Preformatted
  - name: filled
    type: Preformatted
''')
    write(m, 'R/bad.b.R', '''badClass <- R6::R6Class("badClass", private = list(
    .run = function() {
        on.exit(for (x in items) x$setVisible(FALSE))
        self$results$text$setContent("")
        jmvcore::reject("bad")
    },
    .normal = function() {
        self$results$text$setContent("")
        return()
        jmvcore::reject("never")
    },
    .grid = function() { paste0("<table>", x, "</table>") },
    .populateInstructions = function() { "<table>help</table>" },
    .fill = function() {
        self$results$filled$setContent("x")
        tbl$setNote("error", "failed")
        tbl$setNote("error", NULL)
        tbl$setRow(rowKey = "k", values = list(value = format(k0, digits = 3)))
        x <- .("Fleiss kappa")
        y <- .("Standardization exists to equalise the contribution; the optimism correction")
    }
    # avoids the jmvcore::Notice serialization error
    #
    #
    #
    # NOTE (corrected): jmvcore::Notice serializes fine
))
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    for fn in (rg.check_html_tables, rg.check_error_presentation, rg.check_variables_first,
               rg.check_level_as_string, rg.check_number_list_strings, rg.check_default_guidance,
               rg.check_empty_preformatted, rg.check_setnote_errors, rg.check_uyaml_structure,
               rg.check_plot_theme_option, rg.check_label_notation, rg.check_text_number_cells,
               rg.check_notice_myths, rg.check_collapsebox_titlecase, rg.check_label_case):
        fn()
    warns = '\n'.join(rg.WARN)
    assert 'bad::.grid (1)' in warns and '.populateInstructions' not in warns, warns  # help is exempt by name
    assert 'bad::.run on.exit hides' in warns and 'bad::.run blanks before reject()' in warns, warns
    assert '.normal' not in warns, warns                              # clearing before a normal return is right
    assert 'supplier(s) in a CollapseBox' in warns and 'CollapseBox before the variables' in warns, warns
    assert 'bad::refNegative' in warns, warns
    assert 'bad::range' in warns and 'bad::cutoffs' not in warns, warns  # open-ended list waived by comment
    assert 'showGuidance default true' in warns and 'instructions always shown' in warns, warns
    assert 'bad::someHeading' in warns and 'bad::filled' not in warns, warns
    assert '1 shipped setNote' in warns, warns                        # setNote("error", NULL) is a clear
    assert 'method options:' in warns and '"Plot Style"' in warns, warns
    assert 'bad::plotTheme' in warns and 'bad::lookTheme' not in warns, warns  # follows jamovi by default
    assert 'bad.u.yaml:15' in warns and 'bad.b.R:' in warns, warns    # "Explain ..." label; bare eponym
    assert '1 shipped cells receive R-formatted numbers' in warns, warns
    assert '1 shipped comments repeat' in warns, warns                # the corrected comment is not a hit
    assert 'bad: Patient identifier' in warns and 'Outcome Variable' not in warns, warns  # variable boxes too
    fails = '\n'.join(rg.FAIL)
    assert 'equalis' in fails and 'optimis' not in fails, fails        # "optimism" is not British

print('round-8 checks: ok')

# visible: jmvcore evaluates only `^\([$A-Za-z]`; `((`, `(!` and `( x` are always visible.
with tempfile.TemporaryDirectory() as m:
    write(m, 'jamovi/v.r.yaml', '''items:
  - name: a
    visible: ((x || y) && z)
  - name: b
    visible: (!x)
  - name: c
    visible: ( x)
  - name: d
    visible: (z && (x || y))
  - name: e
    visible: ($x)
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    rg.check_visible_bang()
    warns = '\n'.join(rg.WARN)
    assert 'v.r.yaml:3' in warns and 'v.r.yaml:5' in warns and 'v.r.yaml:7' in warns, warns
    assert 'v.r.yaml:9' not in warns and 'v.r.yaml:11' not in warns, warns
print('visible-expression check: ok')

# setState payload: a heavy object as the VALUE of an innocent key (library-audit 2026-09-15
# jsurvival [LOW], oddsratio plot_nomogram `list(data = mydata, ...)`) was invisible to the key-side pattern.
os.chdir(os.path.dirname(os.path.dirname(GATE)))   # the earlier blocks left a deleted temp dir as cwd
sys.argv = ['release_gate.py']
spec = importlib.util.spec_from_file_location('release_gate', GATE)
rg = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rg)
heavy = lambda payload: sorted({m.group(1) for m in rg._HEAVY.finditer(payload)} |
                               {m.group(1) for m in rg._HEAVY_VALUE.finditer(payload)})
assert heavy('list(data = mydata, dependent = dv)') == ['mydata'], heavy('list(data = mydata, dependent = dv)')
assert heavy('list(fit = cox_fit)') == ['cox_fit', 'fit'] or 'fit' in heavy('list(fit = cox_fit)')
assert heavy('list(nom = private$.nom_object)') == [], heavy('list(nom = private$.nom_object)')
assert heavy('list(data = plotData, fitted = fitted_vals)') == [], heavy('list(data = plotData, fitted = fitted_vals)')
assert heavy('list(n = nrow(mydata), ok = x == model)') == [], heavy('list(n = nrow(mydata), ok = x == model)')
print('setState value-side payload check: ok')

# Round 9 (library-audit 2026-11-20 jsurvival): the gate read 0 for a HIGH and three LOWs. Each
# blind spot below is one the reviewer's list exposed.
with tempfile.TemporaryDirectory() as m:
    write(m, 'jamovi/bad.a.yaml', 'name: bad\nmenuGroup: Survival\noptions: []\n')
    write(m, 'jamovi/bad.r.yaml', 'items:\n  - name: plot\n    type: Image\n    renderFun: .plot\n    requiresData: true\n'
          '  - name: plotRows\n    type: Image\n    renderFun: .plotRows\n    requiresData: true\n')
    write(m, 'R/utils-bad.R', '.describeThing <- function(res) {\n'
          '    paste0("<table><tr><td>Event</td><td>", res$n, "</td></tr></table>")\n}\n')
    write(m, 'R/bad.b.R', r'''badClass <- R6::R6Class("badClass", private = list(
    .run = function() {
        self$results$info$setContent(.describeThing(res))
        plotData <- list(cleanData = cleanData, title = title)
        image$setState(plotData)
        labels <- list(title = title, n = nrow(mydata))
        other$setState(labels)
        jmvcore::reject("Unknown survival type: ", kind)
        jmvcore::reject("Variable '{}' not found", code = NULL, name)
        jmvcore::reject(.("One translated sentence that needs no value at all"))
    },
    .formatScoreGuide = function(n) {
        paste0("<table><tr><th>", .("Metric"), "</th><td>", n, "</td></tr></table>")
    },
    .populateInstructions = function() {
        paste0("<table><tr><td>", .("Choose a variable to begin the analysis"), "</td></tr></table>")
    },
    .cox = function(zph) {
        # rendered as HTML to avoid Notice protobuf issues
        self$results$ph$setContent(paste(utils::capture.output(print(zph)), collapse = "\n"))
    },
    .explain = function() {
        paste0("<p>", "The median time is the first time survival drops below one half", "</p>",
               "<p>", "A wide interval means the estimate is not precise at all", "</p>",
               "<p>", "Read the numbers at risk before trusting the tail", "</p>")
    },
    .codeText = function() {
        # i18n-exempt: generated R code, not prose
        paste0("# Refit this development analysis using upstream packages here",
               "# Supply the same data frame named data to the function",
               "# This code does not validate the model or anything else")
    },
    .translated = function() {
        paste0(.("The median time is the first time survival drops below one half"),
               .("A wide interval means the estimate is not precise at all"),
               .("Read the numbers at risk before trusting the tail"))
    },
    .plot = function(image, ...) {
        state <- image$state
        labelled <- private$.getData()
        TRUE
    },
    .switched = function() {
        third$setState(if (isTRUE(shown[[name]])) plotData else NULL)
        # state-payload: a Kaplan-Meier curve is drawn from rows; measured
        fourth$setState(if (on) plotData else NULL)
    },
    .plotRows = function(image, ...) {
        # render-route: data - the state is a flag
        flag <- image$state
        rows <- private$.cleandata()
        TRUE
    }
))
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    for fn in (rg.check_html_tables, rg.check_state_payload, rg.check_notice_myths,
               rg.check_untranslated_prose, rg.check_reject_positional, rg.check_render_state_and_data):
        fn()
    warns = '\n'.join(rg.WARN)
    assert 'bad::.formatScoreGuide (1)' in warns, warns               # a help-like NAME does not exempt a data cell
    assert '.populateInstructions' not in warns, warns                # cells that splice .() only are help
    assert 'bad::.cox (printed)' in warns, warns                      # a printed object pushed into a result
    assert 'utils-bad.R::.describeThing (1)' in warns, warns          # a file-level helper a shipped analysis calls
    assert 'bad.b.R:5 (plotData <- list(cleanData))' in warns and 'bad.b.R:7' not in warns, warns
    assert 'bad.b.R:19' in warns, warns                               # "Notice protobuf issues"
    assert 'bad::.explain (3)' in warns and '.translated' not in warns and '.codeText' not in warns, warns
    fails = '\n'.join(rg.FAIL)                                        # blocking since 2026-11-22
    assert 'bad.b.R:8' in fails and 'bad.b.R:9' not in fails and 'bad.b.R:10' not in fails, fails
    assert 'bad::.plot' in warns, warns                               # reads $state and loads the dataset
    assert warns.count('plotData <- list(cleanData)') == 2, warns     # the same payload behind `if (...) else NULL`
    assert 'bad::.plotRows' not in warns, warns                       # a declared data route with a rowless state
print('round-9 checks: ok')

# Round 4 jjstatsplot (library-audit 2026-09-16 jjstatsplot) checks:
# - 00refs.yaml missing author/year/title (FAIL)
# - dead Remotes: entry not in Imports/Suggests/Depends (WARN)
# - dotted option names in .a.yaml (WARN)
# - raw-data / ggplot object setState (setState(plot), setState(as.data.frame(data)), setState(state_df))
# - counter-variable sequential addRow(rowKey = row_num) in .run()
# - comma-first R6 method definition (`, .plot2 = function(`) still traced by check_requires_data
with tempfile.TemporaryDirectory() as m:
    write(m, 'DESCRIPTION', 'Package: fake\nImports:\n    waffle\nRemotes:\n'
          '    gastonstat/arcdiagram@7be212cd632f182d332eaae088cd6f1db55d5848,\n'
          '    ClinicoPath/waffle@f5150355d99521417cb406156b5ee0c626fe94a8\n')
    write(m, 'jamovi/00refs.yaml', '''refs:
  ggdist:
    type: software
    author: ""
    year: ""
    title: "ggdist: R package"
    url: https://CRAN.R-project.org/package=ggdist
  waffle:
    type: software
    author: "Bob Rudis"
    year: 2024
    title: "waffle"
    url: https://github.com/ClinicoPath/waffle
''')
    write(m, 'jamovi/bad.a.yaml', '''name: bad
menuGroup: JJStatsPlot
options:
  - name: test.value
    type: Number
    default: 0
  - name: testValue
    type: Number
    default: 0
''')
    write(m, 'jamovi/bad.r.yaml', '''items:
  - name: corr
    type: Table
    columns: []
  - name: plot
    type: Image
    renderFun: .plot
  - name: plot2
    type: Image
    renderFun: .plot2
    requiresData: true
refs:
  - ggdist
  - waffle
''')
    write(m, 'R/bad.b.R', '''badClass <- R6::R6Class("badClass", private = list(
    .init = function() {
        self$results$corr$setVisible(FALSE)
    },
    .run = function() {
        self$results$plot$setState(plot)
        self$results$plot$setState(as.data.frame(data))
        self$results$plot$setState(state_df)
        table <- self$results$corr
        row_num <- 1
        table$addRow(rowKey = row_num, values = list(measure = .("Pearson")))
        row_num <- row_num + 1
        table$addRow(rowKey = row_num, values = list(measure = .("R-squared")))
        row_num <- row_num + 1
        table$addRow(rowKey = row_num, values = list(measure = .("Slope")))
    }
    , .plot = function(image, ...) {
        TRUE
    }
    , .plot2 = function(image, ...) {
        d <- self$data
        TRUE
    }
))
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    for fn in (rg.check_refs, rg.check_dead_remotes, rg.check_dotted_options,
               rg.check_state_payload, rg.check_init_row_structure, rg.check_requires_data):
        fn()
    fails, warns = '\n'.join(rg.FAIL), '\n'.join(rg.WARN)
    assert 'ggdist (author/year)' in fails and 'waffle' not in fails, fails
    assert 'arcdiagram' in warns and 'waffle' not in warns, warns
    assert 'bad::test.value' in warns and 'bad::testValue' not in warns, warns
    assert 'setState(plot)' in warns and 'setState(as.data.frame(data))' in warns and 'setState(state_df)' in warns, warns
    assert 'bad::corr (3 counter rows)' in warns, warns
    assert 'bad:plot2' not in warns, warns                            # comma-first .plot2 reads self$data -> NOT surplus
print('round-4 jjstatsplot checks: ok')

# check-module 2026-11-27 jjstatsplot: a handler that re-raises (stop(e)) or rejects again does not
# swallow a reject(); only the handler that carries on is listed.
with tempfile.TemporaryDirectory() as m:
    write(m, 'jamovi/rj.a.yaml', 'name: rj\nmenuGroup: fake\noptions: []\n')
    write(m, 'R/rj.b.R', '''rjClass <- R6::R6Class("rjClass", private = list(
    .check = function() {
        jmvcore::reject("bad input")
    },
    .run = function() {
        tryCatch(private$.check(), error = function(e) {
            private$.note <- conditionMessage(e)
            NULL
        })
    },
    .run2 = function() {
        tryCatch(private$.check(), error = function(err) {
            private$.cache <- NULL
            stop(err)
        })
    },
    .run3 = function() {
        tryCatch(private$.check(), error = function(e) jmvcore::reject(conditionMessage(e)))
    }
))
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    rg.check_reject_swallowed()
    out = '\n'.join(rg.FAIL + rg.WARN)
    assert 'rj.b.R:6 .run (.check)' in out, out                       # swallowing handler listed
    assert '.run2' not in out and '.run3' not in out, out             # re-raising handlers are not
print('reject_swallowed re-raise rule: ok')

# check-module 2026-11-28 multisurvival: a per-run cache wrapper rejects only on its first call. When
# the analysis also calls it outside every tryCatch, calls inside a handler return the cache.
with tempfile.TemporaryDirectory() as m:
    write(m, 'jamovi/cw.a.yaml', 'name: cw\nmenuGroup: fake\noptions: []\n')
    write(m, 'R/cw.b.R', '''cwClass <- R6::R6Class("cwClass", private = list(
    .clean_impl = function() {
        jmvcore::reject("no rows")
    },
    .clean = function() {
        if (!private$.done) {
            private$.cache <- private$.clean_impl()
            private$.done <- TRUE
        }
        private$.cache
    },
    .other_impl = function() {
        jmvcore::reject("bad")
    },
    .other = function() {
        if (!private$.odone) {
            private$.ocache <- private$.other_impl()
            private$.odone <- TRUE
        }
        private$.ocache
    },
    .run = function() {
        d <- private$.clean()
        tryCatch(private$.clean(), error = function(e) NULL)
        tryCatch(private$.other(), error = function(e) NULL)
    }
))
''')
    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    rg.check_reject_swallowed()
    out = '\n'.join(rg.FAIL + rg.WARN)
    assert '(.clean)' not in out, out                                 # cached, called unguarded: not listed
    assert '(.other)' in out, out                                     # cached, only ever guarded: listed
print('reject_swallowed cache-wrapper rule: ok')
