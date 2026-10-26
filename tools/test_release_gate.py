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
