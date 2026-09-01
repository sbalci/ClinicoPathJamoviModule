# Follow-ups from the `type: Output` wiring sweep (2026-08-31)

Filed while fixing the dead-Output bug class. Everything here was **seen and deliberately not
fixed** in that pass — either pre-existing and unrelated, or a distinct piece of work.
The sweep itself is done and gated by `tools/check_output_items_wired.py`.

## Background: why this bug class is invisible

`jmvcore::Output$enabled` resolves its gating option **by the result item's own name**
(`options$get(private$.name)`), and `Output$asProtoBuf()` wraps the entire payload in
`if (self$enabled)`. Three independent silent-failure mechanisms then stack up, every one a
jmvcore design choice rather than a module error:

- `Options$get()` returns `NULL` for an unknown name, silently.
- `Output$enabled` falls through `NULL` to `FALSE` rather than raising.
- `setValues()` succeeds regardless, so `isFilled()` is `TRUE` on a completely dead item.

**Any test asserting `isFilled()` passes on a dead item.** Assert `$enabled`.

---

## HIGH — statistical / clinical

- [ ] **`classicalSurvivalPower`: the Lachin-Foulkes "power" branch does not solve for power.**
      `R/classicalSurvivalPower.b.R:~185-232` runs `nSurvival` at the requested beta and then
      rescales, so the reported power is a transformation of the input rather than a solution.
      Pre-existing; found while adding the `events`/`hazard_ratio` rejection.
- [ ] **`classicalSurvivalPower`: `.generate_power_curve_data()` returns 0 rows for Schoenfeld.**
      `R/classicalSurvivalPower.b.R:636+` only computes powers on the Lachin-Foulkes path, so the
      power curve is silently empty for the other method.
- [ ] **`mixedcox`: three plot methods only call `setState(NULL)`.**
      `.plotFixedEffects`, `.plotRandomEffects`, `.plotClusterSurvival` render empty placeholders
      while their option toggles are live. Has an in-file TODO already.

## MEDIUM — correctness / staleness

- [ ] **`clearWith` completeness on newly-live Output columns.** An incomplete `clearWith` leaves a
      stale column in the user's spreadsheet, which only became reachable now that these items are
      delivered. `Output$fromProtoBuf` sets the stale flag only when a changed option is in
      `clearWith`, so `isNotFilled()` returns FALSE and the write is skipped.
      - `latentbiomarker` `save_factor_scores`: omits `factor_score_method`, `standardize_scores`,
        `adjusters`, `dep_time`, `dep_event` — all of which change the scores.
      - `datetimeconverter`: inconsistent across its 13 Output items; only `month_out` and
        `monthname_out` list their own option name.
- [ ] **`mixedcox` / `surveysurvival` still compute `calculated_time` internally**
      (`R/mixedcox.b.R:183`, `R/surveysurvival.b.R:255`) but no longer declare an Output item for
      it. If either graduates out of the `SurvivalD` dev menu, wiring the canonical triple is a
      small change — the source column already exists.
- [ ] **`refs:` naming.** `jamovi/mixedcox.r.yaml` and `jamovi/surveysurvival.r.yaml` cite
      `ClinicoPathJamoviModule` (the repo name) rather than the package name.
- [ ] **`tests/testthat/test-classicalSurvivalPower.R`: four generated-template assertions that can
      never pass.** The public wrapper returns `analysis$results` (an R6 Results object), so
      `expect_true(is.list(model))` and `inherits(model, 'jmvcoreClass')` both fail. Rewrite to
      assert on the results object.

## LOW — hygiene

- [ ] **A stray `[1] TRUE` is printed to stdout** on every `survival` and `agreement` run in a bare
      Rscript session. Pre-existing auto-print, harmless in jamovi, noisy in tests.
- [ ] **`color: #666` on translucent-tinted panels** (unreadable in jamovi's dark theme) appears
      across ~40 `.b.R` files. `tools/theme_safe_html.py` is blind to this mirror case — it only
      catches an opaque background with no foreground. Grep `color: #` directly.
- [ ] **The fatal `visible:` pattern (`&&` with a List/String operand)** was found still live in
      `classicalSurvivalPower` despite the 2026-08-14 module-wide sweep. Re-run that sweep.
- [ ] **`classicalSurvivalPower` option names** `export_results` / `export_power_curve` still read
      as data-export in generated R syntax, though they now only drive an on-screen summary.
      Renaming is breaking; left for a version that can absorb it.
- [ ] **`jmvcore::Output$setTitle(title, key, index)` has an inverted-looking guard**: when `key`
      IS found it does `if (!is.na(index)) return()` and silently does nothing. Only the
      index/default path works. Worth reporting upstream.
- [ ] **`tools/ui_harness/render_ui.sh agreement`** fails with `ReferenceError: require is not
      defined`, before and after the change — the harness itself, not the `.u.yaml`. It works for
      other analyses (`timeinterval` renders clean), so something in `agreement.u.yaml` trips a
      code path the harness stub does not implement.

## Known-good, recorded so nobody re-derives it

- `type: Output` options take **no** `default:` — the compiler's `optionify()` emits only the bare
  name and silently discards every other property. 52/52 in this module omit it.
- An Output option is **not** an argument of the generated R wrapper (filtered out of the
  signature, the `Options$new()` call and the roxygen `@param`). From the R API `$enabled` is
  always FALSE; gate the backend write on `isNotFilled()` alone, never on `self$options$<name>`.
- Adding the **first** Output option to an analysis flips `weightsSupport` from `'auto'` to
  `'none'` (`header.template:62`); removing the last one flips it back.
- `jmvcore::format`'s placeholder regex is `\{ *[A-Za-z][A-Za-z0-9]* *\}` — **no underscore**. A
  `varTitle: '`${ some_option }`'` ships to jamovi verbatim as the user's column name. Use a static
  `varTitle` plus `setTitle()` at runtime. `tools/check_output_items_wired.py` gates this.
- `jamovi/0000.yaml` carries no options for any analysis, so renaming an option produces a
  zero-byte diff there.
