#!/usr/bin/env python3
"""Gate: every `type: Output` result item must have a gating option of the SAME name.

Why this exists
---------------
`jmvcore::Output$enabled` resolves its gating option by the RESULT ITEM'S OWN NAME:

    enabled = function() {
        value <- private$.options$get(private$.name)   # NOT some other option
        if (is.logical(value)) return(value)
        ...
        FALSE
    }

and `Output$asProtoBuf()` wraps the entire outputs payload in `if (self$enabled)`.
So a `.r.yaml` Output item with no `type: Output` option of the same name in the
paired `.a.yaml` is DEAD: the backend computes the column, `setValues()` succeeds,
`isFilled()` returns TRUE -- and jamovi never receives a single value.

Three independent silent-failure mechanisms stack up, every one a jmvcore design
choice rather than a module error:
  * Options$get() returns NULL for an unknown name, silently.
  * Output$enabled falls through NULL to FALSE rather than raising.
  * setValues() succeeds regardless, so isFilled() is TRUE on a dead item.
Which is why a test asserting isFilled() passes for the whole life of the bug.
The only reliable assertion is on `$enabled` -- or on the schema, here.

It also checks varTitle/varDescription for the underscore trap: jmvcore::format's
placeholder regex is \\{ *[A-Za-z][A-Za-z0-9]* *\\} -- NO underscore -- so a
`${ dx_date }` in a varTitle ships to jamovi verbatim as the user's column name.

Usage:  python3 tools/check_output_items_wired.py [--quiet]
Exit 1 if anything is unwired. Run from the repo root.
"""
import os
import re
import sys

try:
    import yaml
except ImportError:
    sys.exit("PyYAML required: pip3 install pyyaml")

PLACEHOLDER = re.compile(r"\$?\{\s*([A-Za-z][A-Za-z0-9_]*)\s*\}")
# jmvcore::format only substitutes names matching this; anything else ships literally.
INTERPOLABLE = re.compile(r"^[A-Za-z][A-Za-z0-9]*$")


def names_of_type(entries, wanted):
    """Names of every `type: <wanted>` entry, recursing into nested `items:`.

    Group and Array result items nest their children under `items:`, so a flat
    scan would report a green gate while a nested Output sat unwired. There are
    no nested Output items today; this keeps that from becoming a silent hole.
    """
    out = []
    for e in entries or []:
        if not isinstance(e, dict):
            continue
        if e.get("type") == wanted and "name" in e:
            out.append(str(e["name"]))
        if isinstance(e.get("items"), list):
            out.extend(names_of_type(e["items"], wanted))
    return out


def output_items(entries):
    """Every `type: Output` item dict, recursing the same way."""
    out = []
    for e in entries or []:
        if not isinstance(e, dict):
            continue
        if e.get("type") == "Output":
            out.append(e)
        if isinstance(e.get("items"), list):
            out.extend(output_items(e["items"]))
    return out


def main():
    quiet = "--quiet" in sys.argv
    jam = "jamovi"
    if not os.path.isdir(jam):
        sys.exit("run from the repository root (no ./jamovi directory)")

    unwired, literal_titles, checked = {}, {}, 0

    for fn in sorted(os.listdir(jam)):
        if not fn.endswith(".r.yaml"):
            continue
        base = fn[: -len(".r.yaml")]
        rpath = os.path.join(jam, fn)
        apath = os.path.join(jam, base + ".a.yaml")
        if not os.path.exists(apath):
            continue
        try:
            rdoc = yaml.safe_load(open(rpath, encoding="utf-8")) or {}
            adoc = yaml.safe_load(open(apath, encoding="utf-8")) or {}
        except yaml.YAMLError as exc:
            unwired.setdefault(base, []).append("UNPARSEABLE YAML: %s" % exc)
            continue

        items = rdoc.get("items") or []
        r_out = names_of_type(items, "Output")
        if not r_out:
            continue
        checked += 1

        a_out = names_of_type(adoc.get("options") or [], "Output")
        missing = [n for n in r_out if n not in a_out]
        if missing:
            unwired[base] = missing

        # An Output option carrying a default:/varTitle:/clearWith: is not an error --
        # the compiler's optionify() drops every property but the name for type Output --
        # but a placeholder that cannot interpolate becomes the user's column name.
        for item in output_items(items):
            for key in ("varTitle", "varDescription"):
                val = item.get(key)
                if not isinstance(val, str):
                    continue
                for ph in PLACEHOLDER.findall(val):
                    if not INTERPOLABLE.match(ph):
                        literal_titles.setdefault(base, []).append(
                            "%s.%s: {%s} contains an underscore, so it ships literally" % (item.get("name"), key, ph)
                        )

    if not quiet:
        print("checked %d analyses with type:Output result items" % checked)

    ok = True
    if unwired:
        ok = False
        print("\nDEAD Output items -- computed by the backend, never delivered to jamovi:")
        for base in sorted(unwired):
            print("  %-26s %s" % (base, ", ".join(unwired[base])))
        print("\n  Fix: add a `type: Output` option of the SAME name to jamovi/<fn>.a.yaml,")
        print("  a `type: Output` control to <fn>.u.yaml, and gate the backend write on")
        print("  self$results$<name>$isNotFilled(). See jamovi/survival.* for the canonical triple.")
    if literal_titles:
        ok = False
        print("\nvarTitle/varDescription placeholders that cannot interpolate:")
        for base in sorted(literal_titles):
            for msg in literal_titles[base]:
                print("  %-26s %s" % (base, msg))
        print("\n  jmvcore::format's regex excludes underscores. Use a static string and")
        print("  setTitle() at runtime, or rename the option.")

    if ok and not quiet:
        print("all Output result items are wired to an option of the same name")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
