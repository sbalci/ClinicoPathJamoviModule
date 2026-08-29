#!/usr/bin/env python3
"""Remove analyses from jamovi/0000.yaml that have no .a.yaml source file.

WHY THIS EXISTS
---------------
jamovi/0000.yaml looks generated, but the jamovi-compiler (0.3.5, index.js)
READS the existing 0000.yaml into `packageInfo`, then for each .a.yaml either
Object.assign()s over the matching entry or pushes a new one. There is no
removal pass anywhere in the compiler. So jmvtools::prepare() can only ADD and
UPDATE -- an entry whose .a.yaml was deleted survives every prepare() forever,
and keeps exporting its stale title/description into jamovi/i18n/*.po.

Deleting the entry here is therefore the ONLY mechanism, and it is stable:
prepare() will not re-add an analysis that has no .a.yaml.

Usage:
    python3 tools/prune_stale_analyses.py          # dry run, lists what would go
    python3 tools/prune_stale_analyses.py --apply  # rewrite 0000.yaml
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INDEX = os.path.join(ROOT, "jamovi", "0000.yaml")


def find_stale(lines):
    """Return (stale_names, spans) where spans are (start, end) line indices."""
    try:
        a_start = next(i for i, l in enumerate(lines) if l.rstrip("\n") == "analyses:")
    except StopIteration:
        sys.exit("no 'analyses:' key found in %s" % INDEX)

    # Item lines look like "  - title: ...". Anything at column 0 ends the list.
    item_re = re.compile(r"^(\s+)-\s")
    spans, starts = [], []
    end_of_list = len(lines)
    for i in range(a_start + 1, len(lines)):
        line = lines[i]
        if line.strip() and not line.startswith((" ", "\t")):
            end_of_list = i
            break
        if item_re.match(line):
            starts.append(i)
    for n, s in enumerate(starts):
        e = starts[n + 1] if n + 1 < len(starts) else end_of_list
        spans.append((s, e))

    stale, drop = [], []
    for s, e in spans:
        block = "".join(lines[s:e])
        m = re.search(r"^\s*(?:-\s+)?name:\s*(\S+)\s*$", block, re.M)
        if not m:
            continue
        name = m.group(1).strip("'\"")
        if not os.path.exists(os.path.join(ROOT, "jamovi", name + ".a.yaml")):
            stale.append(name)
            drop.append((s, e))
    return stale, drop



def report_anomalies():
    """Report the OTHER damage the same accumulator bug causes. Detection only.

    The compiler dedups with `for (existing of packageInfo.analyses) { if
    (existing.name === analysis.name) { Object.assign(existing, aObj); break } }`.
    Two consequences:
      1. It breaks on the FIRST match, so a pre-existing duplicate entry is never
         updated and never removed -- it keeps whatever menuGroup/title it had.
      2. Object.assign MERGES, so a key removed from the .a.yaml (e.g. dropping
         menuSubgroup) is never cleared from the existing entry.
    """
    import yaml
    doc = yaml.safe_load(open(INDEX, encoding="utf-8"))
    analyses = doc.get("analyses", [])

    seen, dups = {}, []
    for a in analyses:
        seen.setdefault(a["name"], []).append(a)
    for name, entries in sorted(seen.items()):
        if len(entries) > 1:
            dups.append((name, entries))

    if dups:
        print("\nDUPLICATE registrations (%d) -- compiler updates only the first," % len(dups))
        print("so every later copy is a permanently stale menu entry:")
        for name, entries in dups:
            print("  %s x%d" % (name, len(entries)))
            for i, e in enumerate(entries):
                tag = "kept/updated" if i == 0 else "STALE ZOMBIE"
                print("      [%s] menuGroup=%s menuSubgroup=%s title=%s"
                      % (tag, e.get("menuGroup"), e.get("menuSubgroup"), e.get("title")))

    phantom = []
    for a in analyses:
        src = os.path.join(ROOT, "jamovi", a["name"] + ".a.yaml")
        if not os.path.exists(src):
            continue
        try:
            sa = yaml.safe_load(open(src, encoding="utf-8")) or {}
        except Exception:
            continue
        for key in ("menuSubgroup", "menuSubtitle"):
            if key in a and key not in sa:
                phantom.append((a["name"], key, a[key]))

    if phantom:
        print("\nPHANTOM keys (%d) -- present in 0000.yaml, absent from the .a.yaml." % len(phantom))
        print("Object.assign never clears a removed key:")
        for name, key, val in phantom:
            print("  %-24s %s: %r" % (name, key, val))

    if not dups and not phantom:
        print("\nno duplicate or phantom-key anomalies found.")


def main():
    apply = "--apply" in sys.argv
    with open(INDEX, encoding="utf-8") as fh:
        lines = fh.readlines()

    stale, drop = find_stale(lines)
    if not stale:
        print("0000.yaml is clean: every registered analysis has a .a.yaml.")
        report_anomalies()
        return 0

    print("Stale analyses (registered in 0000.yaml, no jamovi/<name>.a.yaml):")
    for n in stale:
        print("  -", n)
    print("total: %d" % len(stale))

    report_anomalies()

    if not apply:
        print("\ndry run -- re-run with --apply to remove them.")
        return 1

    keep = set(range(len(lines)))
    for s, e in drop:
        keep -= set(range(s, e))
    out = [l for i, l in enumerate(lines) if i in keep]
    with open(INDEX, "w", encoding="utf-8") as fh:
        fh.writelines(out)
    print("\nremoved %d entries from jamovi/0000.yaml" % len(stale))
    print("next: regenerate catalogs, then commit 0000.yaml + jamovi/i18n/*")
    return 0


if __name__ == "__main__":
    sys.exit(main())
