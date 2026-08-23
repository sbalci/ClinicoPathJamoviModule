#!/usr/bin/env python3
"""Guard against duplicate control `name:` values inside one .u.yaml.

Since jamovi 2.8 the options panel registers every control under its `name`
and throws on a repeat:

    The following resource id is already in use: showClinicalImpactPlot

The symptom is NOT an error message: the options panel stays stuck on its grey
loading skeleton forever (the title renders, nothing else does). Older jamovi
tolerated the duplicates, so files that worked for years break silently on
upgrade. decisioncurve (5 duplicated checkboxes) and hematologicindices were
hit on 2026-08-23.

Two legitimate patterns that LOOK like duplicates and how to write them:

  * The same option shown in two sections -> it cannot be. Keep one control,
    in the section that holds its dependents (`enable: (thatOption)`).
  * One NMXList option spread over several CheckBoxes -> each box needs a
    UNIQUE `name`, plus `optionName:` pointing at the option and `optionPart:`
    naming the level:

        - type: CheckBox
          name: indices_nlr
          optionName: indices
          optionPart: nlr

Usage:
    python3 tools/check_uyaml_duplicate_names.py          # scan jamovi/*.u.yaml
    python3 tools/check_uyaml_duplicate_names.py a.u.yaml # scan given files
Exit status 1 when any duplicate is found.
"""
import glob
import sys
from collections import Counter

import yaml


def control_names(node, out):
    if isinstance(node, dict):
        if "name" in node and "type" in node:
            out.append(str(node["name"]))
        for child in node.get("children", []) or []:
            control_names(child, out)
    elif isinstance(node, list):
        for child in node:
            control_names(child, out)
    return out


def main(paths):
    bad = 0
    for path in paths:
        try:
            doc = yaml.safe_load(open(path, encoding="utf-8"))
        except yaml.YAMLError as exc:  # a broken file is somebody else's gate
            print(f"{path}: YAML parse error ({exc})")
            bad += 1
            continue
        dupes = [n for n, c in Counter(control_names(doc, [])).items() if c > 1]
        if dupes:
            bad += 1
            print(f"{path}: duplicate control name(s): {', '.join(sorted(dupes))}")
    if bad:
        print(f"\n{bad} file(s) with duplicate control names -- jamovi >= 2.8 will "
              "leave their options panel stuck on the loading skeleton.")
        return 1
    print(f"OK: no duplicate control names in {len(paths)} .u.yaml file(s)")
    return 0


if __name__ == "__main__":
    files = sys.argv[1:] or sorted(glob.glob("jamovi/*.u.yaml"))
    sys.exit(main(files))
