#!/usr/bin/env python3
"""Guard the two reference shapes that crash jamovi result serialization.

Both produce the same, deeply misleading error -- no analysis output at all, and a
traceback that points at RProtoBuf rather than at anything the analysis did:

    Error in h(simpleError(msg, call)) :
      error in evaluating the argument 'object' in selecting a method for
      function 'serialize': argument is of length zero
    ... base::quote(if (grepl("^[0-9]+$", year)) refPB$year <- as.integer(year))

jmvcore builds a protobuf for every reference an analysis cites. If `year` is NULL,
`grepl("^[0-9]+$", NULL)` returns `logical(0)` and `if (logical(0))` throws.

CRASH SHAPE 1 -- a null/absent year in the refs database:

    DecisionCurve:
        year:              # bare key -> NULL -> crash

    `year: ""` is SAFE (grepl returns FALSE and the field is simply skipped).
    Only null/absent crashes. When the real year is unknown, use "" -- never
    invent a date for a citation.

CRASH SHAPE 2 -- `refs:` written as a scalar string instead of a YAML list:

    refs: survival, rpart      # ONE ref named "survival, rpart" -> undefined -> crash

    (Note this is an R-vs-Python trap: in Python that string iterates character by
    character, which makes the scan look far worse than it is; in R it is a single
    length-1 string. The single-item form `refs: survival` happens to resolve, but
    the comma form never does.)

Usage:
    python3 tools/check_refs_years.py [refs.yaml ...]        # shape 1
    python3 tools/check_refs_years.py --citations [module-dir ...]   # both shapes
Exits non-zero if anything would crash.
"""
import glob
import os
import sys

import yaml


def load_refs(path):
    d = yaml.safe_load(open(path))
    refs = d.get('refs', d) if isinstance(d, dict) else d
    if isinstance(refs, list):
        refs = {v.get('name'): v for v in refs if isinstance(v, dict)}
    return refs or {}


def check_years(paths):
    bad = 0
    for p in paths:
        if not os.path.exists(p):
            continue
        for name, v in load_refs(p).items():
            if isinstance(v, dict) and v.get('year') is None:
                print(f"{p}: ref '{name}' has a null/absent year -> serialization will crash")
                bad += 1
    print(f"year check: {len(paths)} file(s), {bad} crash-inducing ref(s)")
    return bad


def check_citations(module_dirs):
    bad = 0
    for d in module_dirs:
        refs_path = os.path.join(d, 'jamovi', '00refs.yaml')
        if not os.path.exists(refs_path):
            continue
        refs = load_refs(refs_path)
        for p in sorted(glob.glob(os.path.join(d, 'jamovi', '*.r.yaml'))):
            try:
                y = yaml.safe_load(open(p))
            except Exception as e:
                print(f"{p}: unparseable ({e})")
                bad += 1
                continue
            if not isinstance(y, dict):
                continue
            cited = y.get('refs')
            if cited is None:
                continue
            if isinstance(cited, str):
                print(f"{p}: `refs:` is a scalar string ({cited!r}); must be a YAML list")
                bad += 1
                continue
            for c in cited:
                if isinstance(c, str) and c not in refs:
                    print(f"{p}: cites undefined ref '{c}' -> year NULL -> will crash")
                    bad += 1
    print(f"citation check: {len(module_dirs)} module(s), {bad} problem(s)")
    return bad


if __name__ == '__main__':
    args = sys.argv[1:]
    if args and args[0] == '--citations':
        targets = args[1:] or ['.']
        sys.exit(1 if check_citations(targets) else 0)
    sys.exit(1 if check_years(args or ['jamovi/00refs.yaml']) else 0)
