#!/usr/bin/env python3
"""Every ref must have a non-null `year`.

A bare `year:` (or an absent year key) reads as NULL in R, so jmvcore's
    if (grepl("^[0-9]+$", year)) refPB$year <- as.integer(year)
receives logical(0) and dies with "argument is of length zero" while serializing
results -- taking down every analysis that cites that reference.
`year: ""` is safe (grepl returns FALSE); only null/absent crashes.
"""
import sys, os, yaml
roots = sys.argv[1:] or ['jamovi/00refs.yaml']
bad = 0
for p in roots:
    if not os.path.exists(p):
        continue
    d = yaml.safe_load(open(p))
    refs = d.get('refs', d) if isinstance(d, dict) else d
    if isinstance(refs, list):
        refs = {v.get('name'): v for v in refs if isinstance(v, dict)}
    for name, v in (refs or {}).items():
        if isinstance(v, dict) and v.get('year') is None:
            print(f"{p}: ref '{name}' has a null/absent year -> will crash serialization")
            bad += 1
print(f"checked {len(roots)} file(s), {bad} crash-inducing ref(s)")
sys.exit(1 if bad else 0)
