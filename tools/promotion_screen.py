#!/usr/bin/env python3
"""Rank dev/test-routed jamovi analyses by objective release-readiness signals.

331 of this module's 390 analyses sit in a `<Group>D` (dev) or `<Group>T` (test)
menuGroup and therefore never reach a user (see _updateModules.R: the suffix
convention). Deciding which deserve promotion by hand is impractical, so this
scores every one on signals that can be computed without judgement, and leaves
the judgement calls (clinical value, statistical soundness) to a human or a
targeted review of the shortlist.

Signals, all cheap and all falsifiable:
  impl      .b.R line count, and whether .run() actually touches self$data/self$options
  wired     fraction of .r.yaml result items the backend actually writes to
  tests     a tests/testthat/test-<name>.R exists, and how many test_that blocks
  docs      a vignette mentions it
  refs      the analysis cites literature (refs: in .a.yaml/.r.yaml)
  i18n      user-facing strings are wrapped in .()
  debt      TODO/FIXME markers
  churn     commits touching the .b.R (a proxy for how settled it is)

Nothing here proves an analysis is correct. A high score means "worth a human
looking at it"; a low score means "don't bother yet".
"""
import os, re, subprocess, json, sys
from collections import defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
J = os.path.join(ROOT, 'jamovi')
R = os.path.join(ROOT, 'R')


def read(p):
    try:
        return open(p, encoding='utf-8', errors='replace').read()
    except OSError:
        return ''


def menu_group(name):
    m = re.search(r'^menuGroup:\s*(.+?)\s*$', read(os.path.join(J, name + '.a.yaml')), re.M)
    return m.group(1) if m else ''


def result_items(name):
    """Top-level result item names declared in the .r.yaml (uncommented only)."""
    txt = read(os.path.join(J, name + '.r.yaml'))
    return set(re.findall(r'^\s{2,6}-\s*name:\s*(\w+)', txt, re.M))


def git_commits(path):
    try:
        out = subprocess.run(['git', 'log', '--oneline', '--', path], cwd=ROOT,
                             capture_output=True, text=True, timeout=30).stdout
        return len([l for l in out.split('\n') if l.strip()])
    except Exception:
        return 0


def vignette_hits(name):
    try:
        out = subprocess.run(['grep', '-rl', name, os.path.join(ROOT, 'vignettes')],
                             capture_output=True, text=True, timeout=60).stdout
        return len([l for l in out.split('\n') if l.strip()])
    except Exception:
        return 0


def score_one(name, with_git=True):
    b = os.path.join(R, name + '.b.R')
    src = read(b)
    if not src:
        return None
    loc = src.count('\n') + 1

    # does it compute, or is it a scaffold?
    computes = bool(re.search(r'self\$data', src)) and len(re.findall(r'self\$options\$\w+', src)) >= 3

    declared = result_items(name)
    written = set(re.findall(r'self\$results\$(\w+)', src))
    wired = len(declared & written) / len(declared) if declared else 0.0

    tfile = os.path.join(ROOT, 'tests', 'testthat', 'test-%s.R' % name)
    ttxt = read(tfile)
    ntests = len(re.findall(r'test_that\(', ttxt))

    a = read(os.path.join(J, name + '.a.yaml'))
    r = read(os.path.join(J, name + '.r.yaml'))
    refs = len(re.findall(r'^\s*refs:', a + r, re.M))
    i18n = len(re.findall(r'\.\("', src))
    debt = len(re.findall(r'\b(TODO|FIXME)\b', src))
    vign = vignette_hits(name)
    commits = git_commits(b) if with_git else 0

    # deliberately blunt: each signal contributes a small, comparable amount
    score = (
        min(loc / 1000.0, 3.0) * 1.0 +      # substantial implementation
        (2.0 if computes else -4.0) +        # a scaffold is disqualifying
        wired * 3.0 +                        # results actually populated
        min(ntests / 5.0, 2.0) * 1.5 +       # tested
        min(vign, 2) * 0.75 +                # documented
        min(refs / 3.0, 1.0) * 1.0 +         # cites literature
        min(i18n / 50.0, 1.0) * 0.5 +        # translation-ready
        min(commits / 10.0, 1.0) * 0.5 -     # settled
        min(debt / 5.0, 1.0) * 1.0           # open debt
    )
    return dict(name=name, group=menu_group(name), loc=loc, computes=computes,
                wired=round(wired, 2), declared=len(declared), tests=ntests,
                vignettes=vign, refs=refs, i18n=i18n, debt=debt, commits=commits,
                score=round(score, 2))


def main():
    names = sorted(os.path.basename(p)[:-7] for p in
                   [os.path.join(J, f) for f in os.listdir(J)] if p.endswith('.a.yaml'))
    with_git = '--no-git' not in sys.argv
    rows = []
    for n in names:
        s = score_one(n, with_git)
        if s:
            rows.append(s)
    dev = [r for r in rows if re.match(r'^[A-Za-z]+(Extra)?[DT]+$', r['group'])]
    dev.sort(key=lambda r: -r['score'])
    json.dump(dev, open(os.path.join(ROOT, 'tools', 'promotion_scores.json'), 'w'), indent=1)
    print('%-28s %-16s %6s %5s %5s %5s %4s %4s %5s' %
          ('analysis', 'menuGroup', 'score', 'loc', 'wired', 'tests', 'vig', 'ref', 'debt'))
    for r in dev[:40]:
        print('%-28s %-16s %6.2f %5d %5.2f %5d %4d %4d %5d' %
              (r['name'], r['group'], r['score'], r['loc'], r['wired'],
               r['tests'], r['vignettes'], r['refs'], r['debt']))
    print('\n%d dev/test-routed scored; full ranking in tools/promotion_scores.json' % len(dev))
    print('scaffolds (computes=False): %d' % sum(1 for r in dev if not r['computes']))


if __name__ == '__main__':
    main()
