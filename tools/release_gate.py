#!/usr/bin/env python3
"""Release-readiness gate for the jamovi module.

Every check corresponds to something the jamovi library reviewer verifies (see
vignettes/jamovi_library_review_guide.md section 1). Exit status is non-zero if
any BLOCKING check fails.
"""
import os, re, sys, glob, subprocess
import yaml

# --root <dir> runs every check against another module tree, e.g. a generated submodule
# (the library-audit skill verifies a fix in the sibling repo the reviewer audits).
ROOT = (os.path.abspath(sys.argv[sys.argv.index('--root') + 1]) if '--root' in sys.argv
        else os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
os.chdir(ROOT)
# with --root every list is printed in full, so it can be compared name by name with a report
FULL = '--root' in sys.argv


def cap(xs, n=10):
    return xs if FULL else xs[:n]


FAIL, WARN = [], []


def load(p):
    try:
        return yaml.safe_load(open(p, encoding='utf-8'))
    except Exception as e:
        FAIL.append('YAML will not parse: %s (%s)' % (p, e))
        return None


def check_versions():
    desc = open('DESCRIPTION', encoding='utf-8').read()
    dv = re.search(r'^Version:\s*(\S+)', desc, re.M).group(1)
    zz = load('jamovi/0000.yaml') or {}
    zv = str(zz.get('version', ''))
    cite = open('CITATION.cff', encoding='utf-8').read() if os.path.exists('CITATION.cff') else ''
    cv = (re.search(r'^version:\s*"?([^"\n]+)"?', cite, re.M) or [None, ''])[1].strip()
    if dv != zv:
        FAIL.append('version mismatch: DESCRIPTION %s vs 0000.yaml %s' % (dv, zv))
    if cv and cv != dv:
        FAIL.append('version mismatch: DESCRIPTION %s vs CITATION.cff %s' % (dv, cv))
    if dv.startswith('0.'):
        FAIL.append('version %s is still pre-1.0' % dv)
    print('  version %s consistent across DESCRIPTION / 0000.yaml / CITATION.cff' % dv)


def check_license():
    lic = re.search(r'^License:\s*(.+)$', open('DESCRIPTION', encoding='utf-8').read(), re.M).group(1).strip()
    if 'GPL' not in lic:
        WARN.append('License is %r - the reviewer expects an OSI-approved licence' % lic)
    print('  license: %s' % lic)


def check_refs():
    refs = load('jamovi/00refs.yaml') or {}
    defined = set(refs.get('refs', refs).keys()) if isinstance(refs, dict) else set()
    used = set()
    for p in glob.glob('jamovi/*.a.yaml') + glob.glob('jamovi/*.r.yaml'):
        txt = open(p, encoding='utf-8').read()
        # comment lines may sit inside the list; they used to end the match and hide every later key
        for m in re.finditer(r'^\s*refs:\s*$\n((?:\s*(?:-\s*\w+|#[^\n]*)\s*\n)+)', txt, re.M):
            used |= set(re.findall(r'^\s*-\s*(\w+)', m.group(1), re.M))
        for m in re.finditer(r'^\s*refs:\s*\[([^\]]*)\]', txt, re.M):
            used |= set(x.strip() for x in m.group(1).split(',') if x.strip())
    dangling = sorted(used - defined)
    if dangling:
        FAIL.append('refs: keys with no entry in 00refs.yaml: %s' % ', '.join(cap(dangling, 12)))
    body = refs.get('refs', refs) if isinstance(refs, dict) else {}
    nourl = [k for k, v in body.items() if isinstance(v, dict) and not v.get('url')]
    if nourl:
        WARN.append('%d citation entries still have no url: %s' % (len(nourl), ', '.join(cap(sorted(nourl)))))
    print('  citations: %d defined, %d cited, %d dangling' % (len(defined), len(used), len(dangling)))


def _walk(node, out):
    """Collect every clearWith list anywhere in a results tree."""
    if isinstance(node, dict):
        cw = node.get('clearWith')
        if isinstance(cw, list):
            out.extend(x for x in cw if isinstance(x, str))
        for v in node.values():
            _walk(v, out)
    elif isinstance(node, list):
        for v in node:
            _walk(v, out)


def check_clearwith():
    """Walk the PARSED yaml. A regex here previously ran on past the clearWith
    block and swallowed the following `refs:` list, reporting package names like
    `ggplot2` and `rms` as dangling options."""
    bad = []
    for p in glob.glob('jamovi/*.r.yaml'):
        name = os.path.basename(p)[:-7]
        a = load('jamovi/%s.a.yaml' % name)
        r = load(p)
        if not a or not r:
            continue
        opts = {o['name'] for o in (a.get('options') or []) if isinstance(o, dict) and 'name' in o}
        entries = []
        _walk(r, entries)
        for e in entries:
            base = e.split(':')[0].strip()
            if base and base not in opts:
                bad.append('%s :: %s' % (name, e))
    if bad:
        FAIL.append('%d clearWith entries do not resolve to an option: %s'
                    % (len(bad), '; '.join(cap(sorted(set(bad))))))
    print('  clearWith: %d dangling entries' % len(bad))


def check_renderfun():
    """A renderFun may be defined in the analysis .b.R or in any shared R file,
    so search the whole R/ tree before calling one unresolved."""
    alltext = ''
    for f in glob.glob('R/*.R'):
        alltext += open(f, encoding='utf-8', errors='replace').read()
    defined = set(re.findall(r'(\.[A-Za-z_][\w.]*)\s*=\s*function', alltext))
    bad = []
    for p in glob.glob('jamovi/*.r.yaml'):
        name = os.path.basename(p)[:-7]
        for fn in re.findall(r'^\s*renderFun:\s*(\.[\w.]+)', open(p, encoding='utf-8').read(), re.M):
            if fn not in defined:
                bad.append('%s :: %s' % (name, fn))
    if bad:
        FAIL.append('%d renderFun values resolve to no method anywhere in R/: %s'
                    % (len(bad), '; '.join(cap(bad))))
    print('  renderFun: %d unresolved' % len(bad))


def check_artifacts():
    # tracked + untracked-but-not-ignored: a gitignored local .jmo/.tar.gz is not shipped
    files = subprocess.run(['git', 'ls-files', '-co', '--exclude-standard'],
                           capture_output=True, text=True).stdout.split('\n')
    arts = [f for f in files if f.endswith(('.tar.gz', '.jmo'))]
    if arts:
        FAIL.append('committed build artifacts: %s' % ', '.join(cap(sorted(set(arts)), 6)))
    print('  build artifacts committed: %d' % len(set(arts)))


def check_tame():
    missing = [os.path.basename(p) for p in glob.glob('jamovi/*.u.yaml')
               if 'compilerMode: tame' not in open(p, encoding='utf-8').read()]
    if missing:
        WARN.append('%d .u.yaml files without compilerMode: tame (e.g. %s)'
                    % (len(missing), ', '.join(cap(missing, 5))))
    print('  compilerMode: tame missing on %d of %d .u.yaml' % (len(missing), len(glob.glob('jamovi/*.u.yaml'))))


def check_visible_bang():
    hits = []
    for p in glob.glob('jamovi/*.r.yaml'):
        for i, l in enumerate(open(p, encoding='utf-8'), 1):
            if re.match(r'^\s*visible:\s*\(\s*!', l):
                hits.append('%s:%d' % (os.path.basename(p), i))
    if hits:
        WARN.append('%d `visible: (!x)` expressions - jmvcore cannot route these, so the item is '
                    'ALWAYS VISIBLE: %s' % (len(hits), ', '.join(cap(hits, 8))))
    print('  visible: (!x) silent-always-visible: %d' % len(hits))


def check_entities():
    structural = {'&lt;', '&gt;', '&amp;', '&quot;', '&apos;'}
    hits = []
    for p in glob.glob('R/*.R'):
        for i, l in enumerate(open(p, encoding='utf-8', errors='replace'), 1):
            if re.search(r'gsub\(\s*["\']&[a-zA-Z]+;', l):
                continue
            for e in set(re.findall(r'&[a-zA-Z][a-zA-Z0-9]{1,12};', l)) - structural:
                hits.append('%s:%d %s' % (os.path.basename(p), i, e))
    if hits:
        FAIL.append('%d non-structural HTML entities: %s' % (len(hits), ', '.join(cap(hits, 8))))
    print('  non-structural HTML entities: %d' % len(hits))


def _dicts(node):
    if isinstance(node, dict):
        yield node
        for v in node.values():
            yield from _dicts(v)
    elif isinstance(node, list):
        for v in node:
            yield from _dicts(v)


def _safe_yaml(p):
    try:
        return yaml.safe_load(open(p, encoding='utf-8'))
    except Exception:
        return None


def _shipped(name):
    """Production menuGroups carry no D (development) or T (JamoviTest) suffix."""
    a = _safe_yaml('jamovi/%s.a.yaml' % name) or {}
    return not re.search(r'[DT]$', str(a.get('menuGroup', 'D')))


_METHOD = re.compile(r'^\s*(\.[A-Za-z_][\w.]*)\s*=\s*function\s*\(', re.M)
_DATA = re.compile(r'self\$data\b|self\$readDataset|private\$\.data\b')
_CALL = re.compile(r'private\$(\.[A-Za-z_][\w.]*)\s*\(')


def check_requires_data():
    """jmvcore nulls private$.data once .run() returns and re-reads the dataset for a
    redraw or export only when the Image declares requiresData: true (library review
    guide section 15). Trace each renderFun through the private$ helpers it calls.
    ponytail: method bodies are split at method headers with comments stripped - a
    heuristic, but it reproduces the 2026-09-15 jsurvival report exactly."""
    missing, surplus = [], []
    for p in glob.glob('jamovi/*.r.yaml'):
        name = os.path.basename(p)[:-7]
        b = 'R/%s.b.R' % name
        r = _safe_yaml(p)
        if not os.path.exists(b) or not r:
            continue
        src = re.sub(r'#[^\n]*', '', open(b, encoding='utf-8', errors='replace').read())
        heads = list(_METHOD.finditer(src))
        body = {h.group(1): src[h.end(): heads[i + 1].start() if i + 1 < len(heads) else len(src)]
                for i, h in enumerate(heads)}
        for d in _dicts(r):
            if d.get('type') != 'Image' or not d.get('renderFun'):
                continue
            seen, todo, touches = set(), [d['renderFun']], False
            while todo and not touches:
                f = todo.pop()
                if f in seen or f not in body:
                    continue
                seen.add(f)
                touches = bool(_DATA.search(body[f]))
                todo.extend(_CALL.findall(body[f]))
            if touches != (d.get('requiresData') is True):
                (missing if touches else surplus).append((name, '%s:%s' % (name, d.get('name'))))
    ship_missing = sorted(x for n, x in missing if _shipped(n))
    ship_surplus = sorted(x for n, x in surplus if _shipped(n))
    if ship_missing:
        FAIL.append('%d shipped Image(s) reach self$data without requiresData: true - the plot '
                    'errors on resize / .omv reopen / export: %s' % (len(ship_missing), ', '.join(ship_missing)))
    if ship_surplus:
        WARN.append('%d shipped Image(s) declare requiresData: true but draw only from image$state '
                    '(dataset re-read for nothing): %s' % (len(ship_surplus), ', '.join(cap(ship_surplus))))
    print('  requiresData: %d missing (%d shipped), %d surplus (%d shipped)'
          % (len(missing), len(ship_missing), len(surplus), len(ship_surplus)))


_SMALL_WORDS = {'a', 'an', 'and', 'as', 'at', 'by', 'for', 'from', 'in', 'of', 'on', 'or',
                'per', 'the', 'to', 'via', 'vs', 'with'}


def check_collapsebox_titlecase():
    """Group headings are Title Case; individual controls are sentence case (section 12)."""
    bad = []
    for p in glob.glob('jamovi/*.u.yaml'):
        name = os.path.basename(p)[:-7]
        for d in _dicts(_safe_yaml(p)):
            label = d.get('label')
            if d.get('type') != 'CollapseBox' or not isinstance(label, str):
                continue
            # parenthesised package names - "(ggpubr)", "(visdat)" - keep their own case
            words = re.findall(r"[A-Za-z][A-Za-z'-]*", re.sub(r'\([^)]*\)', '', label))
            if any(w[0].islower() and (i == 0 or w.lower() not in _SMALL_WORDS)
                   for i, w in enumerate(words)):
                bad.append((name, '%s: %s' % (name, label)))
    ship = sorted(x for n, x in bad if _shipped(n))
    if ship:
        WARN.append('%d shipped CollapseBox headings not in Title Case: %s' % (len(ship), '; '.join(cap(ship))))
    print('  CollapseBox Title Case: %d off-convention (%d shipped)' % (len(bad), len(ship)))


def check_i18n_padding():
    """A separator inside .() - leading space , ; . or a trailing space - is load-bearing
    and invisible to translators (section 9). The older checklist grep also matched every
    `collapse = ", "` and buried the real sites."""
    lead = re.compile(r'\.\(\s*"(?:[\s,;:]|\.(?!\.\.))')   # a leading "..." ellipsis is fine
    trail = re.compile(r'\.\(\s*"[^"\n]*\s"\s*[,)]')
    hits = []
    for p in glob.glob('R/*.b.R'):
        name = os.path.basename(p)[:-4]
        for i, l in enumerate(open(p, encoding='utf-8', errors='replace'), 1):
            if l.lstrip().startswith('#'):
                continue
            if lead.search(l) or trail.search(l):
                hits.append((name, '%s:%d' % (os.path.basename(p), i)))
    ship = [x for n, x in hits if _shipped(n)]
    if ship:
        WARN.append('%d separator/padding sites inside .() in shipped analyses: %s'
                    % (len(ship), ', '.join(cap(ship))))
    print('  .() separator/padding: %d sites (%d shipped)' % (len(hits), len(ship)))


if __name__ == '__main__':
    print('RELEASE GATE  %s\n' % ROOT)
    for fn in (check_versions, check_license, check_refs, check_clearwith, check_renderfun,
               check_artifacts, check_tame, check_visible_bang, check_entities,
               check_requires_data, check_collapsebox_titlecase, check_i18n_padding):
        try:
            fn()
        except Exception as e:
            FAIL.append('%s crashed: %s' % (fn.__name__, e))
    print()
    for w in WARN:
        print('  WARN  %s' % w)
    for f in FAIL:
        print('  FAIL  %s' % f)
    print('\n%d blocking, %d advisory' % (len(FAIL), len(WARN)))
    sys.exit(1 if FAIL else 0)
