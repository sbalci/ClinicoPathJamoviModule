#!/usr/bin/env python3
"""Release-readiness gate for the jamovi module.

Every check corresponds to something the jamovi library reviewer verifies (see
vignettes/jamovi_library_review_guide.md section 1). Exit status is non-zero if
any BLOCKING check fails.
"""
import os, re, sys, glob, subprocess
import yaml

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.chdir(ROOT)
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
        for m in re.finditer(r'^\s*refs:\s*$\n((?:\s*-\s*\w+\s*\n)+)', txt, re.M):
            used |= set(re.findall(r'-\s*(\w+)', m.group(1)))
        for m in re.finditer(r'^\s*refs:\s*\[([^\]]*)\]', txt, re.M):
            used |= set(x.strip() for x in m.group(1).split(',') if x.strip())
    dangling = sorted(used - defined)
    if dangling:
        FAIL.append('refs: keys with no entry in 00refs.yaml: %s' % ', '.join(dangling[:12]))
    body = refs.get('refs', refs) if isinstance(refs, dict) else {}
    nourl = [k for k, v in body.items() if isinstance(v, dict) and not v.get('url')]
    if nourl:
        WARN.append('%d citation entries still have no url: %s' % (len(nourl), ', '.join(sorted(nourl)[:10])))
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
                    % (len(bad), '; '.join(sorted(set(bad))[:10])))
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
                    % (len(bad), '; '.join(bad[:10])))
    print('  renderFun: %d unresolved' % len(bad))


def check_artifacts():
    arts = [p for p in glob.glob('*.tar.gz') + glob.glob('*.jmo')]
    tracked = subprocess.run(['git', 'ls-files'], capture_output=True, text=True).stdout.split('\n')
    arts += [t for t in tracked if t.endswith(('.tar.gz', '.jmo'))]
    if arts:
        FAIL.append('committed build artifacts: %s' % ', '.join(sorted(set(arts))[:6]))
    print('  build artifacts committed: %d' % len(set(arts)))


def check_tame():
    missing = [os.path.basename(p) for p in glob.glob('jamovi/*.u.yaml')
               if 'compilerMode: tame' not in open(p, encoding='utf-8').read()]
    if missing:
        WARN.append('%d .u.yaml files without compilerMode: tame (e.g. %s)'
                    % (len(missing), ', '.join(missing[:5])))
    print('  compilerMode: tame missing on %d of %d .u.yaml' % (len(missing), len(glob.glob('jamovi/*.u.yaml'))))


def check_visible_bang():
    hits = []
    for p in glob.glob('jamovi/*.r.yaml'):
        for i, l in enumerate(open(p, encoding='utf-8'), 1):
            if re.match(r'^\s*visible:\s*\(\s*!', l):
                hits.append('%s:%d' % (os.path.basename(p), i))
    if hits:
        WARN.append('%d `visible: (!x)` expressions - jmvcore cannot route these, so the item is '
                    'ALWAYS VISIBLE: %s' % (len(hits), ', '.join(hits[:8])))
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
        FAIL.append('%d non-structural HTML entities: %s' % (len(hits), ', '.join(hits[:8])))
    print('  non-structural HTML entities: %d' % len(hits))


if __name__ == '__main__':
    print('RELEASE GATE\n')
    for fn in (check_versions, check_license, check_refs, check_clearwith, check_renderfun,
               check_artifacts, check_tame, check_visible_bang, check_entities):
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
