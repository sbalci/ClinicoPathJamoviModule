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


def check_news():
    """NEWS.md needs a section for the version DESCRIPTION declares - the one the reviewer reads
    (2026-09-16 OncoPath [LOW]). _updateModules.R rewrites Version: on every regeneration and never
    writes NEWS.md (release notes need a person), so a missing entry only surfaces here."""
    if not os.path.exists('NEWS.md'):
        return
    dv = re.search(r'^Version:\s*(\S+)', open('DESCRIPTION', encoding='utf-8').read(), re.M).group(1)
    heads = re.findall(r'^#{1,2} +(.*)$', open('NEWS.md', encoding='utf-8').read(), re.M)
    covered = any(re.search(r'(?<![\w.])%s(?![\w.])' % re.escape(dv), h) for h in heads)
    if not covered:
        WARN.append('NEWS.md has no heading for DESCRIPTION version %s (newest heading: %s)'
                    % (dv, next((h for h in heads if re.search(r'\d+\.\d+', h)), 'none')))
    print('  NEWS.md: DESCRIPTION %s %s' % (dv, 'has a heading' if covered else 'has NO heading'))


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


def _images(node, name=None):
    """(Image dict, display name) for every Image in a results tree; an Array template is named
    after its Array."""
    if isinstance(node, dict):
        name = node.get('name') or name
        if node.get('type') == 'Image':
            yield node, name
        for v in node.values():
            yield from _images(v, name)
    elif isinstance(node, list):
        for v in node:
            yield from _images(v, name)


def _safe_yaml(p):
    try:
        return yaml.safe_load(open(p, encoding='utf-8'))
    except Exception:
        return None


def _shipped(name):
    """Production menuGroups carry no D (draft), P (pending) or T (JamoviTest) suffix."""
    a = _safe_yaml('jamovi/%s.a.yaml' % name) or {}
    return not re.search(r'[DPT]$', str(a.get('menuGroup', 'D')))


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
        for d, iname in _images(r):
            if not d.get('renderFun'):
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
                (missing if touches else surplus).append((name, '%s:%s' % (name, iname)))
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


_ASSIGN_PRIVATE = re.compile(r'private\$(\.[A-Za-z_][\w.]*)\s*(?:\[\[[^\]]*\]\]|\$\w+)?\s*(?:<-|=(?!=))')
_READ_PRIVATE = re.compile(r'private\$(\.[A-Za-z_][\w.]*)\b(?!\s*\()')


def check_render_private_state():
    """Export and .omv reopen rebuild the analysis and call the renderer WITHOUT .run(), so a
    private$ field only .run() fills is NULL there (2026-09-16 meddecide [HIGH]: psychopdaROC
    forest plot exported blank; [MEDIUM]: ROC overlays vanished; jjstatsplot [HIGH]). Traces each
    renderFun through its private$ helpers. Not flagged: fields whose only writer is .init(), private[["..."]] <- restore helpers,
    fields null-checked on a path that reads state (or, on a requiresData: true image, any null-checked
    cache - a miss recomputes from self$data), and fields named in a `# render-state: <field>`
    comment inside the traced code (a traced, deliberate fallback). ponytail: text heuristic; prove a
    hit on the engine path (.createImage on a fresh analysis), guide section 15."""
    hits = []
    for p in glob.glob('jamovi/*.r.yaml'):
        name = os.path.basename(p)[:-7]
        b = 'R/%s.b.R' % name
        r = _safe_yaml(p)
        if not os.path.exists(b) or not r:
            continue
        raw = open(b, encoding='utf-8', errors='replace').read()
        src = re.sub(r'#[^\n]*', '', raw)
        heads = list(_METHOD.finditer(src))
        body = {h.group(1): src[h.end(): heads[i + 1].start() if i + 1 < len(heads) else len(src)]
                for i, h in enumerate(heads)}
        rheads = list(_METHOD.finditer(raw))
        raw_body = {h.group(1): raw[h.end(): rheads[i + 1].start() if i + 1 < len(rheads) else len(raw)]
                    for i, h in enumerate(rheads)}
        init_set = set(_ASSIGN_PRIVATE.findall(body.get('.init', '')))
        for d, iname in _images(r):
            if not d.get('renderFun'):
                continue
            seen, todo = [], [d['renderFun']]
            while todo:
                f = todo.pop()
                if f in seen or f not in body:
                    continue
                seen.append(f)
                todo.extend(_CALL.findall(body[f]))
            traced = ''.join(body[f] for f in seen)
            others = ''.join(v for k, v in body.items() if k not in seen and k != '.init')
            run_set = set(_ASSIGN_PRIVATE.findall(others))
            ok = set(_ASSIGN_PRIVATE.findall(traced)) | (init_set - run_set)   # .init() the only writer
            if re.search(r'private\[\[', traced):
                ok |= set(re.findall(r'["\'](\.[A-Za-z_][\w.]*)["\']', traced))
            reads = set(_READ_PRIVATE.findall(traced)) - set(body)
            if d.get('requiresData') is True or re.search(r'image\$state|\bstate\$|\bst\$', traced):
                ok |= {f for f in reads if re.search(r'is\.null\(\s*private\$' + re.escape(f) + r'\b', traced)}
            ok |= set(re.findall(r'#\s*render-state:\s*(\.[A-Za-z_][\w.]*)', ''.join(raw_body.get(f, '') for f in seen)))
            bad = sorted((reads & run_set) - ok)
            if bad:
                hits.append((name, '%s:%s (%s)' % (name, iname, ', '.join(bad))))
    ship = sorted(x for n, x in hits if _shipped(n))
    if ship:
        WARN.append('%d shipped Image(s) draw from private$ fields only .run() fills - blank or incomplete on '
                    'export / .omv reopen: %s' % (len(ship), '; '.join(cap(ship))))
    print('  renderer private$ state: %d images (%d shipped)' % (len(hits), len(ship)))


# enclosing scope = an R6 method (`.name = function`) or a top-level function; inline handlers such as
# `error = function(e)` stay inside their method
_FUNC_HEAD = re.compile(r'^(?:\s*(\.[A-Za-z_][\w.]*)\s*=|([A-Za-z.][\w.]*)\s*<-)\s*function\s*\(', re.M)
_STRING = re.compile(r'"(?:[^"\\\n]|\\.)*"|\'(?:[^\'\\\n]|\\.)*\'')


def check_bare_set_seed():
    """Every analysis in a jamovi session runs in one R process, so a bare set.seed() leaves a fixed
    random stream for whatever runs next (2026-09-16 meddecide [LOW]). Seed with
    withr::local_seed() / with_seed(); a function that saves and restores .Random.seed, or calls
    withr::local_preserve_seed(), is not flagged. Strings (generated R code) and comments are ignored."""
    hits = []
    for p in sorted(glob.glob('R/*.R')):
        if p.endswith('.h.R'):
            continue
        code, text = [], []          # code: strings blanked (same length); text: strings kept
        for line in open(p, encoding='utf-8', errors='replace'):
            masked = _STRING.sub(lambda m: m.group(0)[0] + ' ' * (len(m.group(0)) - 2) + m.group(0)[-1], line)
            cut = masked.find('#')
            code.append(masked if cut < 0 else masked[:cut] + '\n')
            text.append(line if cut < 0 else line[:cut] + '\n')
        code, text = ''.join(code), ''.join(text)
        heads = list(_FUNC_HEAD.finditer(code))
        for i, h in enumerate(heads):
            end = heads[i + 1].start() if i + 1 < len(heads) else len(code)
            if re.search(r'(?<![\w.$:])set\.seed\s*\(', code[h.end(): end]) and \
                    not re.search(r'\.Random\.seed|preserve_seed', text[h.end(): end]):
                hits.append((p, '%s::%s' % (os.path.basename(p), h.group(1) or h.group(2))))
    ship = [x for p, x in hits if _file_shipped(p)]
    if ship:
        WARN.append('%d shipped functions call set.seed() without restoring the RNG state (leaks into the '
                    'shared engine process; use withr::local_seed): %s' % (len(ship), ', '.join(cap(ship))))
    print('  bare set.seed(): %d functions (%d shipped)' % (len(hits), len(ship)))


_ATTACHED = {'base', 'stats', 'utils', 'graphics', 'grDevices', 'methods', 'datasets'}


def check_unused_imports():
    """A package in DESCRIPTION Imports that no R/ code uses still installs, with its whole dependency
    tree, for every user (2026-09-16 meddecide [LOW]: ggraph, igraph, glue, Matrix, htmlTable). Used =
    `pkg::`, a NAMESPACE importFrom() symbol that appears in the code, import(pkg), or a
    requireNamespace/library/loadNamespace of it. Comments never count; strings count for `pkg::`
    (formula text) but not for symbols (a `refs:` name is not a use - htmlTable). R/00jmv.R is skipped.
    The always-attached base packages are skipped: their bare calls cannot be told apart."""
    desc = open('DESCRIPTION', encoding='utf-8').read()
    m = re.search(r'^Imports:(.*?)(?=^\S)', desc + '\nEnd:', re.M | re.S)
    imports = [re.sub(r'\(.*', '', x).strip() for x in (m.group(1).split(',') if m else [])]
    imports = [x for x in imports if x and x not in _ATTACHED]
    ns = open('NAMESPACE', encoding='utf-8').read() if os.path.exists('NAMESPACE') else ''
    code, text = [], []          # code: strings blanked; text: strings kept
    for p in sorted(glob.glob('R/*.R')):
        if os.path.basename(p) == '00jmv.R':
            continue
        for line in open(p, encoding='utf-8', errors='replace'):
            masked = _STRING.sub(lambda s: s.group(0)[0] + ' ' * (len(s.group(0)) - 2) + s.group(0)[-1], line)
            cut = masked.find('#')
            code.append(masked if cut < 0 else masked[:cut] + '\n')
            text.append(line if cut < 0 else line[:cut] + '\n')
    code, text = ''.join(code), ''.join(text)
    unused = []
    for pkg in imports:
        q = re.escape(pkg)
        if re.search(r'(?<![\w.])%s:::?' % q, text) or \
                re.search(r'^import\(\s*"?%s"?\s*[,)]' % q, ns, re.M) or \
                re.search(r'(?:requireNamespace|loadNamespace|library|require)\s*\(\s*["\']?%s["\')\s,]' % q, text):
            continue
        syms = [s.strip('"\'` ') for s in re.findall(r'^importFrom\(\s*"?%s"?\s*,\s*(.+?)\)\s*$' % q, ns, re.M)]
        if any(re.search(r'(?<![\w.])%s(?![\w.])' % re.escape(s), code) if re.match(r'^[\w.]+$', s)
               else s in code for s in syms):
            continue
        unused.append(pkg)
    if unused:
        WARN.append('%d DESCRIPTION Imports used by no R/ code (every install pulls them in; drop them from '
                    'Imports and their tags from R/zzz_imports.R, prune_imports in the umbrella registry): %s'
                    % (len(unused), ', '.join(cap(unused))))
    print('  Imports: %d checked, %d unused' % (len(imports), len(unused)))


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


_UNESC = re.compile(r'\\(["\'\\nrtbf])')
_UNESC_MAP = {'n': '\n', 'r': '\r', 't': '\t', 'b': '\b', 'f': '\f'}


def _unescape(s):
    return _UNESC.sub(lambda m: _UNESC_MAP.get(m.group(1), m.group(1)), s)


def _i18n_key(s):
    """jamovi-compiler parseContext(): a trailing ' [ctx]' is msgctxt, not msgid."""
    m = re.match(r'(.+) \[([^\[\]]+)\]$', s, re.S)
    return m.group(1) if m else s


def _pot_msgids(p):
    """msgids of a .pot (obsolete #~ entries and the header skipped)."""
    ids, cur, field = [], None, None
    for line in list(open(p, encoding='utf-8')) + ['msgid ""']:
        if line.startswith('#'):
            continue
        m = re.match(r'(msgctxt|msgid|msgid_plural|msgstr(?:\[\d+\])?)\s+"(.*)"\s*$', line)
        if m:
            if m.group(1) == 'msgid' and cur is not None:
                ids.append(cur)
            field = m.group(1)
            if field == 'msgid':
                cur = _unescape(m.group(2))
        elif field == 'msgid' and line.strip().startswith('"'):
            cur += _unescape(line.strip()[1:-1])
    return [i for i in ids if i]


def check_i18n_catalog_scope():
    """jamovi/i18n must describe THIS module (library review guide section 9). The updater
    copies the umbrella catalog and jmvtools::i18nUpdate() trims it to the module's strings;
    without the trim a submodule ships every other module's msgids (2026-09-16 OncoPath:
    31,690 entries, 7.3 MB of inst/i18n json). Used strings are rebuilt like the compiler:
    .()/.('') literals in R/, and - as a superset - every string in jamovi yaml and js."""
    pot = 'jamovi/i18n/catalog.pot'
    if not os.path.exists(pot):
        print('  i18n catalog: none')
        return
    used = set()
    lit = re.compile(r'[^a-zA-Z._]\.\((?:"([^"\\]*(?:\\.[^"\\]*)*)"|\'([^\'\\]*(?:\\.[^\'\\]*)*)\')')
    for p in glob.glob('R/*.R'):
        if p.endswith('.h.R'):
            continue
        src = re.sub(r'\\u([0-9A-Fa-f]{4})', lambda m: chr(int(m.group(1), 16)),
                     open(p, encoding='utf-8', errors='replace').read())
        used.update(_i18n_key(_unescape(a or b)) for a, b in lit.findall(src))

    def leaves(node):
        if isinstance(node, str):
            used.add(_i18n_key(node.strip()))
        elif isinstance(node, dict):
            for v in node.values():
                leaves(v)
        elif isinstance(node, list):
            for v in node:
                leaves(v)
    for p in glob.glob('jamovi/*.yaml'):
        leaves(_safe_yaml(p))
    for p in glob.glob('jamovi/js/**/*.js', recursive=True):
        for a, b in re.findall(r'"((?:[^"\\]|\\.)*)"|\'((?:[^\'\\]|\\.)*)\'', open(p, encoding='utf-8').read()):
            used.add(_i18n_key(_unescape(a or b)))
    ids = _pot_msgids(pot)
    foreign = sorted(i for i in ids if i not in used)
    if foreign:
        WARN.append('%d of %d catalog.pot msgids are used by no source in this module - run '
                    'jmvtools::i18nUpdate() here (the umbrella catalog was copied untrimmed): %s'
                    % (len(foreign), len(ids), ', '.join(repr(f[:40]) for f in foreign[:5])))
    print('  i18n catalog: %d msgids, %d not used by this module' % (len(ids), len(foreign)))


_DOT_LIT = re.compile(r'(?<![\w.])\.\(\s*"((?:[^"\\\n]|\\.)*)"')


def _file_shipped(p):
    """A generated submodule ships everything in R/. In the umbrella an analysis ships by its
    menuGroup and a helper `<analysis>-<topic>.R` with its owner; other helpers (utils*.R) ship."""
    base = os.path.basename(p)
    if FULL or not base.endswith('.R'):
        return True
    if base.endswith('.b.R'):
        return _shipped(base[:-4])
    owner = re.split(r'[-_]', base[:-2])[0]
    return _shipped(owner) if os.path.exists('jamovi/%s.a.yaml' % owner) else True


def _dot_literals():
    """(file, line, literal) for every .("...") in R/, comments skipped."""
    for p in sorted(glob.glob('R/*.R')):
        if p.endswith('.h.R'):
            continue
        for i, l in enumerate(open(p, encoding='utf-8', errors='replace'), 1):
            if not l.lstrip().startswith('#'):
                for s in _DOT_LIT.findall(l):
                    yield p, i, s


def check_i18n_bracket():
    """jmvcore's Translator splits "(.*) \\[(.*)\\]": when a string has no catalog entry - any
    language without a catalog - everything from " [" on is dropped (2026-09-16 OncoPath [INFO]:
    diagnosticmeta's LR notes ended at "(specificity"). Section 9."""
    hits = [(p, '%s:%d' % (os.path.basename(p), i)) for p, i, s in _dot_literals() if re.search(r' \[.*\]', s)]
    ship = [x for p, x in hits if _file_shipped(p)]
    if ship:
        FAIL.append('%d .() strings contain " [..]", which jmvcore cuts off when untranslated: %s'
                    % (len(ship), ', '.join(cap(ship))))
    print('  .() " [..]" truncation: %d sites (%d shipped)' % (len(hits), len(ship)))


def check_i18n_braced_escape():
    """The catalog extractor (jamovi-compiler i18n.js) decodes only \\uXXXX. A braced \\u{XXXX}
    inside .() reaches the catalog as a literal escape, so its msgid never matches the runtime
    string and the sentence stays English in every language. Section 9."""
    hits = [(p, '%s:%d' % (os.path.basename(p), i)) for p, i, s in _dot_literals() if '\\u{' in s]
    ship = [x for p, x in hits if _file_shipped(p)]
    if ship:
        WARN.append('%d .() strings use a braced \\u{XXXX} escape and can never be translated '
                    '(write \\uXXXX): %s' % (len(ship), ', '.join(cap(ship))))
    print('  .() braced \\u{} escapes: %d sites (%d shipped)' % (len(hits), len(ship)))


_FMT = re.compile(r'%(?:\d+\$)?[-+0#]*\d*(?:\.\d+)?[sdif]')
_BAD_PCT = re.compile(r'%(?!(?:\d+\$)?[-+0#]*\d*(?:\.\d+)?[sdifeEgGxXo])')


def _po_entries(p):
    """(msgid, msgstr) pairs of a .po file, continuation lines joined."""
    out, cur, field = [], {}, None
    for line in list(open(p, encoding='utf-8')) + ['msgid ""']:
        if line.startswith('#'):
            continue
        m = re.match(r'(msgctxt|msgid|msgid_plural|msgstr)(?:\[\d+\])?\s+"(.*)"\s*$', line)
        if m:
            if m.group(1) in ('msgctxt', 'msgid') and 'msgstr' in cur:
                out.append((cur.get('msgid', ''), cur['msgstr']))
                cur = {}
            field = m.group(1)
            cur[field] = cur.get(field, '') + _unescape(m.group(2))
        elif field and line.strip().startswith('"'):
            cur[field] += _unescape(line.strip()[1:-1])
    return [(i, s) for i, s in out if i and s]


def check_i18n_po_formats():
    """A translated sprintf template must keep the msgid's conversions. tr.po rendered "50%%" as
    "%%%50", which leaves "%50'" - sprintf() stops with "unrecognised format specification" in
    Turkish only (2026-09-16 OncoPath: diagnosticmeta negative LR; jsurvival singlearm)."""
    bad = []
    for p in sorted(glob.glob('jamovi/i18n/*.po')):
        for msgid, msgstr in _po_entries(p):
            if not (_FMT.search(msgid) or '%%' in msgid):
                continue
            strip = lambda s: re.sub('%%', '', s)
            if (sorted(_FMT.findall(strip(msgid))) != sorted(_FMT.findall(strip(msgstr)))
                    or bool(_BAD_PCT.search(strip(msgstr))) != bool(_BAD_PCT.search(strip(msgid)))):
                bad.append('%s: %s' % (os.path.basename(p), msgid[:50]))
    if bad:
        WARN.append('%d translations do not keep their sprintf conversions (sprintf() fails in that '
                    'language): %s' % (len(bad), '; '.join(cap(bad, 6))))
    print('  translation format specifiers: %d mismatched' % len(bad))


def check_notice_title_colour():
    """Notice renderers put titles on a translucent tint; a fixed hue there fell to 2.7-2.9:1 on
    jamovi's dark pane (#dc2626, #2563eb). Titles inherit; the border carries severity. Section 4."""
    hits = []
    for p in sorted(glob.glob('R/*.R')):
        src = open(p, encoding='utf-8', errors='replace').read()
        for m in re.finditer(r'\.renderNotices\s*=\s*function', src):
            body = src[m.start(): m.start() + 3000]
            if re.search(r"<strong style='color: \",\s*\w+\$color", body) or \
                    re.search(r'\bcolor\s*=\s*"#[0-9A-Fa-f]{6}"', body):
                hits.append((p, os.path.basename(p)))
    ship = [x for p, x in hits if _file_shipped(p)]
    if ship:
        WARN.append('%d notice renderers give titles a fixed colour (unreadable on the dark theme): %s'
                    % (len(ship), ', '.join(cap(ship))))
    print('  notice title colours: %d renderers (%d shipped)' % (len(hits), len(ship)))


if __name__ == '__main__':
    print('RELEASE GATE  %s\n' % ROOT)
    for fn in (check_versions, check_news, check_license, check_refs, check_clearwith, check_renderfun,
               check_artifacts, check_tame, check_visible_bang, check_entities,
               check_requires_data, check_render_private_state, check_bare_set_seed, check_unused_imports, check_collapsebox_titlecase, check_i18n_padding,
               check_i18n_catalog_scope, check_i18n_bracket, check_i18n_braced_escape,
               check_i18n_po_formats, check_notice_title_colour):
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
