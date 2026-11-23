#!/usr/bin/env python3
"""Wrap the text of static HTML literals in .() so the translation catalogue can see it.

    python3 tools/i18n_wrap_html_literals.py R/<file>.b.R [...]            # dry run: what would change
    python3 tools/i18n_wrap_html_literals.py --write R/<file>.b.R [...]    # rewrite in place
    python3 tools/i18n_wrap_html_literals.py --pairs out.json R/<file>.b.R # old/new expression pairs
    python3 tools/i18n_wrap_html_literals.py --selftest

An explanation panel written as one HTML string literal never reaches the catalogue (library audit
2026-11-20 jsurvival [LOW]; review guide section 9, round 9). This rewrites such a literal, or a
paste0() whose arguments are all literals, into paste0() of markup strings and .("...") calls: one
call per paragraph, heading, list item or table cell. Block tags stay outside the string, inline
tags (<strong>, <i>, <sub> ...) stay inside it, so no sentence is cut into fragments.

It only touches what it can prove safe, and prints what it skipped:
  - R6 methods only (`.name = function(`): jmvcore's .() needs `self`, a file-level helper has none
  - the literal must begin and end with a block tag, so no text touches a neighbouring argument
  - no sprintf/glue placeholders, and inline tags balanced inside every text node
The English output is unchanged up to whitespace around block tags. Prove it for a run with --pairs
and tools/i18n_wrap_html_literals_check.R, which evaluates both expressions with `.` as identity.
"""
import json
import re
import sys

TOKEN = re.compile(r'#[^\n]*|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|`[^`\n]*`', re.S)
BLOCK = r'div|p|h[1-6]|ul|ol|li|table|thead|tbody|tfoot|tr|td|th|br|hr|details|summary|section|blockquote|pre|dl|dt|dd|caption|center'
BLOCK_SPLIT = re.compile(r'(<\s*/?\s*(?:%s)\b[^>]*>)' % BLOCK, re.I)
STYLE_SPLIT = re.compile(r'(<style\b.*?</style>)', re.I | re.S)
TAG = re.compile(r'<\s*(/?)\s*([A-Za-z][A-Za-z0-9]*)\b[^>]*?(/?)>')
VOID = {'br', 'hr', 'img', 'input', 'wbr'}
PLACEHOLDER = re.compile(r'%[-+0#]*\d*(?:\.\d+)?[sdifgeE]\b|\{[A-Za-z_.][\w.$]*\}|\{\}')   # not "50% in"
METHOD = re.compile(r'^[ \t]+(\.[A-Za-z_][\w.]*)[ \t]*=[ \t]*function[ \t]*\(', re.M)
TOPLEVEL = re.compile(r'^(\.?[A-Za-z_][\w.]*)[ \t]*(?:<-|=)[ \t]*(?:function|if|R6::|R6Class)', re.M)


def decode(tok):
    """The value of an R string literal token (quotes included)."""
    s, out, i = tok[1:-1], [], 0
    while i < len(s):
        c = s[i]
        if c != '\\':
            out.append(c)
            i += 1
            continue
        e = s[i + 1]
        simple = {'n': '\n', 't': '\t', 'r': '\r', '\\': '\\', '"': '"', "'": "'", '`': '`', '0': '\0'}
        if e in simple:
            out.append(simple[e])
            i += 2
        elif e in 'uU':
            m = re.match(r'\{([0-9A-Fa-f]{1,8})\}|([0-9A-Fa-f]{1,%d})' % (4 if e == 'u' else 8), s[i + 2:])
            out.append(chr(int(m.group(1) or m.group(2), 16)))
            i += 2 + m.end()
        elif e == 'x':
            m = re.match(r'[0-9A-Fa-f]{1,2}', s[i + 2:])
            out.append(chr(int(m.group(0), 16)))
            i += 2 + m.end()
        else:
            out.append(e)
            i += 2
    return ''.join(out)


def encode(s, prefer_single=False):
    """An ASCII R string literal for s; never the braced escape form, which the extractor skips."""
    def esc(ch, quote):
        o = ord(ch)
        if ch == '\\':
            return '\\\\'
        if ch == quote:
            return '\\' + quote
        if ch == '\n':
            return '\\n'
        if ch == '\t':
            return '\\t'
        if 32 <= o < 127:
            return ch
        return '\\u%04x' % o if o <= 0xFFFF else '\\U%08x' % o
    quote = "'" if prefer_single and '"' in s and "'" not in s else '"'
    return quote + ''.join(esc(ch, quote) for ch in s) + quote


def squeeze(s):
    return re.sub(r'\s+', ' ', s)


def pieces(html):
    """[('m', markup) | ('t', text)] for a decoded literal, or (None, reason) when it is not safe."""
    out, edge_text = [], []

    def text_node(t):
        t = squeeze(t).strip()
        if not t:
            return None
        while True:                                   # peel an inline element that wraps the whole node
            m = re.fullmatch(r'(<([A-Za-z][A-Za-z0-9]*)\b[^>]*>)(.*)(</\2\s*>)', t, re.S)
            if not m or re.search(r'<\s*/?\s*%s\b' % m.group(2), m.group(3), re.I):
                break
            out.append(('m', m.group(1)))
            inner = text_node(m.group(3))
            out.append(('m', m.group(4)))
            return inner
        plain = re.sub(r'<[^>]*>|&[A-Za-z#0-9]+;', ' ', t)
        if not re.search(r'[A-Za-z]{2,}', plain):
            out.append(('m', t))
            return None
        if PLACEHOLDER.search(t):
            return 'a placeholder (%s) in the text' % PLACEHOLDER.search(t).group(0)
        depth = {}
        for m in TAG.finditer(t):
            name = m.group(2).lower()
            if name in VOID or m.group(3):
                continue
            depth[name] = depth.get(name, 0) + (-1 if m.group(1) else 1)
            if depth[name] < 0:
                return 'an inline </%s> whose opening tag is elsewhere' % name
        if any(depth.values()):
            return 'an inline tag left open in a text node'
        out.append(('t', t))
        return None

    for si, part in enumerate(STYLE_SPLIT.split(html)):
        if si % 2:
            out.append(('m', squeeze(part)))
            continue
        for bi, chunk in enumerate(BLOCK_SPLIT.split(part)):
            if bi % 2:
                out.append(('m', squeeze(chunk)))
            else:
                before = len(out)
                problem = text_node(chunk)
                if problem:
                    return None, problem
                if len(out) > before:
                    edge_text.append(len(out))
    if not any(k == 't' for k, _ in out):
        return None, None
    if out[0][0] != 'm' or out[-1][0] != 'm' or not BLOCK_SPLIT.match(squeeze(html).strip()) \
            or not re.search(r'%s\s*$' % BLOCK_SPLIT.pattern, squeeze(html).strip(), re.I):
        return None, 'text at the edge of the literal (a fragment of a longer sentence)'
    merged = []
    for kind, value in out:
        if kind == 'm' and merged and merged[-1][0] == 'm':
            merged[-1] = ('m', merged[-1][1] + value)
        else:
            merged.append((kind, value))
    return merged, None


def render(parts, indent):
    pad = ' ' * (indent + 4)
    lines = [(pad + '.(' + encode(v) + ')') if k == 't' else (pad + encode(v, prefer_single=True)) for k, v in parts]
    return 'paste0(\n' + ',\n'.join(lines) + ')'


def units(src):
    """Candidate spans: every string literal, and every paste0() whose arguments are all literals."""
    toks = [t for t in TOKEN.finditer(src)]
    masked = list(src)
    for t in toks:
        fill = ' ' if t.group(0)[0] == '#' else 'x'
        for q in range(t.start(), t.end()):
            if masked[q] != '\n':
                masked[q] = fill
    masked = ''.join(masked)
    strings = {t.start(): t for t in toks if t.group(0)[0] in '"\''}
    taken, found = [], []
    for m in re.finditer(r'(?<![\w.$])paste0\s*\(', masked):
        depth, j, last, spans = 1, m.end(), m.end(), []
        while j < len(masked) and depth:
            ch = masked[j]
            depth += (ch in '([{') - (ch in ')]}')
            if (ch == ',' and depth == 1) or depth == 0:
                spans.append((last, j))
                last = j + 1
            j += 1
        args = []
        for a, b in spans:
            lead = a + len(masked[a:b]) - len(masked[a:b].lstrip())
            tok = strings.get(lead)
            if tok is None or masked[tok.end(): b].strip():
                args = None
                break
            args.append(tok)
        if args:
            found.append((m.start(), j, ''.join(decode(t.group(0)) for t in args)))
            taken.append((m.start(), j))
    for start, tok in strings.items():
        if not any(a <= start < b for a, b in taken):
            found.append((start, tok.end(), decode(tok.group(0))))
    return sorted(found), masked


def owner_at(src, pos):
    """('method', name) when pos is inside an R6 method, ('top', name) inside a file-level function."""
    method = [(m.start(), m.group(1)) for m in METHOD.finditer(src) if m.start() <= pos]
    top = [(m.start(), m.group(1)) for m in TOPLEVEL.finditer(src) if m.start() <= pos]
    in_class = bool(top) and re.match(r'.*(?:R6::)?R6Class', src[top[-1][0]: src.find('\n', top[-1][0]) + 200], re.S)
    if method and in_class and method[-1][0] > top[-1][0]:
        return 'method', method[-1][1]
    return 'top', top[-1][1] if top else '<top>'


def rewrite(src, only=None):
    edits, skipped, pairs = [], [], []
    found, masked = units(src)
    for start, end, html in found:
        if masked[max(0, start - 8): start].rstrip().endswith('.('):
            continue
        parts, problem = pieces(html)
        kind, name = owner_at(src, start)
        if only and name not in only:
            continue
        line = src.count('\n', 0, start) + 1
        if parts is None:
            if problem and len(re.findall(r"[A-Za-z][A-Za-z'/-]*(?: [A-Za-z(][A-Za-z'/().,;:-]*){3,}",
                                          squeeze(re.sub(r'<[^>]*>', ' ', html)))):
                skipped.append((line, name, problem))
            continue
        if kind != 'method':
            skipped.append((line, name, 'a file-level function: .() has no self here; use the caller\'s translator'))
            continue
        indent = len(src[src.rfind('\n', 0, start) + 1: start]) - len(src[src.rfind('\n', 0, start) + 1: start].lstrip())
        new = render(parts, indent)
        edits.append((start, end, new, line, name, sum(1 for k, _ in parts if k == 't')))
        pairs.append({'line': line, 'owner': name, 'old': src[start:end], 'new': new})
    out = src
    for start, end, new, _, _, _ in sorted(edits, reverse=True):
        out = out[:start] + new + out[end:]
    return out, edits, skipped, pairs


def selftest():
    src = '''xClass <- R6::R6Class("xClass", private = list(
    .panel = function() {
        self$results$a$setContent('
            <div style="margin: 5px;">
                <h4> Understanding the result</h4>
                <p><strong>What is it?</strong> It is the <i>first</i> time at which 50% remain.</p>
                <table><tr><td><strong>Records</strong></td><td>0.5</td></tr></table>
            </div>')
        html <- paste0("<p>The test is controlled by <b>rho (\\u{03c1})</b>, ",
                       "a single number.</p>")
        paste0("<p>The median follow-up in this cohort was ", med, " months.</p>")
        sprintf("<p>A total of %d patients were analysed here</p>", n)
    }
))
.helper <- function() "<p>This sentence lives in a file-level helper.</p>"
'''
    out, edits, skipped, _ = rewrite(src)
    assert '.("Understanding the result")' in out, out
    assert '.("<strong>What is it?</strong> It is the <i>first</i> time at which 50% remain.")' in out, out
    assert "'<td><strong>'" in out.replace('</tr></table></div>', '') or '<strong>' in out
    assert '.("Records")' in out and '.("0.5")' not in out, out
    assert '.("The test is controlled by <b>rho (\\u03c1)</b>, a single number.")' in out, out   # never \\u{...}
    assert '"<p>The median follow-up in this cohort was "' in out and 'patients were analysed here</p>", n)' in out, out
    assert '.helper <- function() "<p>This sentence lives' in out, out
    reasons = ' | '.join(r for _, _, r in skipped)
    assert 'fragment' in reasons and 'placeholder' in reasons and 'file-level' in reasons, reasons
    assert len(edits) == 2, edits
    again, edits2, _, _ = rewrite(out)
    assert again == out and not edits2, 'not idempotent'
    print('selftest ok')


if __name__ == '__main__':
    argv = sys.argv[1:]
    if '--selftest' in argv:
        selftest()
        sys.exit(0)
    write = '--write' in argv
    pairs_path = argv[argv.index('--pairs') + 1] if '--pairs' in argv else None
    only = set(argv[argv.index('--only') + 1].split(',')) if '--only' in argv else None
    files = [a for i, a in enumerate(argv) if not a.startswith('--') and (i == 0 or argv[i - 1] not in ('--pairs', '--only'))]
    all_pairs = []
    for path in files:
        src = open(path, encoding='utf-8').read()
        out, edits, skipped, pairs = rewrite(src, only)
        for p in pairs:
            p['file'] = path
        all_pairs += pairs
        per = {}
        for _, _, _, _, name, k in edits:
            per[name] = per.get(name, 0) + k
        print('%s: %d literal(s), %d text node(s)%s' % (path, len(edits), sum(per.values()), ' -> written' if write and edits else ''))
        for name, k in sorted(per.items(), key=lambda kv: -kv[1]):
            print('    wrap  %-44s %d' % (name, k))
        for line, name, why in skipped:
            print('    skip  %s:%d %s - %s' % (path.split('/')[-1], line, name, why))
        if write and edits:
            open(path, 'w', encoding='utf-8').write(out)
    if pairs_path:
        json.dump(all_pairs, open(pairs_path, 'w', encoding='utf-8'), ensure_ascii=True, indent=0)
