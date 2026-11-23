#!/usr/bin/env python3
"""Turn sentences pasted together around values into translatable templates.

    python3 tools/i18n_wrap_templates.py R/<file>.b.R [...]              # dry run: what would change
    python3 tools/i18n_wrap_templates.py --write R/<file>.b.R [...]      # rewrite in place
    python3 tools/i18n_wrap_templates.py --skip-owners .buildRCode ...   # leave these functions alone
    python3 tools/i18n_wrap_templates.py --selftest

`paste0("The median was ", med, " months.")` never reaches the translation catalogue, and wrapping
each literal on its own would hand the translator two fragments (library audit 2026-11-20 jsurvival
[LOW]; review guide section 9). This rewrites such a call into one template per sentence:

    .pasteT(.("The median was {med} months."), med = med)

`.pasteT()` (R/utils.R) pastes the pieces of the translated template and the values with paste0(),
in the order the template gives them, so vectors, NULL and numbers behave exactly as before and the
English output is unchanged. Block tags stay outside the string, as in i18n_wrap_html_literals.py.

Handled: paste0()/paste() with literal and expression arguments (innermost call first), and the
format string of sprintf(). A call qualifies when one of its literals holds a run of four or more
English words. Reported and left alone: a call with a comment inside it, a literal brace in the
text, `sep =` other than "" or " ", and a file-level function with no `. <- .callerTranslator(`
(jmvcore's .() needs `self`). Every rewrite is checked symbolically: the new expression, with each
template expanded, must reproduce the old sequence of text and expressions exactly.
"""
import importlib.util
import os
import re
import sys

_spec = importlib.util.spec_from_file_location(
    'wraphtml', os.path.join(os.path.dirname(os.path.abspath(__file__)), 'i18n_wrap_html_literals.py'))
W = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(W)

PROSE = re.compile(r"[A-Za-z][A-Za-z'/-]*(?: [A-Za-z(][A-Za-z'/().,;:-]*){3,}")
NAMED = re.compile(r'\s*([A-Za-z_.][\w.]*)\s*=(?!=)')
SIMPLE = re.compile(r'[A-Za-z_.][\w.]*(?:\$[A-Za-z_.][\w.]*)*$')


def plain(text):
    return re.sub(r'\s+', ' ', re.sub(r'<[^>]*>|&[A-Za-z#0-9]+;', ' ', text))


def mask(src):
    toks = list(W.TOKEN.finditer(src))
    masked = list(src)
    for t in toks:
        fill = ' ' if t.group(0)[0] == '#' else 'x'
        for q in range(t.start(), t.end()):
            if masked[q] != '\n':
                masked[q] = fill
    strings = {t.start(): t for t in toks if t.group(0)[0] in '"\''}
    comments = [(t.start(), t.end()) for t in toks if t.group(0)[0] == '#']
    return ''.join(masked), strings, comments


def call_args(masked, open_end):
    """(end, [(a, b)]) for the call whose '(' ends at open_end."""
    depth, j, last, spans = 1, open_end, open_end, []
    while j < len(masked) and depth:
        ch = masked[j]
        depth += (ch in '([{') - (ch in ')]}')
        if (ch == ',' and depth == 1) or depth == 0:
            spans.append((last, j))
            last = j + 1
        j += 1
    return j, spans


def classify(src, masked, strings, a, b):
    seg = masked[a:b]
    lead, trail = a + len(seg) - len(seg.lstrip()), a + len(seg.rstrip())
    if lead >= trail:
        return ('X', '')
    m = NAMED.match(masked, a, b)
    if m:
        return ('N', m.group(1), src[m.end():trail].strip())
    tok = strings.get(lead)
    if tok is not None and tok.end() == trail:
        return ('L', W.decode(tok.group(0)), 'bare')
    if masked.startswith('.(', lead) and masked[trail - 1] == ')':
        inner = masked[lead + 2: trail - 1]
        s0 = lead + 2 + len(inner) - len(inner.lstrip())
        tok = strings.get(s0)
        if tok is not None and not masked[tok.end(): trail - 1].strip():
            return ('L', W.decode(tok.group(0)), 'wrapped')  # already .("..."): merged into the sentence
    return ('E', src[lead:trail])


def glue_seq(text):
    """[('L', text, 'bare') | ('E', expr, '')] for the template of a glue() call, or None.
    glue() drops the first and last line break and the common indentation of a multi-line
    template; that is reproduced here, so only the class of the result changes."""
    if '\n' in text:
        lines = text.split('\n')
        if not lines[0].strip():
            lines = lines[1:]
        if lines and not lines[-1].strip():
            lines = lines[:-1]
        pad = min((len(ln) - len(ln.lstrip(' ')) for ln in lines if ln.strip()), default=0)
        text = '\n'.join(ln[pad:] if ln.strip() else '' for ln in lines)
    seq, buf, i = [], '', 0
    while i < len(text):
        two = text[i:i + 2]
        if two in ('{{', '}}'):
            return None                                     # an escaped brace: leave the call alone
        if text[i] == '{':
            depth, j = 1, i + 1
            while j < len(text) and depth:
                depth += (text[j] == '{') - (text[j] == '}')
                j += 1
            if depth:
                return None
            if buf:
                seq.append(('L', buf, 'bare'))
                buf = ''
            seq.append(('E', text[i + 1: j - 1].strip(), ''))
            i = j
        else:
            buf += text[i]
            i += 1
    if buf:
        seq.append(('L', buf, 'bare'))
    return seq


def placeholder(expr, used):
    """A short letters-and-digits name for an expression, stable for a repeated expression."""
    for name, e in used.items():
        if e == expr:
            return name
    base, target, prefix = '', expr, ''
    call = re.match(r'([A-Za-z_.][\w.:]*)\(\s*(?:["\'][^"\']*["\']\s*,\s*)?([A-Za-z_.][\w.]*(?:\$[A-Za-z_.][\w.]*)*)\s*[,)\[]', expr)
    if not SIMPLE.match(expr) and call and '\n' not in expr:      # round(x, 1), sprintf("%.1f", x), length(x)
        target = call.group(2)
        prefix = 'n' if call.group(1).split('::')[-1] in ('length', 'nrow', 'sum', 'NROW') else ''
    if SIMPLE.match(target):
        words = [w for w in re.split(r'[^A-Za-z0-9]+', target.split('$')[-1]) if w]
        if prefix:
            words = [prefix] + words
        base = ''.join(w if i == 0 else w[:1].upper() + w[1:] for i, w in enumerate(words))
        if not re.match(r'[A-Za-z]', base or '0') or base in ('self', 'private', 'x', 'TRUE', 'FALSE', 'NULL'):
            base = ''
    base = base or 'v'
    name, k = (base, 1) if base != 'v' else ('v1', 1)
    while name in used:
        k += 1
        name = '%s%d' % (base, k)
    used[name] = expr
    return name


def nodes(seq):
    """Split [('L', text) | ('E', expr)] into pieces: ('m', [items]) markup, ('n', [items]) one node."""
    stream, in_tag = [], False
    for kind, val, how in seq:
        if kind == 'E':
            stream.append(('ME' if in_tag else 'E', val))
            continue
        text = val
        while text:
            if in_tag:
                close = text.find('>')
                if close < 0:
                    stream.append(('M', text))
                    text = ''
                else:
                    stream.append(('M', text[:close + 1]))
                    text, in_tag = text[close + 1:], False
                continue
            open_ = text.rfind('<')
            if open_ >= 0 and '>' not in text[open_:] and re.match(r'<\s*/?\s*[A-Za-z!]', text[open_:]):
                head, tail = text[:open_], text[open_:]       # a tag left open: an expression sits inside it
            else:
                head, tail = text, ''
            for i, chunk in enumerate(W.BLOCK_SPLIT.split(head)):
                if chunk:
                    stream.append(('M', chunk) if i % 2 else ('L', chunk, how))
            if tail:
                stream.append(('M', tail))
                in_tag = True
            text = ''
    out = []
    for item in stream:
        group = 'm' if item[0] in ('M', 'ME') else 'n'
        if out and out[-1][0] == group:
            out[-1][1].append(item)
        else:
            out.append((group, [item]))
    return out


def build(seq):
    """Pieces for the new paste0(): (src, expansion) pairs, or (None, reason)."""
    pieces, touched = [], [0]

    def lit(text):
        if not text:
            return
        if pieces and pieces[-1][1] and len(pieces[-1][1]) == 1 and pieces[-1][1][0][0] == 'L' \
                and not pieces[-1][0].startswith('.('):
            text = pieces[-1][1][0][1] + text                 # two plain literals in a row: one literal
            pieces.pop()
        pieces.append((W.encode(text, prefer_single=True), [('L', text)]))

    for group, items in nodes(seq):
        if group == 'm':
            run = ''
            for kind, val in [it[:2] for it in items]:
                if kind == 'M':
                    run += val
                else:
                    lit(run)
                    run = ''
                    pieces.append((val, [('E', val)]))
            lit(run)
            continue
        bare = any(it[0] == 'L' and it[2] == 'bare' and re.search(r'[A-Za-z]{2,}', plain(it[1])) for it in items)
        sentence = PROSE.search(plain(''.join(it[1] if it[0] == 'L' else ' X ' for it in items)))
        text = ''.join(it[1] for it in items if it[0] == 'L')
        if not bare or not sentence or re.search(r'\w::\w|<-|\w\(.*~', text):   # nothing unwrapped, no sentence, or R code
            for it in items:
                if it[0] == 'E':
                    pieces.append((it[1], [('E', it[1])]))
                elif it[2] == 'wrapped':
                    pieces.append(('.(' + W.encode(it[1]) + ')', [('L', it[1])]))
                else:
                    lit(it[1])
            continue
        touched[0] += 1
        items = [it[:2] for it in items]
        close_tag = ''
        if items[0][0] == 'L' and items[-1][0] == 'L':
            head = re.match(r'\s*(<([A-Za-z][A-Za-z0-9]*)\b[^>]*>)', items[0][1])
            tail = head and re.search(r'(</%s\s*>)\s*$' % head.group(2), items[-1][1])
            if tail and len(re.findall(r'<\s*/?\s*%s\b' % head.group(2), text, re.I)) == 2:
                close_tag = items[-1][1][tail.start():]       # an inline element that wraps the whole node
                items[-1] = ('L', items[-1][1][:tail.start()])
                lit(items[0][1][:head.end()])
                items[0] = ('L', items[0][1][head.end():])
                text = ''.join(v for k, v in items if k == 'L')
        if '{' in text or '}' in text:
            return None, 'a literal brace in the text'
        lead = trail = ''
        if items[0][0] == 'L':
            t = items[0][1]
            lead, items[0] = t[:len(t) - len(t.lstrip())], ('L', t.lstrip())
        if items[-1][0] == 'L':
            t = items[-1][1]
            trail, items[-1] = t[len(t.rstrip()):], ('L', t.rstrip())
        items = [it for it in items if it[1] != '']
        lit(lead)
        if all(k == 'L' for k, _ in items):
            body = ''.join(v for _, v in items)
            pieces.append(('.(' + W.encode(body) + ')', [('L', body)]))
        else:
            used, template = {}, ''
            for kind, val in items:
                template += val if kind == 'L' else '{%s}' % placeholder(val, used)
            args = ', '.join('%s = %s' % (n, e) for n, e in used.items())
            pieces.append(('.pasteT(.(' + W.encode(template) + '), ' + args + ')', list(items)))
        lit(trail)
        lit(close_tag)
    if not touched[0]:
        return None, None
    return pieces, None


def flatten(items):
    out = []
    for kind, val in items:
        if kind == 'L' and out and out[-1][0] == 'L':
            out[-1] = ('L', out[-1][1] + val)
        elif not (kind == 'L' and val == ''):
            out.append((kind, val))
    return out


def owner_ok(src, pos):
    kind, name = W.owner_at(src, pos)
    if kind == 'method':
        return True, name
    top = [m.start() for m in W.TOPLEVEL.finditer(src) if m.start() <= pos]
    return bool(top) and '.callerTranslator(' in src[top[-1]:pos], name


def one_pass(src, skip_owners):
    masked, strings, comments = mask(src)
    cands, skipped = [], []
    for m in re.finditer(r'(?<![\w.$])(paste0|paste|sprintf|glue::glue|glue)\s*\(', masked):
        end, spans = call_args(masked, m.end())
        args = [classify(src, masked, strings, a, b) for a, b in spans]
        args = [a for a in args if a[0] != 'X']
        pos = [a for a in args if a[0] in 'LE']
        line = src.count('\n', 0, m.start()) + 1
        if masked[max(0, m.start() - 8): m.start()].rstrip().endswith('.('):
            continue
        fn = m.group(1)
        ok, owner = owner_ok(src, m.start())
        if fn == 'sprintf':
            if not pos or pos[0][0] != 'L' or args[0][0] != 'L' or args[0][2] != 'bare' \
                    or not PROSE.search(plain(pos[0][1])):
                continue
            a, b = spans[0]
            seg = masked[a:b]
            lead = a + len(seg) - len(seg.lstrip())
            if masked.startswith('.(', lead):
                continue
            text = pos[0][1]
            body = text.strip()
            new = '.(' + W.encode(body) + ')'
            if body != text:
                parts = [W.encode(text[:len(text) - len(text.lstrip())]), new, W.encode(text[len(text.rstrip()):])]
                new = 'paste0(' + ', '.join(p for p in parts if p != '""') + ')'
            cands.append((lead, a + len(seg.rstrip()), new, line, owner, ok, None))
            continue
        if not any(a[0] == 'L' and a[2] == 'bare' for a in args):
            continue
        named = {a[1]: a[2] for a in args if a[0] == 'N'}
        sep = ' ' if fn == 'paste' else ''
        problem = None
        if fn.endswith('glue'):
            if named or len(pos) != 1 or pos[0][0] != 'L' or pos[0][2] != 'bare':
                continue
            gseq = glue_seq(pos[0][1])
            if gseq is None:
                skipped.append((line, owner, 'glue() with an escaped or unbalanced brace'))
                continue
            if W.BLOCK_SPLIT.search(pos[0][1]):             # HTML: runs of whitespace mean nothing, so squeeze them
                gseq = [(k, re.sub(r'\s+', ' ', v), h) if k == 'L' else (k, v, h) for k, v, h in gseq]
            pos = gseq
        if 'sep' in named:
            if fn == 'paste' and named['sep'] in ('""', "''", '" "', "' '"):
                sep = '' if named['sep'] in ('""', "''") else ' '
            else:
                problem = 'sep = %s' % named['sep']
        if set(named) - {'sep', 'collapse'}:
            problem = 'argument %s' % ', '.join(sorted(set(named) - {'sep', 'collapse'}))
        if any(a < ce and cs < end for cs, ce in comments for a in [m.start()]):
            problem = 'a comment inside the call'
        seq = []
        for a in pos:
            if seq and sep:
                seq.append(('L', sep, 'sep'))
            seq.append(a if a[0] == 'L' else (a[0], a[1], ''))
        if fn.endswith('glue'):
            comments = [c for c in comments if not (m.start() <= c[0] < end)]   # '#' inside the template is text
        pieces = None
        if problem is None:
            pieces, problem = build(seq)
            if pieces is None and problem is None:
                continue                                    # no node with an unwrapped sentence
        if problem is None and flatten([it for _, exp in pieces for it in exp]) != flatten([x[:2] for x in seq]):
            problem = 'the rewrite does not reproduce the call (tool defect)'
        if problem:
            if PROSE.search(plain(' '.join(a[1] for a in pos if a[0] == 'L' and a[2] == 'bare'))):
                skipped.append((line, owner, problem))
            continue
        cands.append((m.start(), end, (pieces, named.get('collapse')), line, owner, ok, seq))
    inner = [c for c in cands if not any(o is not c and c[0] <= o[0] and o[1] <= c[1] for o in cands)]
    edits = []
    for start, end, new, line, owner, ok, seq in inner:
        if owner in skip_owners:
            continue
        if not ok:
            skipped.append((line, owner, 'a file-level function with no `. <- .callerTranslator(parent.frame())`'))
            continue
        if not isinstance(new, str):
            pieces, collapse = new
            indent = len(src[src.rfind('\n', 0, start) + 1: start]) - len(src[src.rfind('\n', 0, start) + 1: start].lstrip())
            parts = [p for p, _ in pieces]
            if len(parts) == 1 and collapse is None:
                new = parts[0]
            else:
                if collapse is not None:
                    parts.append('collapse = ' + collapse)
                one = 'paste0(' + ', '.join(parts) + ')'
                new = one if len(one) + indent <= 110 and '\n' not in one else \
                    'paste0(\n' + ',\n'.join(' ' * (indent + 4) + p for p in parts) + ')'
        if new != src[start:end]:
            edits.append((start, end, new, line, owner))
    out = src
    for start, end, new, _, _ in sorted(edits, reverse=True):
        out = out[:start] + new + out[end:]
    return out, edits, skipped


def rewrite(src, skip_owners=()):
    total, skipped = [], []
    for _ in range(8):
        out, edits, skipped = one_pass(src, set(skip_owners))
        if not edits:
            break
        total += edits
        src = out
    return src, total, skipped


def selftest():
    src = r'''xClass <- R6::R6Class("xClass", private = list(
    .summary = function(med, n, lo) {
        a <- paste0("The median follow-up in this cohort was ", med, " months.")
        b <- paste0("<p><b>Records:</b> ", n, " patients were included in this analysis ",
                    if (lo) "after exclusions", ".</p>")
        d <- paste("Zero survival times detected:", n, "of the rows are affected.")
        e <- sprintf("A total of %d patients were analysed here", n)
        f <- paste0("<div style='color:", col, ";'>The estimate could not be computed here.</div>")
        g <- paste0("Use {braces} only in code that is never translated ", n)
        h <- paste0(x, collapse = ", ")
        g <- glue::glue("
                <p>All {n} {if (n == 1) 'participant was' else 'participants were'} removed by the landmark here.</p>
                <li>Check the unit of the landmark ({unit})</li>")
        k <- paste0("<li>", paste0("The interval ending at ", t, " holds the peak rate"), "</li>", collapse = "")
        w <- paste0("<p><strong style='color: inherit;'> WARNING: The assumption may be violated here</strong></p>")
        r <- paste0("<code>survival::coxph(Surv(time, status) ~ other vars + strata(", v, "))</code>", "<p>Stratify by the variable if it has few categories.</p>")
        z <- paste0("The value was ", round(est, 2), " across ", length(rows), " rows of data.")
        q <- paste0("<p style='margin:0;", "background-color: rgba(1, 2, 3, 0.1); color: inherit'>",
                    .("<b>No events: check the event level.</b>"), " ", .fmt(.("It occurs in {n} rows."), n = n), "</p>")
        a
    }
))
.helper <- function(n) paste0("This helper counted ", n, " records in the file.")
'''
    out, edits, skipped = rewrite(src)
    assert '.pasteT(.("The median follow-up in this cohort was {med} months."), med = med)' in out, out
    assert '"<p><b>Records:</b> "' not in out and '.pasteT(.("<b>Records:</b> {n} patients were included in this analysis {v1}."), n = n, v1 = if (lo) "after exclusions")' in out, out
    assert '.pasteT(.("Zero survival times detected: {n} of the rows are affected."), n = n)' in out, out
    assert 'sprintf(.("A total of %d patients were analysed here"), n)' in out, out
    assert "\"<div style='color:\", col, \";'>\", .(\"The estimate could not be computed here.\"), \"</div>\"" in out, out
    assert 'paste0("Use {braces}' in out and 'paste0(x, collapse = ", ")' in out, out
    assert 'paste0("<li>", .pasteT(.("The interval ending at {t} holds the peak rate"), t = t), "</li>", collapse = "")' in out, out
    assert '.helper <- function(n) paste0("This helper counted "' in out, out
    flat = re.sub(r'\s+', ' ', out)
    assert "\"<p><strong style='color: inherit;'> \", .(\"WARNING: The assumption may be violated here\"), \"</strong></p>\")" in flat, out
    assert '"<code>survival::coxph(Surv(time, status) ~ other vars + strata("' in flat and '.("Stratify by the variable if it has few categories.")' in flat, out
    assert '.pasteT(.("The value was {est} across {nRows} rows of data."), est = round(est, 2), nRows = length(rows))' in out, out
    assert '.("<b>No events: check the event level.</b>"), " ", .fmt(.("It occurs in {n} rows."), n = n), "</p>")' in out, out
    assert ".pasteT(.(\"All {n} {v1} removed by the landmark here.\"), n = n, v1 = if (n == 1) 'participant was' else 'participants were')" in out, out
    assert '.pasteT(.("Check the unit of the landmark ({unit})"), unit = unit)' in out and 'glue::glue' not in out, out
    why = ' | '.join(r for _, _, r in skipped)
    assert 'brace' in why and 'file-level' in why, why
    again, edits2, _ = rewrite(out)
    assert again == out and not edits2, 'not idempotent'
    print('selftest ok')


if __name__ == '__main__':
    argv = sys.argv[1:]
    if '--selftest' in argv:
        selftest()
        sys.exit(0)
    write = '--write' in argv
    skip = set(argv[argv.index('--skip-owners') + 1].split(',')) if '--skip-owners' in argv else set()
    files = [a for i, a in enumerate(argv) if not a.startswith('--') and (i == 0 or argv[i - 1] != '--skip-owners')]
    for path in files:
        src = open(path, encoding='utf-8').read()
        out, edits, skipped = rewrite(src, skip)
        per = {}
        for _, _, _, _, name in edits:
            per[name] = per.get(name, 0) + 1
        print('%s: %d call(s)%s' % (path, len(edits), ' -> written' if write and edits else ''))
        for name, k in sorted(per.items(), key=lambda kv: -kv[1]):
            print('    wrap  %-44s %d' % (name, k))
        for line, name, why in skipped:
            print('    skip  %s:%d %s - %s' % (os.path.basename(path), line, name, why))
        if write and edits:
            open(path, 'w', encoding='utf-8').write(out)
