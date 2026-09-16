#!/usr/bin/env python3
"""Write our response under a finding in a jamovi library audit report, in place.

The reviewer's text is never altered. Each `### [SEVERITY] title` section holds at
most one <!-- response:start --> ... <!-- response:end --> blockquote, at its end.
Only the sections named in the input are rewritten: other findings' responses, the
status dashboard and anything after "## What's Already Working Well" are left
byte-for-byte. Re-running the same input produces the same file.

Usage:
    python3 tools/annotate_audit.py responses.json [--date YYYY-MM-DD] [--check]
    python3 tools/annotate_audit.py --summary ["<report>.md" ...]
    python3 tools/annotate_audit.py --selftest

responses.json is a list of
    {"file": "2026-09-16 OncoPath.md", "heading": "[LOW] exact heading text",
     "status": "DONE|PARTIAL|DEFERRED|REJECTED|NOT_APPLICABLE",
     "reason": "...", "evidence": "...", "docs": "... or none"}
reason/evidence/docs may hold blank-line-separated paragraphs.
--date defaults to today. --check writes nothing and lists matched/unmatched
headings plus the findings that would still have no response.
--summary prints STATUS.md scoreboard rows and every finding not DONE, for the
latest report of each module unless reports are named.

The report markdown is the only store: write the JSON to a scratchpad per finding.
Used by the library-audit skill (.claude/skills/library-audit/).
"""
import datetime
import json
import re
import sys
from pathlib import Path

AUDIT_DIR = Path(__file__).resolve().parent.parent / 'jamovi-library-audit'
MARK_START, MARK_END = '<!-- response:start -->', '<!-- response:end -->'
BADGE = {'DONE': '✅ DONE', 'PARTIAL': '🟡 PARTIAL', 'DEFERRED': '⏸️ DEFERRED',
         'REJECTED': '🚫 REJECTED', 'NOT_APPLICABLE': '➖ NOT APPLICABLE'}
SEVERITY = ('CRITICAL', 'HIGH', 'MEDIUM', 'LOW', 'INFO')
FINDING = re.compile(r'^### (\[(%s)\] .*)$' % '|'.join(SEVERITY))
OLD_BLOCK = re.compile(r'\n*' + re.escape(MARK_START) + r'.*?' + re.escape(MARK_END), re.S)


def norm(s):
    return re.sub(r'[^a-z0-9]+', '', s.lower())


def quote(text):
    return '\n'.join('> ' + l if l.strip() else '>' for l in text.strip().split('\n'))


def block(e, date):
    parts = ['**%s** — %s' % (BADGE[e['status']], e['reason'].strip())]
    if (e.get('evidence') or '').strip():
        parts.append('*Evidence:* ' + e['evidence'].strip())
    docs = (e.get('docs') or '').strip()
    if docs:
        parts.append('*Rule captured in:* ' + ('not documented — no general rule to capture.'
                                               if docs.lower() == 'none' else docs))
    parts.append('<sub>reviewed %s</sub>' % date)
    return MARK_START + '\n' + quote('\n\n'.join(parts)) + '\n' + MARK_END


def sections(lines):
    """(start, end, heading) per finding; a section ends at the next ###, ## or --- line."""
    out = []
    for i, line in enumerate(lines):
        m = FINDING.match(line)
        if m:
            end = next((k for k in range(i + 1, len(lines))
                        if lines[k].startswith(('### ', '## ', '---'))), len(lines))
            out.append((i, end, m.group(1).strip()))
    return out


def status_of(section_text):
    m = re.search(re.escape(MARK_START) + r'\n> \*\*(.+?)\*\*', section_text)
    return m and next((k for k, v in BADGE.items() if v == m.group(1)), m.group(1))


def apply(text, entries, date):
    lines = text.split('\n')
    secs = sections(lines)
    hits, unmatched = {}, []
    for e in entries:
        want = e['heading'].strip().lstrip('#').strip()
        s = (next((s for s in secs if s[2] == want), None)
             or next((s for s in secs if norm(s[2]) == norm(want)), None))
        if s is None:
            unmatched.append(want)
        else:
            hits[s[0]] = (s, e)
    for start in sorted(hits, reverse=True):          # bottom-up keeps earlier indices valid
        (s, end, _), e = hits[start]
        body = OLD_BLOCK.sub('', '\n'.join(lines[s:end])).rstrip('\n')
        lines[s:end] = (body + '\n\n' + block(e, date) + '\n').split('\n')
    return '\n'.join(lines), len(hits), unmatched


def unanswered(text):
    lines = text.split('\n')
    return [h for s, e, h in sections(lines) if not status_of('\n'.join(lines[s:e]))]


def summary(paths):
    if not paths:                                     # latest report per module
        latest = {}
        for p in sorted(AUDIT_DIR.glob('[0-9]*.md')):
            latest[p.stem.split(' ', 1)[1]] = p
        paths = sorted(latest.values(), reverse=True)
    print('| Report | Findings | ✅ done | 🟡 partial | ⏸️ deferred | 🚫 rejected | ➖ n/a | no response |')
    print('|---|---|---|---|---|---|---|---|')
    still_open = []
    for p in paths:
        lines = p.read_text(encoding='utf-8').split('\n')
        secs, count = sections(lines), {}
        for s, e, h in secs:
            st = status_of('\n'.join(lines[s:e]))
            count[st] = count.get(st, 0) + 1
            if st not in ('DONE', 'NOT_APPLICABLE'):
                still_open.append((SEVERITY.index(h[1:h.index(']')]), p.stem, h, st or 'NO RESPONSE'))
        print('| [%s](%s) | %d | %s |' % (p.stem, p.name.replace(' ', '%20'), len(secs),
              ' | '.join(str(count.get(k, 0)) for k in list(BADGE) + [None])))
    print('\n| Report | Finding | Status |\n|---|---|---|')
    for _, report, h, st in sorted(still_open):
        print('| %s | %s | %s |' % (report, h, st))


def selftest():
    doc = ('# R\n<!-- status:start -->\n## Dash\n<!-- status:end -->\n\n---\n\n## What I Found\n\n'
           '### [CRITICAL] A\nbody a\n\n%s\n> **✅ DONE** — hand written\n%s\n\n'
           '### [LOW] B\nbody b\n\n## What\'s Already Working Well\n- ok\n\n%s\n> **✅ X**\n%s\n\n— Claudia\n'
           % (MARK_START, MARK_END, MARK_START, MARK_END))
    e = {'heading': '### [LOW] B', 'status': 'DEFERRED', 'reason': 'para-one\n\npara-two',
         'evidence': 'ev', 'docs': 'none'}
    out, n, un = apply(doc, [e], '2026-09-16')
    assert (n, un) == (1, [])
    assert 'hand written' in out and '> **✅ X**' in out and '<!-- status:start -->' in out
    assert out.count(MARK_START) == 3 and '\n>\n> para-two' in out
    assert apply(out, [e], '2026-09-16')[0] == out                     # idempotent
    out2 = apply(out, [dict(e, status='DONE', reason='fixed')], '2026-09-16')[0]
    assert out2.count(MARK_START) == 3 and 'para-one' not in out2
    assert apply(doc, [], 'x')[0] == doc                                # no-op is byte-identical
    assert apply(doc, [dict(e, heading='[LOW] missing')], 'x')[2] == ['[LOW] missing']
    assert unanswered(doc) == ['[LOW] B'] and unanswered(out) == []
    print('selftest ok')


def main(argv):
    flags = {a for a in argv if a.startswith('--')}
    args = [a for a in argv if not a.startswith('--')]
    if '--selftest' in flags:
        return selftest()
    if '--summary' in flags:
        return summary([Path(a) if Path(a).exists() else AUDIT_DIR / a for a in args])
    date = next((argv[i + 1] for i, a in enumerate(argv[:-1]) if a == '--date'), None)
    if date:
        args.remove(date)
    date = date or datetime.date.today().isoformat()
    if len(args) != 1:
        print(__doc__)
        return 1
    by_file = {}
    for e in json.loads(Path(args[0]).read_text(encoding='utf-8')):
        by_file.setdefault(e['file'], []).append(e)
    failed = False
    for fname, items in by_file.items():
        path = AUDIT_DIR / fname
        if not path.exists():
            print('MISSING FILE: %s' % fname)
            failed = True
            continue
        out, n, unmatched = apply(path.read_text(encoding='utf-8'), items, date)
        print('%-46s %2d/%-2d matched' % (fname, n, len(items)))
        for u in unmatched:
            print('   UNMATCHED: %s' % u)
        failed |= bool(unmatched)
        if '--check' in flags:
            for h in unanswered(out):
                print('   no response yet: %s' % h)
        elif not unmatched:
            path.write_text(out, encoding='utf-8')
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
