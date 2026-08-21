#!/usr/bin/env python3
"""Annotate a jamovi library audit report in place with our response to each finding.

The reviewer's text is never altered. A blockquote response block is appended to
the END of each `### [SEVERITY] ...` section, so the reader sees the finding
first and our answer immediately after it.

Re-running replaces any previous response block for the same finding, so this is
idempotent and safe to run after a status changes.

Usage:
    python3 tools/annotate_audit.py responses.json [--check]

responses.json is a list of:
    {"file": "<audit file name>", "heading": "<exact ### heading text>",
     "status": "DONE|PARTIAL|DEFERRED|REJECTED|NOT_APPLICABLE",
     "reason": "...", "evidence": "...", "docs": "..."}
"""
import json
import re
import sys
from pathlib import Path

AUDIT_DIR = Path(__file__).resolve().parent.parent / 'jamovi-library-audit'

MARK_START = '<!-- response:start -->'
MARK_END = '<!-- response:end -->'

BADGE = {
    'DONE': '✅ DONE',
    'PARTIAL': '🟡 PARTIAL',
    'DEFERRED': '⏸️ DEFERRED',
    'REJECTED': '🚫 REJECTED',
    'NOT_APPLICABLE': '➖ NOT APPLICABLE',
}


def block(entry, date):
    lines = [MARK_START,
             '> **%s** — %s' % (BADGE[entry['status']], entry['reason'].strip())]
    ev = (entry.get('evidence') or '').strip()
    if ev:
        lines.append('>')
        lines.append('> *Evidence:* %s' % ev)
    docs = (entry.get('docs') or '').strip()
    if docs and docs.lower() != 'none':
        lines.append('>')
        lines.append('> *Rule captured in:* %s' % docs)
    elif docs.lower() == 'none':
        lines.append('>')
        lines.append('> *Rule captured in:* not documented — no general rule to capture.')
    lines.append('>')
    lines.append('> <sub>reviewed %s</sub>' % date)
    lines.append(MARK_END)
    return '\n'.join(lines)


def strip_old(text):
    return re.sub(re.escape(MARK_START) + r'.*?' + re.escape(MARK_END) + r'\n*',
                  '', text, flags=re.S)


def annotate(path, entries, date):
    src = strip_old(path.read_text(encoding='utf-8'))
    lines = src.split('\n')

    # index every '### ' heading -> (line_no, text)
    heads = [(i, lines[i][4:].strip()) for i in range(len(lines))
             if lines[i].startswith('### ')]

    def section_end(idx):
        """line index one past the end of the section starting at heads[idx]."""
        start = heads[idx][0]
        for j in range(start + 1, len(lines)):
            if lines[j].startswith('### ') or lines[j].startswith('## ') or lines[j].startswith('---'):
                return j
        return len(lines)

    inserts = []          # (line_index, text)
    unmatched = []
    for e in entries:
        want = e['heading'].strip()
        hit = next((k for k, (_, h) in enumerate(heads) if h == want), None)
        if hit is None:                      # tolerate minor formatting drift
            norm = lambda s: re.sub(r'[^a-z0-9]+', '', s.lower())
            hit = next((k for k, (_, h) in enumerate(heads) if norm(h) == norm(want)), None)
        if hit is None:
            unmatched.append(want)
            continue
        end = section_end(hit)
        while end > heads[hit][0] + 1 and not lines[end - 1].strip():
            end -= 1                          # tuck in before trailing blank lines
        inserts.append((end, block(e, date)))

    for at, text in sorted(inserts, key=lambda x: -x[0]):
        lines.insert(at, '\n' + text + '\n')

    path.write_text('\n'.join(lines), encoding='utf-8')
    return len(inserts), unmatched


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    if not args:
        print(__doc__)
        return 1
    entries = json.loads(Path(args[0]).read_text(encoding='utf-8'))
    date = args[1] if len(args) > 1 else '2026-08-20'

    by_file = {}
    for e in entries:
        by_file.setdefault(e['file'], []).append(e)

    total, missing = 0, []
    for fname, items in by_file.items():
        path = AUDIT_DIR / fname
        if not path.exists():
            print('MISSING FILE: %s' % fname)
            missing.extend(i['heading'] for i in items)
            continue
        n, un = annotate(path, items, date)
        total += n
        missing.extend('%s :: %s' % (fname, u) for u in un)
        print('%-46s %2d/%-2d annotated' % (fname, n, len(items)))

    if missing:
        print('\nUNMATCHED HEADINGS (%d):' % len(missing))
        for m in missing:
            print('  ', m)
        return 1
    print('\n%d findings annotated' % total)
    return 0


if __name__ == '__main__':
    sys.exit(main())
