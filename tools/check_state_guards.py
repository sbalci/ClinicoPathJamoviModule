#!/usr/bin/env python3
"""Find render functions that read image$state without a NULL guard.

A render function can run when .run() did NOT set the state:
  * .run() returned early on a validation failure,
  * jamovi re-invoked the renderer on resize/redraw,
  * jmvcore's .load() restored a saved .omv without re-running.

When the state is absent the renderer hands NULL to a plotting library and the
user sees a raw library error instead of a clean empty panel.  The house
pattern is:

    plotData <- image$state
    if (is.null(plotData))
        return(FALSE)

Exit status is 1 when any unguarded read remains, so this is usable in CI.
"""
import re, sys, glob

ASSIGN = re.compile(r'^\s*([A-Za-z_.][\w.]*)\s*<-\s*(image[A-Za-z0-9_.]*)\$state\s*$')
SUBFIELD = re.compile(r'^\s*([A-Za-z_.][\w.]*)\s*<-\s*(image[A-Za-z0-9_.]*)\$state\$')

FORWARD_LINES = 7
BACKWARD_LINES = 12


def guard_forward(var, window):
    v = re.escape(var)
    return re.search(r'is\.null\(\s*%s\b|length\(\s*%s\s*\)\s*(==|<)\s*(0|1)' % (v, v), window)


def guard_backward(img, window):
    return re.search(r'is\.null\(\s*%s\$state\b' % re.escape(img), window)


def scan(paths):
    findings = []
    for path in paths:
        lines = open(path, encoding='utf-8', errors='replace').read().split('\n')
        for i, line in enumerate(lines):
            m = ASSIGN.match(line) or SUBFIELD.match(line)
            if not m:
                continue
            var, img = m.group(1), m.group(2)
            fwd = '\n'.join(lines[i + 1:i + 1 + FORWARD_LINES])
            back = '\n'.join(lines[max(0, i - BACKWARD_LINES):i])
            if guard_backward(img, back):
                continue
            if guard_forward(var, fwd):
                # NULL$field is NULL, so a guard on the extracted value covers a
                # missing parent state too.
                continue
            if SUBFIELD.match(line):
                findings.append((path, i + 1, line.strip(),
                                 'guard %s$state before the read' % img))
                continue
            findings.append((path, i + 1, line.strip(),
                             'add: if (is.null(%s)) return(FALSE)' % var))
    return findings


if __name__ == '__main__':
    targets = sys.argv[1:] or sorted(glob.glob('R/*.b.R'))
    findings = scan(targets)
    for path, ln, src, fix in findings:
        print('%s:%d\n    %s\n    -> %s' % (path, ln, src, fix))
    print('---\n%d unguarded image$state read(s)' % len(findings))
    sys.exit(1 if findings else 0)
