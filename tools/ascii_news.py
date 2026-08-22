#!/usr/bin/env python3
"""Convert non-ASCII characters in Markdown release notes to ASCII equivalents.

NEWS.md is read by clinicians and by R CMD check. Typographic dashes, math glyphs,
Greek letters and decorative emoji all render inconsistently across the terminal,
the jamovi help pane, and PDF conversion, and they trip the non-ASCII checks this
project already enforces on R source. Nothing here carries meaning that an ASCII
spelling cannot.

Usage: ascii_news.py [--apply] FILE...
"""
import sys, unicodedata, re

MAP = {
    '—': ' -- ', '–': '-', '−': '-', '‐': '-', '‑': '-',
    '→': '->', '←': '<-', '↓': 'down', '↑': 'up',
    '×': 'x', '±': '+/-', '≥': '>=', '≤': '<=',
    '≈': '~', '≡': '==', '≫': '>>', '√': 'sqrt',
    '∫': 'integral', '∈': 'in', '…': '...', '·': '.',
    '²': '^2', '³': '^3',
    '‘': "'", '’': "'", '“': '"', '”': '"',
    ' ': ' ', '​': '', '‍': '', '️': '',
    'é': 'e', 'ö': 'o', 'ü': 'u', 'è': 'e', 'ä': 'a',
    '̂': '',
    # Greek
    'α': 'alpha', 'β': 'beta', 'γ': 'gamma', 'δ': 'delta',
    'ε': 'epsilon', 'η': 'eta', 'κ': 'kappa', 'λ': 'lambda',
    'σ': 'sigma', 'τ': 'tau', 'φ': 'phi', 'χ': 'chi',
    'ω': 'omega', 'Δ': 'Delta', 'Σ': 'Sigma', 'μ': 'mu',
    # subscripts
    '₀': '0', '₁': '1', '₂': '2', '₃': '3', '₄': '4',
    'ₚ': 'p', 'ₛ': 's', 'ₓ': 'x', 'ᵢ': 'i',
    # check marks / bullets that act as list decoration
    '✅': '', '✓': '', '✖': '', '✚': '', '⭐': '',
    '●': '', '■': '', '▲': '', '▼': '', '◆': '',
    '★': '', '◑': '', '◎': '',
}

def convert(text):
    out = []
    for ch in text:
        if ord(ch) < 128:
            out.append(ch); continue
        if ch in MAP:
            out.append(MAP[ch]); continue
        cat = unicodedata.category(ch)
        # So = emoji/symbols -> drop; anything else -> best-effort transliteration
        if cat in ('So', 'Sk', 'Cf'):
            out.append('')
        else:
            d = unicodedata.normalize('NFKD', ch)
            out.append(''.join(c for c in d if ord(c) < 128))
    s = ''.join(out)
    # Collapse only INTERIOR runs left by dropped glyphs. Leading whitespace is
    # load-bearing in Markdown: a list item's continuation lines must stay indented.
    s = re.sub(r'(?<=\S)[ \t]{2,}', ' ', s)
    s = re.sub(r'(^|\n)(\s*[-*]) +', r'\1\2 ', s)  # tidy list markers
    s = re.sub(r'(?<![\w*])\*\* +', '**', s)   # emoji removal can leave "** text**"
    # A line that STARTS with an em dash maps to ' -- ', adding a stray leading space.
    s = re.sub(r'(^|\n)([ \t]*) -- ', r'\1\2-- ', s)
    s = re.sub(r'[ \t]+(\n|$)', r'\1', s)     # strip trailing space
    return s

if __name__ == '__main__':
    args = sys.argv[1:]
    apply = '--apply' in args
    files = [a for a in args if a != '--apply']
    total = 0
    for p in files:
        t = open(p, encoding='utf-8').read()
        n = sum(1 for c in t if ord(c) > 127)
        if not n:
            continue
        total += n
        if apply:
            open(p, 'w', encoding='utf-8').write(convert(t))
        print(f"{p}: {n} non-ASCII char(s){' converted' if apply else ''}")
    print(f"total: {total} non-ASCII char(s){'' if apply else '  (dry run, pass --apply)'}")
    sys.exit(0 if apply or total == 0 else 1)
