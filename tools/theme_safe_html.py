#!/usr/bin/env python3
"""Make inline HTML panel styling in jamovi results theme-safe.

Problem (jamovi library audit, 2026-08-17):
  Html results in this module paint an opaque light-theme pastel background
  (#f8d7da, #fff3cd, #e3f2fd, #f8f9fa ...) and usually set no text colour.  In
  jamovi's light theme the inherited text colour is dark, so it reads fine.  In
  the dark theme the inherited text colour is light and lands on a pale pastel --
  the error messages, warnings and clinical interpretations become unreadable
  exactly when they matter most.

Fix:
  Replace the opaque background with a translucent tint that reproduces the
  ORIGINAL colour EXACTLY when composited over white, and let the text colour
  follow the pane.

    result_over_white = (1 - a) * 255 + a * T   ==   P      (per channel)
    =>  T = (P - (1 - a) * 255) / a

  Choosing  a >= max_ch((255 - P_ch) / 255)  keeps every channel of T in
  [0, 255].  So the light theme is pixel-identical, and over a dark pane the
  same rgba() tints instead of replacing the background.

Why this works at the CSS-DECLARATION level, not the style-attribute level:
  An earlier version matched whole `style="..."` attributes with one regex and
  silently missed 200 backgrounds -- style strings split across R string
  concatenation, `style =` passed as an htmltools argument, or held in a
  variable.  Matching the declaration itself has no such blind spot, and
  inserting `color: inherit` immediately after the background keeps the pairing
  inside the same declaration list however the R source is wrapped.

Opaque saturated backgrounds (badges/chips) keep their fill -- they are
deliberate -- but are given an explicit foreground so they stay legible.
"""
import re
import sys
import glob

# a CSS background declaration with a hex value, wherever it appears in R source
BG = re.compile(r'\bbackground(-color)?\s*:\s*(#[0-9a-fA-F]{3,8})')
# a CSS foreground declaration (not `background-color`, not `border-color`)
FG = re.compile(r'(?<![-\w])color\s*:\s*(#[0-9a-fA-F]{3,8})')

ALPHA_HEADROOM = 1.15
ALPHA_MIN, ALPHA_MAX = 0.06, 0.90
# above this HSL lightness a background is a "pale panel tint"; below it, it is a
# deliberate saturated chip and the fill is left alone.  HSL lightness separates
# these far better than relative luminance, which drags hue-heavy pastels like
# #ffcdd2 (L = 0.90) below any sensible luminance threshold and misreads them.
PALE_L = 0.80
# a foreground this dark is invisible on a dark pane
DARK_TEXT_L = 0.50


def parse_hex(h):
    h = h.lstrip('#')
    if len(h) == 3:
        h = ''.join(c * 2 for c in h)
    if len(h) == 8:          # #rrggbbaa already carries alpha
        return None
    if len(h) != 6:
        return None
    return tuple(int(h[i:i + 2], 16) for i in (0, 2, 4))


def lightness(rgb):
    """HSL lightness in [0, 1]."""
    return (max(rgb) + min(rgb)) / 2.0 / 255.0


def luminance(rgb):
    """WCAG relative luminance -- used only to pick a chip's foreground."""
    def lin(c):
        c /= 255.0
        return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4
    r, g, b = (lin(c) for c in rgb)
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def translucent(rgb):
    """rgba() that composites over white to exactly `rgb`."""
    a = max((255 - c) / 255.0 for c in rgb) * ALPHA_HEADROOM
    a = min(max(a, ALPHA_MIN), ALPHA_MAX)
    tint = [int(round(min(max((c - (1 - a) * 255.0) / a, 0), 255))) for c in rgb]
    return 'rgba(%d, %d, %d, %s)' % (
        tint[0], tint[1], tint[2], ('%.2f' % a).rstrip('0').rstrip('.'))


# how far either side of a declaration we look for a companion `color:` before
# concluding the style attribute does not already set one
NEIGHBOURHOOD = 220


ANY_FG = re.compile(r'(?<![-\w])color\s*:\s*[^;\'"}]+')


def _has_foreground(text, start, end):
    """A keyword colour (`white`), `inherit` or `var(...)` counts just as much
    as a hex one, so this deliberately does NOT reuse the hex-only FG regex."""
    before = text[max(0, start - NEIGHBOURHOOD):start]
    after = text[end:end + NEIGHBOURHOOD]
    # `}` ends a rule inside a <style> block, `'`/`"` end an inline attribute --
    # a colour beyond either boundary belongs to something else.
    before = re.split(r'[}\'"]', before)[-1]
    after = re.split(r'[{}\'"]', after)[0]
    return bool(ANY_FG.search(before + after))


def convert(text):
    """Return (new_text, n_pale, n_chip)."""
    counts = {'pale': 0, 'chip': 0}

    def repl(m):
        prop = 'background' + (m.group(1) or '')
        rgb = parse_hex(m.group(2))
        if rgb is None:
            return m.group(0)
        if lightness(rgb) >= PALE_L:
            counts['pale'] += 1
            # `color: inherit` goes immediately after, so it lands in the same
            # declaration list no matter how the R source wraps the string.
            return '%s: %s; color: inherit' % (prop, translucent(rgb))
        # A saturated chip is deliberate: keep the fill, and only add a
        # foreground if the attribute does not already declare one.
        if _has_foreground(text, m.start(), m.end()):
            return m.group(0)
        counts['chip'] += 1
        fg = '#ffffff' if luminance(rgb) < 0.35 else '#111111'
        return '%s: %s; color: %s' % (prop, m.group(2), fg)

    out = BG.sub(repl, text)
    return out, counts['pale'], counts['chip']


def tidy(text):
    """Collapse the duplicate declarations the insertion can create."""
    # `color: inherit; color: inherit`  ->  one
    text = re.sub(r'color:\s*inherit;\s*color:\s*inherit', 'color: inherit', text)
    # `color: #fff; color: #fff`        ->  one
    text = re.sub(r'color:\s*(#[0-9a-fA-F]{3,8});\s*color:\s*\1', r'color: \1', text)
    # `;;` and `; ;`
    text = re.sub(r';\s*;', '; ', text)
    return text


def _override_spans(text):
    """Spans of dark `color:` declarations that follow an rgba background closely
    enough to sit in the same style attribute -- CSS is last-wins, so these
    override the `color: inherit` we just inserted and re-break dark mode."""
    spans = []
    for m in re.finditer(r'background(?:-color)?:\s*rgba\(', text):
        window = text[m.end():m.end() + 260]
        # Stop at the end of the enclosing style attribute or CSS rule. Inside a
        # <style> block `}` ends the rule, and the next rule's color: is unrelated.
        stop = min([i for i in (window.find("'"), window.find('"'), window.find('}'))
                    if i != -1] or [len(window)])
        for fm in FG.finditer(window[:stop]):
            rgb = parse_hex(fm.group(1))
            if rgb is not None and lightness(rgb) < DARK_TEXT_L:
                spans.append((m.end() + fm.start(), m.end() + fm.end(), fm.group(0)))
    return sorted(set(spans))


def fix_overrides(text):
    """Rewrite those dark foregrounds to `inherit`. The semantic colour is
    already carried by the panel's saturated border accent."""
    spans = _override_spans(text)
    for a, b, _ in reversed(spans):
        text = text[:a] + 'color: inherit' + text[b:]
    return text, len(spans)


def darken_check(text):
    return [d for _, _, d in _override_spans(text)]


def process(path, apply_changes):
    src = open(path, encoding='utf-8').read()
    out, pale, chip = convert(src)
    out = tidy(out)
    out, fixed = fix_overrides(out)
    if (pale or chip or fixed) and apply_changes:
        open(path, 'w', encoding='utf-8').write(out)
    return pale, chip, darken_check(out)


if __name__ == '__main__':
    apply_changes = '--apply' in sys.argv
    targets = [a for a in sys.argv[1:] if not a.startswith('--')] or sorted(glob.glob('R/*.R'))
    tp = tc = 0
    overrides = []
    for p in targets:
        pale, chip, dark = process(p, apply_changes)
        if pale or chip:
            print('%5d pale %5d chip  %s' % (pale, chip, p))
        tp += pale
        tc += chip
        overrides += [(p, d) for d in dark]
    print('---')
    print('%d pale background(s) made translucent, %d chip(s) given a foreground%s'
          % (tp, tc, '' if apply_changes else '   (DRY RUN)'))
    if overrides:
        print('\n%d dark `color:` declaration(s) still follow a translucent background '
              'and would override `color: inherit`:' % len(overrides))
        for p, d in overrides[:40]:
            print('   %-44s %s' % (p, d))
    sys.exit(1 if (tp or tc or overrides) and not apply_changes else 0)
