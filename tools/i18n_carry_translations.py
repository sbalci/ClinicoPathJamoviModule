# List, and optionally restore, the Turkish translations that reworded English strings lost.
# jmvtools::i18nUpdate() gives a reworded msgid an EMPTY msgstr and drops the old entry - there is no
# "#, fuzzy" carry-over - so every relabel silently falls back to English for Turkish users.
#
# Usage (run from anywhere; <module> is a sibling or the umbrella root):
#   python3 tools/i18n_carry_translations.py <module> [--base REV] [--write]
#   python3 tools/i18n_carry_translations.py --selftest
#
# Compares jamovi/i18n/tr.po in the working tree with the one at REV (default HEAD). An entry that is
# empty now, whose wording did not exist at REV, and matches a translated REV entry once case, spelling and
# punctuation are folded (`fold()`), is a CARRY: --write copies the old translation into it. Any other
# empty entry whose wording replaced a translated one is printed as REVIEW - its meaning changed, so a
# person rewrites the Turkish. Edits touch only the matched msgstr lines (no polib re-wrap).
# OBSOLETE: a live msgid that also survives as an obsolete "#~" entry (a label renamed back to an old
# wording) - msgfmt -c rejects the file as a duplicate definition; delete the "#~" block.
import re, subprocess, sys

PO = 'jamovi/i18n/tr.po'
ENTRY = re.compile(r'^msgid ((?:".*"\n)+)msgstr ((?:".*"\n?)+)', re.M)
join = lambda block: ''.join(re.findall(r'"(.*)"', block))
# American and British forms, and the cutoff spellings, fold together; so do case and quote marks.
SPELLING = [('analys', 'analyz'), ('colour', 'color'), ('centre', 'center'), ('equalis', 'equaliz'),
            ('standardis', 'standardiz'), ('cut-off', 'cutoff'), ('cut-point', 'cutoff'), ('cutpoint', 'cutoff')]

def fold(s):
    s = s.lower()
    for brit, amer in SPELLING:
        s = s.replace(brit, amer)
    return re.sub(r"[\s'’]+", ' ', s).strip()

def parse(text):
    return {join(m.group(1)): (join(m.group(2)), m) for m in ENTRY.finditer(text)}

def plan(old_text, new_text):
    old, new = parse(old_text), parse(new_text)
    translated = {fold(k): v for k, (v, _) in old.items() if k and v}
    carry, review = [], []
    for k, (v, m) in new.items():
        if not k or v or k in old:
            continue                     # translated, or this wording already existed at REV
        if fold(k) in translated:
            carry.append((k, translated[fold(k)], m))
        else:
            review.append(k)
    return carry, review

def obsolete_duplicates(text):
    live = set(parse(text))
    return sorted(set(re.findall(r'^#~ msgid "(.+)"$', text, re.M)) & live)

def apply(text, carry):
    for k, tr, m in sorted(carry, key=lambda c: c[2].start(), reverse=True):
        text = text[:m.start(2)] + '"' + tr + '"\n' + text[m.end(2):]
    return text

def main(argv):
    if argv[:1] == ['--selftest']:
        old = 'msgid "Youden Index"\nmsgstr "Youden indeksi"\n\nmsgid "Analysed rows"\nmsgstr "Analiz edilen"\n\n' \
              'msgid "Null kappa (kappa0)"\nmsgstr "Sifir kappa (kappa0)"\n\nmsgid "Never translated"\nmsgstr ""\n'
        new = 'msgid "Youden index"\nmsgstr ""\n\nmsgid "Analyzed rows"\nmsgstr ""\n\n' \
              'msgid "Null kappa"\nmsgstr ""\n\nmsgid "Never translated"\nmsgstr ""\n'
        carry, review = plan(old, new)
        assert sorted(k for k, _, _ in carry) == ['Analyzed rows', 'Youden index'], carry
        assert review == ['Null kappa'], review           # meaning changed: a person rewrites it
        out = apply(new, carry)
        assert 'msgid "Youden index"\nmsgstr "Youden indeksi"' in out and out.count('msgstr ""') == 2, out
        assert obsolete_duplicates(new + '\n#~ msgid "Null kappa"\n#~ msgstr "x"\n') == ['Null kappa']
        print('selftest ok'); return 0
    root = argv[0]
    base = argv[argv.index('--base') + 1] if '--base' in argv else 'HEAD'
    old = subprocess.run(['git', '-C', root, 'show', f'{base}:{PO}'], capture_output=True, text=True, check=True).stdout
    path = f'{root}/{PO}'
    new = open(path, encoding='utf-8').read()
    carry, review = plan(old, new)
    for k, tr, _ in carry:
        print(f'CARRY   {k[:90]}  ->  {tr[:60]}')
    for k in review:
        print(f'REVIEW  {k[:120]}')
    dups = obsolete_duplicates(new)
    for k in dups:
        print(f'OBSOLETE  {k[:110]}  (also a "#~" entry: msgfmt -c fails; delete that block)')
    print(f'{len(carry)} to carry, {len(review)} to review (new or reworded, empty, no translated twin at {base}), '
          f'{len(dups)} obsolete duplicates')
    if '--write' in argv and carry:
        open(path, 'w', encoding='utf-8').write(apply(new, carry))
        print(f'wrote {len(carry)} translations; rebuild the json: Rscript --vanilla tools/i18n_rebuild_json.R {root}')
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
