#!/usr/bin/env python3
"""Offline, single-pass variant of /review-article-stats for a local Ollama model.

The article never leaves this machine (useful for manuscripts under confidential peer review).
No subagents, skills, web lookups, or YAML roadmap: one prompt = article text + a module function
catalog built from jamovi/*.a.yaml + a trimmed version of the review checklist.

Usage:
    python3 .claude/scripts/review_article_stats_local.py ARTICLE.pdf [MORE_FILES...] --label Kemp-2015
    options: --model gemma4:e4b  --ctx 32768  --think  --out PATH
    python3 .claude/scripts/review_article_stats_local.py --selftest

Output: literature/<label>-citation-review-local.md
"""
import argparse
import glob
import json
import re
import shutil
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OLLAMA = "http://localhost:11434/api/chat"
OUTPUT_RESERVE = 8000  # tokens kept free for the report itself
CHARS_PER_TOKEN = 3.5  # conservative; real count is checked against prompt_eval_count afterwards

SYSTEM = (
    "You are an expert biostatistician who reviews the statistical methods of pathology and "
    "clinical research articles, and a developer of the ClinicoPath jamovi module."
)

TASK = """# TASK
Review the ARTICLE above. Write a Markdown report with exactly these sections.

## 1. Article summary
Title, journal, year, DOI/PMID (only if printed in the ARTICLE, otherwise "not found"), study design, N, groups, endpoints, key analyses.

## 2. Extracted statistical methods
Table: | Method | Role (primary/secondary) | Variants & options | Assumption checks reported | Where (section/table) |
List every test, model, multiple-testing correction, effect size, agreement and validation metric the article uses. Do not list methods it does not use.

## 3. ClinicoPath coverage matrix
Table: | Article method | ClinicoPath function(s) | Coverage | Notes |
- Function names MUST be copied exactly from the CATALOG and wrapped in backticks. If nothing fits write none and mark ❌. Never invent a function name.
- Coverage: ✅ covered, 🟡 partial (say exactly which option is missing), ❌ not covered.

## 4. Critical evaluation
Overall rating (✅ appropriate / 🟡 minor issues / ❌ major concerns) and a 2-4 sentence summary.
Table: | Aspect | Score (0-2) | Evidence (section/page) | Recommendation |
Rows: design-method alignment; assumptions & diagnostics; sample size & power; multiplicity control; model specification & confounding; missing data handling; effect sizes & CIs; validation & calibration; reproducibility/transparency. Then Total score /18.
The overall rating must follow the total: 15-18 ✅, 9-14 🟡, 0-8 ❌.
Check these red flags: chi-square with expected counts < 5; unadjusted multiple comparisons; stepwise selection without validation; proportional hazards not checked; too few events per variable; p-values without effect sizes; kappa or ICC without CI; agreement judged by correlation only.
Base every judgement on the ARTICLE text. If something is not reported, write "not reported" instead of guessing.

## 5. Gap analysis and prioritized backlog
For each 🟡/❌ method: where the article uses it, closest ClinicoPath function, exact missing options. Then a ranked backlog (impact vs effort).

## 6. Caveats
Uncertain method identifications, and content the plain-text extraction may have lost (tables, figures, equations).
"""


def build_catalog():
    rows = []
    for f in sorted(glob.glob(str(ROOT / "jamovi" / "*.a.yaml"))):
        head = Path(f).read_text(encoding="utf-8", errors="replace")[:3000]
        field = lambda k: (re.search(rf"^{k}:\s*(.+)$", head, re.M) or [None, ""])[1].strip().strip("'\"")
        if field("name"):
            rows.append(f"{field('name')} | {field('title')} | {field('menuSubgroup')}")
    return rows


def read_source(path):
    p = Path(path)
    if not p.is_file():
        sys.exit(f"not found: {path}")
    ext = p.suffix.lower()
    if ext in {".md", ".txt", ".csv"}:
        return p.read_text(encoding="utf-8", errors="replace")
    if ext == ".pdf" and shutil.which("pdftotext"):
        # no -layout: it keeps two-column pages side by side and interleaves the reading order
        return subprocess.run(["pdftotext", str(p), "-"], capture_output=True, text=True, check=True).stdout
    tool = shutil.which("markitdown") or str(ROOT / ".venv" / "bin" / "markitdown")
    return subprocess.run([tool, str(p)], capture_output=True, text=True, check=True).stdout


def drop_references(text):
    """Cut the reference list (last 'References' heading in the second half) to save context."""
    hits = [m for m in re.finditer(r"^\s*(References|REFERENCES|Bibliography|Literature Cited)\s*$", text, re.M)
            if m.start() > len(text) / 2]
    return text[: hits[-1].start()] if hits else text


def unverified_functions(report, catalog_names):
    """Backticked names in the coverage-matrix section that are not real module functions."""
    m = re.search(r"^#+\s*3\..*?$(.*?)(?=^#+\s*4\.|\Z)", report, re.M | re.S)
    names = set(re.findall(r"`([A-Za-z][A-Za-z0-9_.]*)`", m.group(1))) if m else set()
    return sorted(n for n in names if n not in catalog_names and n.lower() != "none")


def rating_mismatch(report):
    """Overall rating that contradicts the /18 total (15-18 ✅, 9-14 🟡, 0-8 ❌); small models drift here."""
    rating = re.search(r"Overall rating\W*?(✅|🟡|❌)", report, re.I)
    totals = re.findall(r"(\d{1,2})\s*/\s*18\b", report)
    if not rating or not totals:
        return None
    total = int(totals[-1])
    expected = "✅" if total >= 15 else "🟡" if total >= 9 else "❌"
    if rating.group(1) != expected:
        return f"Overall rating {rating.group(1)} contradicts total {total}/18 (expected {expected})."
    return None


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="*")
    ap.add_argument("--label")
    ap.add_argument("--model", default="gemma4:e4b")
    ap.add_argument("--ctx", type=int, default=32768)
    ap.add_argument("--think", action="store_true", help="enable model thinking (slower)")
    ap.add_argument("--out")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args()
    if args.selftest:
        return selftest()
    if not args.files:
        ap.error("give at least one article file")

    label = args.label or Path(args.files[0]).stem
    out = Path(args.out) if args.out else ROOT / "literature" / f"{label}-citation-review-local.md"

    catalog = build_catalog()
    catalog_block = "# CATALOG (ClinicoPath functions: name | title | menu)\n" + "\n".join(catalog)
    article = "\n\n".join(f"## Source: {Path(f).name}\n{drop_references(read_source(f))}" for f in args.files)

    fixed_tokens = int((len(SYSTEM) + len(TASK) + len(catalog_block)) / CHARS_PER_TOKEN) + 200
    budget_chars = int((args.ctx - OUTPUT_RESERVE - fixed_tokens) * CHARS_PER_TOKEN)
    if budget_chars < 4000:
        sys.exit(f"--ctx {args.ctx} leaves no room for the article; use --ctx 32768 or more")
    truncated = len(article) > budget_chars
    if truncated:
        print(f"warning: article truncated {len(article)} -> {budget_chars} chars; raise --ctx", file=sys.stderr)
        article = article[:budget_chars]

    user = f"# ARTICLE\n{article}\n\n{catalog_block}\n\n{TASK}"
    body = {
        "model": args.model,
        "messages": [{"role": "system", "content": SYSTEM}, {"role": "user", "content": user}],
        "stream": False,
        "think": args.think,
        "options": {"num_ctx": args.ctx, "num_predict": OUTPUT_RESERVE, "temperature": 0.2},
    }
    print(f"{args.model}: ~{int(len(user) / CHARS_PER_TOKEN)} prompt tokens, ctx {args.ctx} ...", file=sys.stderr)
    t0 = time.time()
    req = urllib.request.Request(OLLAMA, data=json.dumps(body).encode(), headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=7200) as r:
        res = json.load(r)
    minutes = (time.time() - t0) / 60
    report = res["message"]["content"]

    notes = []
    if truncated:
        notes.append("Article text was truncated to fit the context window; later sections were not seen.")
    if res.get("prompt_eval_count", 0) >= args.ctx - OUTPUT_RESERVE:
        notes.append(f"Prompt used {res['prompt_eval_count']} tokens of {args.ctx}; Ollama may have dropped the start.")
    if res.get("done_reason") == "length":
        notes.append("Report hit the output token limit and is cut off.")
    if rating_mismatch(report):
        notes.append(rating_mismatch(report))
    bad = unverified_functions(report, {r.split(" | ")[0] for r in catalog})
    if bad:
        notes.append("Coverage matrix names NOT in the module catalog (invented or R packages): "
                     + ", ".join(f"`{b}`" for b in bad))

    header = (f"<!-- local review: model={args.model} ctx={args.ctx} think={args.think} "
              f"prompt_tokens={res.get('prompt_eval_count')} output_tokens={res.get('eval_count')} "
              f"minutes={minutes:.1f} sources={', '.join(Path(f).name for f in args.files)} -->\n\n"
              f"> **Local-model draft.** Generated offline by `{args.model}` without citation verification, "
              f"literature lookup, or agent cross-checks. Verify before use.\n\n")
    if notes:
        header += "> ⚠️ **Automated checks**\n" + "".join(f"> - {n}\n" for n in notes) + "\n"
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(header + report + "\n", encoding="utf-8")
    print(f"wrote {out} ({minutes:.1f} min, {len(bad)} unverified function names)", file=sys.stderr)


def selftest():
    catalog = build_catalog()
    names = {r.split(" | ")[0] for r in catalog}
    assert len(catalog) > 100 and "crosstable" in names, "catalog scan failed"
    report = ("## 2. Methods\n`chisq.test`\n## 3. ClinicoPath coverage matrix\n"
              "| Kappa | `agreement`, `kappaMagic` | 🟡 | x |\n| Foo | none | ❌ | |\n## 4. Critical\n`bar`\n")
    assert unverified_functions(report, names | {"agreement"}) == ["kappaMagic"]
    text = "Intro\n" * 50 + "References\n1. Smith\n"
    assert "Smith" not in drop_references(text) and drop_references("References\nbody") == "References\nbody"
    assert "expected 🟡" in rating_mismatch("**Overall rating:** ✅ fine\n**Total score /18:** 11/18")
    assert rating_mismatch("**Overall rating:** 🟡 minor\nTotal: 11/18") is None
    print("selftest ok")


if __name__ == "__main__":
    main()
