#!/usr/bin/env python3
"""Regenerate the cross-agent referral files from the frontmatter of every playbook.

The playbooks under .claude/ are the single source of truth. Everything this script
writes is a pointer back to them, so adding a playbook means running this script
rather than editing N files by hand.

Outputs (both gitignored — see below):
  .claude/PLAYBOOKS.md                      the human/agent-readable index
  .agents/skills/jamovi-playbooks/SKILL.md  referral skill for non-Claude agents

Both outputs are local-only, because they index `.claude/skills/` which is itself
gitignored by choice. Committing them would ship a routing table whose links resolve
to nothing on a fresh clone — the silent-failure mode this script exists to avoid.
What IS committed: `.claude/commands/`, plus AGENTS.md / GEMINI.md pointing at it.
Run this script on any machine that has local skills to get the full index.

Why `.agents/skills/`: it is the cross-tool convention for a directory of SKILL.md
files — Codex CLI, Gemini CLI, Cursor and Copilot all scan it. Cursor and Copilot
*also* scan `.claude/skills/` natively, which is why this emits ONE aggregate
referral skill rather than a stub per playbook: per-playbook stubs would show up
twice, under the same `name`, in exactly those two tools.

Why not symlinks: Gemini CLI does not read a symlinked GEMINI.md (google-gemini/
gemini-cli#11547, closed not-planned) or traverse symlinked skills dirs (#16247),
and on a Windows checkout without core.symlinks git materialises a symlink as a
plain text file containing the target path — which fails silently, with the agent
loading no instructions at all and nothing reporting an error.

Usage:  python3 .claude/scripts/build_playbook_index.py [--check]
        --check exits 1 if any generated file is stale (for CI / pre-commit).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

CLAUDE_DIR = Path(__file__).resolve().parent.parent
REPO_ROOT = CLAUDE_DIR.parent
INDEX = CLAUDE_DIR / "PLAYBOOKS.md"
REFERRAL_SKILL = REPO_ROOT / ".agents" / "skills" / "jamovi-playbooks" / "SKILL.md"

FRONTMATTER = re.compile(r"\A---\r?\n(.*?)\r?\n---\r?\n", re.DOTALL)


def parse_frontmatter(text: str) -> dict[str, str]:
    """Pull top-level scalar keys out of YAML frontmatter.

    Deliberately not using PyYAML: these files are hand-written and a few carry
    descriptions with unescaped colons and backticks that trip strict parsing.
    Only top-level `key: value` pairs matter here, so a line scanner is both
    sufficient and far less likely to fail on a file it should merely index.
    """
    match = FRONTMATTER.match(text)
    if not match:
        return {}

    fields: dict[str, str] = {}
    key: str | None = None
    for line in match.group(1).splitlines():
        top_level = re.match(r"^([A-Za-z_][A-Za-z0-9_-]*):\s?(.*)$", line)
        if top_level:
            key, value = top_level.group(1), top_level.group(2).strip()
            fields[key] = value
        elif key and line.startswith((" ", "\t")) and fields.get(key) == "":
            # Nested block (args:, examples:) — not needed for the index.
            continue
        elif key and line.startswith((" ", "\t")):
            fields[key] += " " + line.strip()
    return fields


def first_sentence(text: str, limit: int = 240) -> str:
    """Condense a long triggering description into one table-sized line."""
    text = " ".join(text.split()).strip().strip("\"'")
    # Skills lead with boilerplate that reads badly in a table.
    text = re.sub(r"^This skill should be used when the user asks to\s*", "Use when asked to ", text)
    text = text.replace("|", "\\|")
    for end in (". ", "; ", " — ", " -- "):
        head = text.split(end)[0]
        if 40 <= len(head) <= limit:
            return head.rstrip(".")
    if len(text) <= limit:
        return text
    # Long trigger-phrase lists have no sentence break; cut on a word boundary so the
    # table never ends mid-word.
    cut = text[:limit]
    for boundary in ('", "', ", ", " "):
        idx = cut.rfind(boundary)
        if idx > 40:
            return cut[:idx].rstrip(" ,\"") + ', ...'
    return cut.rstrip() + ", ..."


def collect() -> tuple[list[dict], list[dict]]:
    commands = []
    for path in sorted((CLAUDE_DIR / "commands").glob("*.md")):
        fm = parse_frontmatter(path.read_text(encoding="utf-8"))
        commands.append(
            {
                "name": fm.get("name") or path.stem,
                "desc": first_sentence(fm.get("description", "")),
                "usage": fm.get("usage", ""),
                "path": path.relative_to(REPO_ROOT).as_posix(),
            }
        )

    skills = []
    for path in sorted((CLAUDE_DIR / "skills").glob("*/SKILL.md")):
        fm = parse_frontmatter(path.read_text(encoding="utf-8"))
        skills.append(
            {
                "name": fm.get("name") or path.parent.name,
                "desc": first_sentence(fm.get("description", "")),
                "path": path.relative_to(REPO_ROOT).as_posix(),
            }
        )
    return commands, skills


HEADER = """<!-- GENERATED FILE - do not edit by hand.
     Regenerate: python3 .claude/scripts/build_playbook_index.py
     Source of truth: the frontmatter of each file listed below. -->

# Jamovi development playbooks

This repository keeps its task-specific playbooks under `.claude/`. They are plain
markdown with YAML frontmatter, so **any** coding agent can use them — not just Claude
Code. Nothing here depends on a Claude-specific runtime.

## How to use these from any agent

1. Match the user's request against the tables below.
2. Read the whole file at the listed path before acting on it. The frontmatter
   `description` is a routing hint; the body is the actual procedure.
3. Follow the procedure. Where a playbook says `$ARGUMENTS`, substitute the target the
   user named (usually one jamovi analysis, e.g. `singlearm`).
4. Translate Claude-Code-specific vocabulary as you read:

   | Playbook says | Means |
   |---|---|
   | "the Skill tool" / "invoke the skill" | read that file and follow it |
   | `/check-function <fn>` | run the procedure in `.claude/commands/check-function.md` |
   | "subagent" / "Task tool" | do the work yourself, or use your own parallel-agent facility |
   | `TodoWrite` | your own task-tracking mechanism, or a plain checklist |

5. `CLAUDE.md` holds the repository-wide rules (architecture, generated-file policy,
   known pitfalls). Read it too — it applies regardless of which agent you are.

"""

FOOTER = """
## Non-negotiables that apply to every playbook

- `R/*.h.R`, `man/*.Rd`, `jamovi/0000.yaml`, and `NAMESPACE` are **generated**. Never edit
  them. Change `jamovi/<fn>.a.yaml` / `.r.yaml` / `.u.yaml` or `R/<fn>.b.R`, then regenerate
  with `jmvtools::prepare()` and `devtools::document()`.
- `jmvtools::prepare()` must finish with no errors or warnings, or the module will not load
  in jamovi.
- `jmvtools::check()` only locates the jamovi binary. It validates nothing about your code.
- `README.md` is generated from `README.Rmd`.
"""


def link(repo_path: str) -> str:
    """Render a path as repo-root text with an href that also works on GitHub.

    Agents need the repo-root-relative path (that is what they will open), but this
    index lives in .claude/, so the markdown href has to be relative to that folder
    for the rendered page to have working links. Show one, link the other.
    """
    href = repo_path[len(".claude/"):] if repo_path.startswith(".claude/") else "../" + repo_path
    return f"[`{repo_path}`]({href})"


def render(commands: list[dict], skills: list[dict]) -> str:
    out = [HEADER]

    out.append("## Skills\n")
    out.append("Broader, multi-step procedures. Read the whole file.\n")
    out.append("| Skill | Use it when | File |")
    out.append("|---|---|---|")
    for s in skills:
        out.append(f"| `{s['name']}` | {s['desc']} | {link(s['path'])} |")
    out.append("")

    out.append("## Commands\n")
    out.append(
        "Single-purpose procedures, usually scoped to one analysis. In Claude Code these are "
        "slash commands; from any other agent, read the file and follow it.\n"
    )
    out.append("| Command | Purpose | File |")
    out.append("|---|---|---|")
    for c in commands:
        out.append(f"| `/{c['name']}` | {c['desc']} | {link(c['path'])} |")
    out.append("")

    out.append("## Reference guides\n")
    out.append(
        "The playbooks lean on the guides in `vignettes/`. Start with "
        + link("vignettes/jamovi_module_patterns_guide.md")
        + "; "
        + link("vignettes/README_GUIDES.md")
        + " indexes the rest. Official jamovi documentation is vendored at "
        "`development-documentations-dev.jamovi.org-master/` (an old snapshot — its options "
        "reference covers only Data, Bool, Integer, Number, List, Variable, Variables).\n"
    )

    out.append(FOOTER)
    return "\n".join(out).rstrip() + "\n"


REFERRAL_DESCRIPTION = (
    "Routing table for this repository's jamovi development playbooks. Use whenever the task "
    "touches a jamovi analysis in this module - creating a new analysis, reviewing or fixing an "
    "existing one, editing any .a.yaml / .u.yaml / .r.yaml / .b.R file, adding user notices, "
    "generating test data, writing docs or translations, auditing for security, checking "
    "statistical or clinical correctness, or deciding whether an analysis is ready to release. "
    "It maps the request to the right playbook under .claude/ and states the non-negotiable rules "
    "(never edit generated .h.R / .Rd / 0000.yaml / NAMESPACE). Read it before editing module "
    "files, not after - the playbooks encode failure modes that are invisible in the source and "
    "only surface at runtime in jamovi."
)


def render_referral_skill(commands: list[dict], skills: list[dict]) -> str:
    """Emit the aggregate referral skill for agents that scan `.agents/skills/`.

    Kept deliberately thin: the routing table lives here so an agent can pick the right
    playbook in one read, but every procedure stays in .claude/ so there is only ever one
    copy to maintain.
    """
    rows = [f"| `{s['name']}` | {s['desc']} | `{s['path']}` |" for s in skills]
    rows += [f"| `/{c['name']}` | {c['desc']} | `{c['path']}` |" for c in commands]

    return f"""---
name: jamovi-playbooks
description: {REFERRAL_DESCRIPTION}
---

<!-- GENERATED FILE - do not edit by hand.
     Regenerate: python3 .claude/scripts/build_playbook_index.py
     Source of truth: the playbooks under .claude/ listed below. -->

# Jamovi development playbooks — routing table

This skill holds no procedures of its own. The procedures live under `.claude/`, which is the
single source of truth for this repository; this file exists so agents that scan
`.agents/skills/` can find them. Paths below are relative to the repository root.

## How to use this

1. Match the request against the table.
2. **Read the whole file at that path** before acting. The `Use it when` column is only a routing
   hint — the body is the actual procedure, and it is long because the details matter.
3. Where a playbook writes `$ARGUMENTS`, substitute the target the user named (usually one
   analysis, e.g. `singlearm`). Where it says "the Skill tool", "invoke the skill", or
   `/some-command`, that just means: read the corresponding file and follow it.
4. Also read `CLAUDE.md`. Despite the name it is repository-wide guidance — architecture, the
   generated-file policy, and a long list of pitfalls — and applies to every agent.
5. The full index, with more context, is at `.claude/PLAYBOOKS.md`.

| Playbook | Use it when | Path |
|---|---|---|
{chr(10).join(rows)}

## Rules that override anything else you might infer

- `R/*.h.R`, `man/*.Rd`, `jamovi/0000.yaml` and `NAMESPACE` are **generated**. Never edit them.
  Change `jamovi/<fn>.a.yaml` / `.r.yaml` / `.u.yaml` or `R/<fn>.b.R`, then regenerate with
  `jmvtools::prepare()` and `devtools::document()`.
- `jmvtools::prepare()` must finish with no errors or warnings, or the module will not load in
  jamovi. `jmvtools::check()` only locates the jamovi binary — it validates nothing.
- `README.md` is generated from `README.Rmd`.
- Reference guides live in `vignettes/`; start with `vignettes/jamovi_module_patterns_guide.md`.
"""


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true", help="exit 1 if the index is stale")
    args = ap.parse_args()

    commands, skills = collect()
    if not commands and not skills:
        print("no playbooks found — is .claude/ present?", file=sys.stderr)
        return 1

    outputs = {
        INDEX: render(commands, skills),
        REFERRAL_SKILL: render_referral_skill(commands, skills),
    }

    if args.check:
        stale = [
            path
            for path, content in outputs.items()
            if not path.exists() or path.read_text(encoding="utf-8") != content
        ]
        if stale:
            for path in stale:
                print(f"{path.relative_to(REPO_ROOT)} is stale — regenerate it", file=sys.stderr)
            return 1
        print(f"up to date: {', '.join(str(p.relative_to(REPO_ROOT)) for p in outputs)}")
        return 0

    for path, content in outputs.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        print(f"wrote {path.relative_to(REPO_ROOT)}")
    print(f"indexed {len(skills)} skills, {len(commands)} commands")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
