#!/usr/bin/env python3
"""
Fetch a complete file inventory from https://library.jamovi.org/
across all OS folders and all R versions.

Outputs a CSV: jamovi_library_files.csv
- Columns: os, r_version, filename, url, size, last_modified

Usage:
  python fetch_jamovi_library.py           # all files
  python fetch_jamovi_library.py --pattern '\.jmo$'   # only .jmo files

All files
python fetch_jamovi_library.py

Only .jmo packages
python fetch_jamovi_library.py --pattern '\.jmo$' --out jamovi_packages.csv

Be extra polite to the server
python fetch_jamovi_library.py --delay 0.5

"""

import re
import csv
import time
import sys
import argparse
from urllib.parse import urljoin, urlparse
import requests
from bs4 import BeautifulSoup

BASE_URL = "https://library.jamovi.org/"

def _is_dir_link(href: str) -> bool:
    return href.endswith('/')

def _is_file_link(href: str) -> bool:
    return not href.endswith('/')

def _list_links(url: str):
    """Return (dirs, files) from a simple autoindex-like HTML page."""
    resp = requests.get(url, timeout=30)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "html.parser")

    dirs, files = [], []
    for a in soup.find_all('a', href=True):
        href = a['href']
        # Skip parent directory links, anchors
        if href in ('../', './') or href.startswith('#'):
            continue
        full = urljoin(url, href)
        # Heuristic: stay within host
        if urlparse(full).netloc != urlparse(BASE_URL).netloc:
            continue
        if _is_dir_link(href):
            dirs.append(full)
        else:
            files.append(full)

    # Try to extract size/last-modified if present in index tables (optional, best-effort)
    size_map, lm_map = {}, {}
    # Apache/NGINX autoindex often puts details near the link in the same row
    for row in soup.find_all('tr'):
        a = row.find('a', href=True)
        if not a:
            continue
        href = urljoin(url, a['href'])
        tds = row.find_all('td')
        text_cells = [td.get_text(strip=True) for td in tds]
        # very loose heuristic: look for something that looks like a date and/or size
        # we keep best-effort; if nothing matches, leave blank
        last_modified, size = "", ""
        # Commonly last-modified then size; adjust if your server differs
        if len(text_cells) >= 2:
            # try to detect a date-like token
            if re.search(r'\d{4}-\d{2}-\d{2}', " ".join(text_cells)):
                last_modified = re.search(r'\d{4}-\d{2}-\d{2}[^ ]*', " ".join(text_cells)).group(0)
            # find a size-like token (e.g., 12K, 1.2M, 12345)
            for cell in reversed(text_cells):
                if re.match(r'^\d+(\.\d+)?[KMG]?B?$', cell, flags=re.I):
                    size = cell
                    break
        size_map[href] = size
        lm_map[href] = last_modified

    return dirs, files, size_map, lm_map

def crawl(pattern: str, delay: float = 0.2):
    """
    Traverse:
      / -> [linux/, macos/, win64/]
      /{os}/ -> R-version dirs
      /{os}/{r_version}/ -> files
    Collect files matching regex `pattern` (default '.*' = all files).
    """
    rx = re.compile(pattern)
    results = []

    # Level 1: OS
    os_dirs, _, _, _ = _list_links(BASE_URL)
    os_dirs = [d for d in os_dirs if any(x in d for x in ('linux/', 'macos/', 'win64/'))]

    for os_dir in sorted(os_dirs):
        os_name = os_dir.rstrip('/').split('/')[-1]

        time.sleep(delay)
        r_dirs, _, _, _ = _list_links(os_dir)
        # Keep only R version folders (start with 'R' typically)
        r_dirs = [d for d in r_dirs if re.search(r'/R\d', d)]

        for r_dir in sorted(r_dirs):
            r_version = r_dir.rstrip('/').split('/')[-1]

            time.sleep(delay)
            subdirs, files, size_map, lm_map = _list_links(r_dir)

            # Some servers may nest further; if so, descend one more level
            to_scan = [(r_dir, files, size_map, lm_map)]
            if subdirs:
                for sd in subdirs:
                    time.sleep(delay)
                    _, files2, size_map2, lm_map2 = _list_links(sd)
                    to_scan.append((sd, files2, size_map2, lm_map2))

            for base, files_here, sm, lm in to_scan:
                for f in files_here:
                    fname = f.rstrip('/').split('/')[-1]
                    if rx.search(fname):
                        results.append({
                            "os": os_name,
                            "r_version": r_version,
                            "filename": fname,
                            "url": f,
                            "size": sm.get(f, ""),
                            "last_modified": lm.get(f, "")
                        })
    return results

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pattern", default=".*", help="Regex to filter files (e.g. '\\.jmo$')")
    ap.add_argument("--out", default="jamovi_library_files.csv", help="Output CSV path")
    ap.add_argument("--delay", type=float, default=0.2, help="Seconds between requests")
    args = ap.parse_args()

    rows = crawl(pattern=args.pattern, delay=args.delay)
    rows.sort(key=lambda r: (r["os"], r["r_version"], r["filename"]))

    with open(args.out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["os","r_version","filename","url","size","last_modified"])
        w.writeheader()
        w.writerows(rows)

    print(f"Wrote {len(rows)} rows to {args.out}")

if __name__ == "__main__":
    main()