#!/usr/bin/env python3
"""
Download all of *your* jamovi modules from https://library.jamovi.org/
across all OS folders (linux, macos, win64) and all R versions.

By default this script:
  - Crawls only for your packages (.jmo files):
      ClinicoPath, ClinicoPathDescriptives, meddecide, jsurvival, jjstatsplot, OncoPath
  - Waits between requests (polite crawling)
  - Downloads sequentially (concurrency=1) to avoid overwhelming the server
  - Skips files that already exist with the expected size (when Content-Length is available)

You can optionally restrict to the *latest* version per (os, r_version, package).

USAGE EXAMPLES
--------------
# Dry-run (show what would be downloaded), latest version only
python download_jamovi_ClinicoPath_modules.py --latest-only --dry-run

# Sync: only missing or newer than local copies
python download_jamovi_ClinicoPath_modules.py --sync --latest-only

# Download everything (all versions) with a 0.6s delay between requests
python download_jamovi_ClinicoPath_modules.py --delay 0.6

# Download to a custom folder
python download_jamovi_ClinicoPath_modules.py --out jamovi_modules

# Retry settings
python download_jamovi_ClinicoPath_modules.py --retries 3 --timeout 30

python download_jamovi_ClinicoPath_modules.py --latest-only --dry-run
python download_jamovi_ClinicoPath_modules.py --latest-only --delay 0.5
python download_jamovi_ClinicoPath_modules.py --delay 0.6
python download_jamovi_ClinicoPath_modules.py --out jamovi_modules --retries 3 --timeout 30



Usage examples

- Only latest and only new/updated (dry-run):
    - python vignettes/download_jamovi_ClinicoPath_modules.py --latest-only --sync --dry-run
- Sync latest into a target folder:
    - python vignettes/download_jamovi_ClinicoPath_modules.py --latest-only --sync --out jamovi_modules
- Sync all versions:
    - python vignettes/download_jamovi_ClinicoPath_modules.py --sync
- Tuning:
    - --delay 0.6 polite pacing
    - --retries 3 --timeout 30 robustness

If you want to pull all versions (not just latest) and still only fetch missing/newer, run:

- python vignettes/download_jamovi_ClinicoPath_modules.py --sync --dry-run

If you want to actually download only the missing/newer files (none at the moment), run:

- python vignettes/download_jamovi_ClinicoPath_modules.py --sync

Or if you prefer to keep syncing only the latest per OS/R combo:

- python vignettes/download_jamovi_ClinicoPath_modules.py --latest-only --sync

DEPENDENCIES
------------
- Python 3.8+
- Standard library only (no external packages needed)
"""

import argparse
import os
import re
import sys
import time
import urllib.request
import urllib.parse
from html.parser import HTMLParser
from urllib.error import URLError, HTTPError

BASE = "https://library.jamovi.org/"
TARGET_OS = ("linux/", "macos/", "win64/")
PACKAGE_PREFIXES = (
    "ClinicoPath",
    "ClinicoPathDescriptives",
    "meddecide",
    "jsurvival",
    "jjstatsplot",
    "OncoPath",
)
PKG_REGEX = re.compile(
    r"^(ClinicoPath|ClinicoPathDescriptives|meddecide|jsurvival|jjstatsplot|OncoPath)-.*\.jmo$",
    re.IGNORECASE,
)
USER_AGENT = "Jamovi-Module-Fetcher/1.0 (+polite; contact: local user)"
DEFAULT_DELAY = 0.5


class LinkParser(HTMLParser):
    def __init__(self, base_url):
        super().__init__()
        self.base_url = base_url
        self.links = []

    def handle_starttag(self, tag, attrs):
        if tag.lower() != "a":
            return
        href = None
        for k, v in attrs:
            if k.lower() == "href":
                href = v
                break
        if not href:
            return
        if href in ("../", "./") or href.startswith("#"):
            return
        full = urllib.parse.urljoin(self.base_url, href)
        # stay within host
        if urllib.parse.urlparse(full).netloc != urllib.parse.urlparse(BASE).netloc:
            return
        self.links.append(full)


def fetch_html_links(url, timeout, headers):
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        content_type = resp.headers.get("Content-Type", "")
        html_bytes = resp.read()
    parser = LinkParser(url)
    try:
        html_text = html_bytes.decode("utf-8", errors="replace")
    except Exception:
        html_text = html_bytes.decode(errors="replace")
    parser.feed(html_text)
    return parser.links


def list_dirs_and_files(url, timeout, headers):
    """Return (dirs, files) for a simple index page."""
    links = fetch_html_links(url, timeout, headers)
    dirs = [l for l in links if l.endswith("/")]
    files = [l for l in links if not l.endswith("/")]
    return dirs, files


def version_tuple_from_filename(name):
    """
    Extract a version tuple from filenames like 'jsurvival-0.0.3.90.jmo' -> (0,0,3,90)
    If no version found, return empty tuple to place it lower in comparisons.
    """
    m = re.search(r"-([0-9]+(?:\.[0-9]+)*)\.jmo$", name)
    if not m:
        return tuple()
    parts = m.group(1).split(".")
    try:
        return tuple(int(p) for p in parts)
    except ValueError:
        return tuple()


def polite_sleep(delay):
    if delay > 0:
        time.sleep(delay)


def http_head(url, timeout, headers):
    # Not all servers allow HEAD; if it fails, we just return None
    try:
        req = urllib.request.Request(url, method="HEAD", headers=headers)
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return resp.headers
    except Exception:
        return None


def needs_download(dest_path, url, timeout, headers):
    """
    Decide if a download is needed for sync purposes.
    - If file missing -> need = True
    - Else try HEAD: if Last-Modified present, compare to local mtime.
      If remote is newer -> need = True.
      Else if Content-Length present and equals local size -> need = False.
      Else -> need = True (safer to refresh).
    Returns (need: bool, reason: str)
    """
    if not os.path.exists(dest_path):
        return True, "missing"

    head = http_head(url, timeout, headers)
    if not head:
        return False, "exists-no-head"

    # Compare Last-Modified (if present)
    lm = head.get("Last-Modified") or head.get("last-modified")
    try:
        if lm:
            # Parse HTTP-date; fall back safely if parsing fails
            from email.utils import parsedate_to_datetime

            remote_dt = parsedate_to_datetime(lm)
            local_mtime = os.path.getmtime(dest_path)
            import datetime

            local_dt = datetime.datetime.fromtimestamp(local_mtime, tz=remote_dt.tzinfo)
            if remote_dt > local_dt:
                return True, "remote-newer"
            # if remote older or equal, no need to download further
            # still consider size mismatch as potential corruption
    except Exception:
        pass

    # Compare Content-Length vs local size
    cl = head.get("Content-Length") or head.get("content-length")
    if cl and cl.isdigit():
        expected = int(cl)
        local_size = os.path.getsize(dest_path)
        if local_size == expected:
            return False, "up-to-date"
        else:
            return True, "size-changed"

    # No reliable metadata; keep existing file
    return False, "exists"


def filter_missing_or_newer(entries, out_base, timeout, headers, verbose=True, dry_run=False):
    """Filter entries to only those missing locally or with newer remote timestamps.
    Uses HEAD Last-Modified/Content-Length heuristics.
    """
    kept = []
    skipped = 0
    for os_name, r_version, fname, url in entries:
        dest_dir = os.path.join(out_base, os_name, r_version)
        dest_path = os.path.join(dest_dir, fname)
        need, reason = needs_download(dest_path, url, timeout, headers)
        if need:
            kept.append((os_name, r_version, fname, url))
            if verbose:
                print(f"[sync-need] {reason}: {url}")
        else:
            skipped += 1
            if verbose:
                print(f"[sync-skip] {reason}: {dest_path}")
        polite_sleep(0.05)  # tiny pause to avoid rapid-fire HEADs
    if verbose:
        print(f"[sync] keeping {len(kept)} entries; skipping {skipped} already up-to-date")
    return kept


def download_file(url, dest_path, timeout, headers, retries, delay, dry_run=False):
    # Ensure directory exists
    os.makedirs(os.path.dirname(dest_path), exist_ok=True)

    # Try to get expected size
    expected_size = None
    head = http_head(url, timeout, headers)
    if head:
        cl = head.get("Content-Length")
        if cl and cl.isdigit():
            expected_size = int(cl)

    # Skip if exists & size matches
    if os.path.exists(dest_path) and expected_size is not None:
        local_size = os.path.getsize(dest_path)
        if local_size == expected_size:
            print(f"[skip] {dest_path} (already downloaded)")
            return True

    if dry_run:
        print(f"[dry-run] would download -> {url} -> {dest_path}")
        return True

    attempt = 0
    while True:
        attempt += 1
        try:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=timeout) as resp, open(dest_path, "wb") as out:
                # Stream download in chunks
                chunk = resp.read(1024 * 64)
                while chunk:
                    out.write(chunk)
                    chunk = resp.read(1024 * 64)
            print(f"[ok] {dest_path}")
            return True
        except (HTTPError, URLError) as e:
            print(f"[warn] download failed (attempt {attempt}/{retries}): {url} -> {e}")
            if attempt >= retries:
                print(f"[fail] giving up on: {url}")
                return False
            polite_sleep(min(2.0 * attempt, 5.0))  # small backoff
        finally:
            polite_sleep(delay)


def crawl_my_modules(delay, timeout, headers):
    # Level 1: OS
    os_dirs, _ = list_dirs_and_files(BASE, timeout, headers)
    os_dirs = [d for d in os_dirs if any(x in d for x in TARGET_OS)]

    results = []  # (os, r_version, filename, url)

    for os_dir in sorted(os_dirs):
        os_name = os_dir.rstrip("/").split("/")[-1]

        polite_sleep(delay)
        r_dirs, _ = list_dirs_and_files(os_dir, timeout, headers)
        r_dirs = [d for d in r_dirs if "/R" in d]

        for r_dir in sorted(r_dirs):
            r_version = r_dir.rstrip("/").split("/")[-1]

            polite_sleep(delay)
            subdirs, files = list_dirs_and_files(r_dir, timeout, headers)

            all_files = list(files)
            # Some R folders might include an extra level (rare); include them politely
            for sd in subdirs:
                polite_sleep(delay)
                _, files2 = list_dirs_and_files(sd, timeout, headers)
                all_files.extend(files2)

            for f in all_files:
                fname = f.rsplit("/", 1)[-1]
                if PKG_REGEX.search(fname):
                    results.append((os_name, r_version, fname, f))

    return results


def pick_latest_per_tuple(entries):
    """Keep only the highest version for each (os, r_version, package-stem)."""
    keep = {}
    for os_name, r_version, fname, url in entries:
        # package stem = part before first hyphen
        stem = fname.split("-", 1)[0].lower()
        key = (os_name, r_version, stem)
        vt = version_tuple_from_filename(fname)
        cur = keep.get(key)
        if (cur is None) or (vt > cur[0]):
            keep[key] = (vt, (os_name, r_version, fname, url))
    return [v[1] for v in keep.values()]


def main():
    ap = argparse.ArgumentParser(description="Download your jamovi modules from library.jamovi.org politely.")
    ap.add_argument("--out", default="jamovi_modules", help="Output base folder")
    ap.add_argument("--delay", type=float, default=DEFAULT_DELAY, help="Delay (seconds) between requests")
    ap.add_argument("--timeout", type=int, default=25, help="Per-request timeout (seconds)")
    ap.add_argument("--retries", type=int, default=3, help="Retries per file")
    ap.add_argument("--latest-only", action="store_true", help="Only download latest version per (os, r_version, package)")
    ap.add_argument("--sync", action="store_true", help="Only download files that are missing or newer than local copies (uses Last-Modified/Content-Length)")
    ap.add_argument("--dry-run", action="store_true", help="List actions without downloading")
    args = ap.parse_args()

    headers = {"User-Agent": USER_AGENT}

    print("[info] Crawling for your modules...")
    entries = crawl_my_modules(delay=args.delay, timeout=args.timeout, headers=headers)
    if args.latest_only:
        entries = pick_latest_per_tuple(entries)

    # Sort for stable output
    entries.sort(key=lambda t: (t[0], t[1], t[2]))

    if not entries:
        print("[info] No matching modules found.")
        return 0

    # Optional sync filter: keep only missing/newer files
    if args.sync:
        entries = filter_missing_or_newer(entries, args.out, args.timeout, headers, verbose=True, dry_run=args.dry_run)

    print(f"[info] Found {len(entries)} files to process.")
    for os_name, r_version, fname, url in entries:
        dest_dir = os.path.join(args.out, os_name, r_version)
        dest_path = os.path.join(dest_dir, fname)
        ok = download_file(
            url=url,
            dest_path=dest_path,
            timeout=args.timeout,
            headers=headers,
            retries=max(1, args.retries),
            delay=args.delay,
            dry_run=args.dry_run,
        )
        if not ok:
            # continue to next file; we don't abort entire run
            pass

    print("[done]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
