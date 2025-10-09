#!/usr/bin/env python3
# save as fetch_jamovi_mine.py
import re, csv, time
from urllib.parse import urljoin, urlparse
import requests
from bs4 import BeautifulSoup

BASE = "https://library.jamovi.org/"
# case-insensitive, anchors so we only match package stems
RX = re.compile(r'^(ClinicoPath|ClinicoPathDescriptives|meddecide|jsurvival|jjstatsplot|OncoPath)-.*\.jmo$', re.I)

def _list(url):
    r = requests.get(url, timeout=30); r.raise_for_status()
    s = BeautifulSoup(r.text, "html.parser")
    dirs, files = [], []
    for a in s.find_all("a", href=True):
        h = a["href"]
        if h in ("../","./") or h.startswith("#"): continue
        full = urljoin(url, h)
        if urlparse(full).netloc != urlparse(BASE).netloc: continue
        (dirs if h.endswith("/") else files).append(full)
    return dirs, files

rows = []
os_dirs, _ = _list(BASE)
os_dirs = [d for d in os_dirs if any(k in d for k in ("linux/","macos/","win64/"))]

for os_dir in sorted(os_dirs):
    os_name = os_dir.rstrip("/").split("/")[-1]
    time.sleep(0.2)
    r_dirs, _ = _list(os_dir)
    r_dirs = [d for d in r_dirs if "/R" in d]

    for r_dir in sorted(r_dirs):
        r_ver = r_dir.rstrip("/").split("/")[-1]
        time.sleep(0.2)
        subdirs, files = _list(r_dir)

        # collect files at this level
        all_files = list(files)
        # descend one level if needed
        for sd in subdirs:
            time.sleep(0.2)
            _, ff = _list(sd)
            all_files.extend(ff)

        for f in all_files:
            name = f.rsplit("/",1)[-1]
            if RX.search(name):
                rows.append({"os": os_name, "r_version": r_ver, "filename": name, "url": f})

rows.sort(key=lambda r: (r["os"], r["r_version"], r["filename"]))
with open("jamovi_ClinicoPath_modules.csv","w",newline="",encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["os","r_version","filename","url"])
    w.writeheader(); w.writerows(rows)

print(f"Wrote {len(rows)} rows to jamovi_ClinicoPath_modules.csv")