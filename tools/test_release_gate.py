# Self-check for the release_gate.py rules taken from dev.jamovi.org (Distributing Modules,
# Module Datasets; added 2026-10-08): builds a throwaway module that breaks each rule once and
# asserts the gate notices. Run: python3 tools/test_release_gate.py
import os, sys, subprocess, tempfile, importlib.util

GATE = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'release_gate.py')


def write(root, path, text=''):
    p = os.path.join(root, path)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    open(p, 'w').write(text)


with tempfile.TemporaryDirectory() as m:
    subprocess.run(['git', 'init', '-q', m], check=True)
    write(m, 'DESCRIPTION', 'Package: fake\nLicense: AGPL-3\n')
    write(m, 'build/R4.6.0-x/badpkg/DESCRIPTION', 'License: AGPL-3 | file LICENSE\n')
    write(m, 'build/R4.6.0-x/okpkg/DESCRIPTION', 'License: GPL-2\n')
    write(m, 'dist/out.zip', 'x')
    for f in ('a.omv', 'b.omv', 'c.xlsx'):
        write(m, 'data/' + f)
    write(m, 'jamovi/0000.yaml', 'name: fake\ndatasets:\n'
          '  - {name: A, path: a.omv, description: first}\n'
          '  - {name: A, path: b.omv, description: second}\n'
          '  - {name: C, path: c.xlsx, description: third}\n'
          '  - {path: a.omv}\n')

    sys.argv = ['release_gate.py', '--root', m]
    spec = importlib.util.spec_from_file_location('release_gate', GATE)
    rg = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(rg)
    rg.check_license(); rg.check_artifacts(); rg.check_dataset_descriptions()
    fails, warns = '\n'.join(rg.FAIL), '\n'.join(rg.WARN)

    assert 'does not accept AGPL-3' in fails, fails                   # module's own licence
    assert 'badpkg' in warns and 'okpkg' not in warns, warns          # shipped AGPL dependency
    assert 'dist/out.zip' in fails, fails                             # dist/ is a build artifact
    assert "name 'A' used twice" in fails, fails                      # dataset name is unique
    assert 'c.xlsx is not .omv or .csv' in fails, fails               # dataset format
    assert 'lacks name/description' in fails, fails                   # required dataset fields
    print('OK: dev.jamovi.org distribution rules all fire')
