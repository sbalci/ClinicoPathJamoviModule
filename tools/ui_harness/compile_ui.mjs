// usage: node compile_ui.mjs <repo> <basename> <outdir>  (used by render_ui.sh)
import path from 'path';
import fs from 'fs';
const JC = '/Library/Frameworks/R.framework/Versions/4.6/Resources/library/jmvtools/node_modules/jamovi-compiler';
const uicompile = (await import(JC + '/uicompiler.js')).default;
const [repo, base, outdir] = process.argv.slice(2);
const tmp = path.join(outdir, base + '.u.yaml.tmp');   // uicompile may rewrite the .u.yaml in place; work on a copy
fs.copyFileSync(path.join(repo, 'jamovi', base + '.u.yaml'), tmp);
uicompile(path.join(repo, 'jamovi', base + '.a.yaml'), tmp, path.join(repo, 'jamovi/js', base + '.js'), base, JC + '/src.template', path.join(outdir, base + '.js'));
fs.unlinkSync(tmp);
console.log('compiled', base);
