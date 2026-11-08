const assert = require('node:assert/strict');
const test = require('node:test');
global._ = text => text;
const events = require('../../jamovi/js/agreement.events.js');

function panel(requestData) {
    const state = { enabled: true, raters: ['R1', 'R2'], positive: ' Pos ' };
    const element = { textContent: '', style: {}, setAttribute() {}, dispatchEvent() {} };
    Object.defineProperty(element, 'innerHTML', {
        set() { throw Error('Category labels must not be inserted as HTML'); }
    });
    const readOnly = value => ({ value, setValue() { throw Error('Option changed'); } });
    const ui = {
        vars: readOnly(() => state.raters),
        specificAgreement: readOnly(() => state.enabled),
        specificPositiveCategory: readOnly(() => state.positive),
        specificCategoryHint: { $el: [element] }
    };
    return { ui, state, element, context: { requestData } };
}

const column = labels => ({
    columnFound: true, measureType: 'nominal',
    levels: labels.map(label => ({ label }))
});

test('shows the union from every rater, preserving labels and user input', async () => {
    const calls = [];
    const p = panel((type, req) => {
        calls.push([type, req.columnName]);
        return Promise.resolve(column(req.columnName === 'R1' ? ['Neg', 'Pos'] : ['Pos', 'Other']));
    });
    await events.createCategoryHint.call(p.context, p.ui);
    assert.deepEqual(calls, [['column', 'R1'], ['column', 'R2']]);
    assert.equal(p.element.textContent, 'Available categories: Neg; Pos; Other');
    assert.equal(p.state.positive, ' Pos ');
    assert.equal(p.element.style.whiteSpace, 'normal');
});

test('excludes missing/filtered levels and continuous variables', async () => {
    const p = panel((type, req) => Promise.resolve(req.columnName === 'R1' ? {
        columnFound: true, measureType: 'ordinal', levels: [
            { label: 'Allowed' }, { label: 'Missing', treatAsMissing: true },
            { label: 'Excluded', filtered: true }, { label: null }
        ]
    } : { ...column(['1.5']), measureType: 'continuous' }));
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.equal(p.element.textContent, 'Available categories: Allowed');
});

test('renders HTML and replacement metacharacters as literal category text', async () => {
    const label = '<img src=x onerror=alert(1)> $& {total}';
    const p = panel(() => Promise.resolve(column([label])));
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.equal(p.element.textContent, 'Available categories: ' + label);
});

test('a late response cannot replace the newer selection', async () => {
    const pending = {};
    const p = panel((type, req) => new Promise(resolve => { pending[req.columnName] = resolve; }));
    p.state.raters = ['Old'];
    const old = events.refreshCategoryHint.call(p.context, p.ui);
    await Promise.resolve();
    p.state.raters = ['New'];
    const current = events.refreshCategoryHint.call(p.context, p.ui);
    await Promise.resolve();
    pending.New(column(['Current category']));
    await current;
    pending.Old(column(['Obsolete category']));
    await old;
    assert.equal(p.element.textContent, 'Available categories: Current category');
});

test('disabling the analysis clears a pending hint without changing options', async () => {
    let resolve;
    const p = panel(() => new Promise(done => { resolve = done; }));
    p.state.raters = ['R1'];
    const pending = events.refreshCategoryHint.call(p.context, p.ui);
    await Promise.resolve();
    p.state.enabled = false;
    await events.refreshCategoryHint.call(p.context, p.ui);
    resolve(column(['Late category']));
    await pending;
    assert.match(p.element.textContent, /^Enable specific agreement/);
    assert.equal(p.state.positive, ' Pos ');
});

test('a level change refreshes the hint, without reacting to unrelated cell events', async () => {
    let labels = ['Before']; let calls = 0;
    const p = panel(() => { calls++; return Promise.resolve(column(labels)); });
    p.state.raters = ['R1'];
    await events.refreshCategoryHint.call(p.context, p.ui);
    labels = ['After'];
    await events.onRemoteDataChanged.call(p.context, p.ui, { dataType: 'columns' });
    assert.equal(p.element.textContent, 'Available categories: After');
    await events.onRemoteDataChanged.call(p.context, p.ui, { dataType: 'cells' });
    assert.equal(calls, 2);
});

test('handles empty selection, absent columns and metadata failures', async () => {
    const p = panel(() => Promise.resolve({ columnFound: false }));
    p.state.raters = [];
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.match(p.element.textContent, /^Select rater variables/);
    p.state.raters = ['Gone'];
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.match(p.element.textContent, /^No categories available/);
    p.context.requestData = () => { throw Error('offline'); };
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.match(p.element.textContent, /^Categories could not be loaded/);
});

test('bounds long lists and retains the total category count', async () => {
    const p = panel(() => Promise.resolve(column(Array.from({ length: 50 }, (_, i) => 'C' + i))));
    await events.refreshCategoryHint.call(p.context, p.ui);
    assert.match(p.element.textContent, /^Available categories \(first 30 of 50\):/);
    assert.match(p.element.textContent, /C29$/);
    assert.doesNotMatch(p.element.textContent, /C30/);
});
