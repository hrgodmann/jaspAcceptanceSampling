// Execute the functions from Upgrades.qml rather than a second migration implementation.
// The surrounding ChangeRename/ChangeJS dispatch models the JASP Desktop upgrader;
// it does not replace native JASP save/reopen validation.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");

const source = fs.readFileSync(process.argv[2], "utf8");
const fixtures = JSON.parse(fs.readFileSync(process.argv[3], "utf8"));
const clone = value => JSON.parse(JSON.stringify(value));
const owns = (object, key) => object !== null && Object.prototype.hasOwnProperty.call(object, key);

// Find balanced QML/JS blocks while ignoring braces in strings and comments.
function endOfBlock(text, start) {
    let depth = 0;
    let quote = null;
    let comment = null;
    for (let index = start; index < text.length; index++) {
        const character = text[index];
        const next = text[index + 1];
        if (comment === "line") {
            if (character === "\n") comment = null;
        } else if (comment === "block") {
            if (character === "*" && next === "/") { comment = null; index++; }
        } else if (quote !== null) {
            if (character === "\\") index++;
            else if (character === quote) quote = null;
        } else if (character === "/" && next === "/") {
            comment = "line"; index++;
        } else if (character === "/" && next === "*") {
            comment = "block"; index++;
        } else if (character === '"' || character === "'") {
            quote = character;
        } else if (character === "{") {
            depth++;
        } else if (character === "}" && --depth === 0) {
            return index + 1;
        }
    }
    throw new Error("Unclosed QML/JavaScript block");
}

function blocks(text, types) {
    const pattern = new RegExp("\\b(" + types.join("|") + ")\\s*\\{", "g");
    const found = [];
    let match;
    while ((match = pattern.exec(text)) !== null) {
        const start = text.indexOf("{", match.index);
        const end = endOfBlock(text, start);
        found.push({type: match[1], body: text.slice(start + 1, end - 1)});
        pattern.lastIndex = end;
    }
    return found;
}

function property(text, name) {
    const match = new RegExp("\\b" + name + '\\s*:\\s*"([^"\\n]+)"').exec(text);
    assert.ok(match, "Missing property " + name);
    return match[1];
}

function script(text, name, fallback) {
    const match = new RegExp("\\b" + name + "\\s*:\\s*(function\\s*\\([^)]*\\)\\s*\\{)").exec(text);
    if (match === null) return fallback;
    const start = text.indexOf("function", match.index);
    const opening = text.indexOf("{", start);
    return vm.runInNewContext("(" + text.slice(start, endOfBlock(text, opening)) + ")");
}

const upgrades = blocks(source, ["Upgrade"]).map(block => ({
    analysis: property(block.body, "functionName"),
    from: property(block.body, "fromVersion"),
    to: property(block.body, "toVersion"),
    changes: blocks(block.body, ["ChangeRename", "ChangeJS"]).map(change => ({
        type: change.type,
        from: change.type === "ChangeRename" ? property(change.body, "from") : null,
        to: change.type === "ChangeRename" ? property(change.body, "to") : null,
        name: change.type === "ChangeJS" ? property(change.body, "name") : null,
        condition: script(change.body, "condition", () => true),
        transform: script(change.body, "jsFunction", null)
    }))
}));

function compareVersions(first, second) {
    const a = first.split(".").map(Number);
    const b = second.split(".").map(Number);
    for (let index = 0; index < Math.max(a.length, b.length); index++) {
        const difference = (a[index] || 0) - (b[index] || 0);
        if (difference !== 0) return difference;
    }
    return 0;
}

function applyChange(options, change) {
    if (change.type === "ChangeRename") {
        assert.ok(owns(options, change.from), "Missing old key " + change.from);
        assert.ok(!owns(options, change.to), "Colliding current key " + change.to);
        options[change.to] = options[change.from];
        delete options[change.from];
    } else {
        assert.ok(owns(options, change.name), "Missing transformed key " + change.name);
        options[change.name] = clone(change.transform(options));
    }
}

// JASP selects the first step whose fromVersion is >= the saved version and
// applies each successful change to both options and .meta. Metadata errors are
// ignored by the native upgrader. A step with toVersion equal to the running
// module version finishes the migration.
function migrate(analysis, version, original) {
    const options = clone(original);
    const step = upgrades.find(item => item.analysis === analysis && compareVersions(version, item.from) <= 0);
    if (step === undefined) return options;
    for (const change of step.changes) {
        if (!change.condition(options)) continue;
        applyChange(options, change);
        if (!owns(options, ".meta")) options[".meta"] = null;
        try { applyChange(options[".meta"], change); } catch (_) { /* Matches native metadata handling. */ }
    }
    return options;
}

assert.equal(upgrades.length, 5);
assert.equal(new Set(upgrades.map(step => step.analysis)).size, 5);
let checks = 0;
for (const fixture of fixtures) {
    const step = upgrades.find(item => item.analysis === fixture.analysis);
    assert.ok(step, fixture.analysis);
    assert.equal(step.from, "0.96.4");
    assert.equal(step.to, "0.96.5");
    for (const version of ["0.17.0", "0.95.5", "0.96.0", "0.96.3", "0.96.4"]) {
        assert.deepEqual(migrate(fixture.analysis, version, fixture.input), fixture.expected,
                         fixture.analysis + " from " + version);
        checks++;
    }
    // Current and future files do not accidentally re-enter this migration.
    for (const version of ["0.96.5", "0.97.0", "1.0.0"]) {
        assert.deepEqual(migrate(fixture.analysis, version, fixture.expected), fixture.expected);
        checks++;
    }
    assert.deepEqual(migrate(fixture.analysis, "0.96.4", fixture.expected), fixture.expected,
                     "Already-current keys are safe even with old metadata version");
    assert.deepEqual(migrate(fixture.analysis, "0.96.4", {}), {}, "Absent options stay absent");
    checks += 2;

    // A duplicate legacy key must not overwrite an explicitly retained current value.
    for (const change of step.changes.filter(change => change.type === "ChangeRename")) {
        const both = {[change.from]: 123, [change.to]: 0, ".meta": {[change.from]: {old: true}, [change.to]: {current: true}}};
        assert.deepEqual(migrate(fixture.analysis, "0.96.4", both), both,
                         "Current value wins: " + change.to);
        checks++;
    }
}

// The numeric row controls are genuinely nested, and the ComponentsList row
// identity (value) plus any unrelated row fields must survive unchanged.
const mixed = {multiplePlanStages: [
    {value: "1", sampleSizeMult: 99, sampleSize: 0, acceptNumberMult: 0, rejectNumberMult: 2, note: {keep: true}},
    {value: "2", sampleSizeMult: 31, acceptanceNumber: 3, rejectionNumber: 4},
    null
], ".meta": {multiplePlanStages: {containsColumn: false, descriptor: "do not interpret as rows"}}};
const mixedExpected = {multiplePlanStages: [
    {value: "1", sampleSize: 0, acceptanceNumber: 0, rejectionNumber: 2, note: {keep: true}},
    {value: "2", sampleSize: 31, acceptanceNumber: 3, rejectionNumber: 4},
    null
], ".meta": clone(mixed[".meta"])};
assert.deepEqual(migrate("AnalyzeAttributePlan", "0.96.4", mixed), mixedExpected);
checks++;

// Empty arrays and metadata arrays also keep their shape.
assert.deepEqual(migrate("AnalyzeAttributePlan", "0.96.4", {stages: [], ".meta": {stages: []}}),
                 {multiplePlanStages: [], ".meta": {multiplePlanStages: []}});
checks++;

for (const distribution of ["binom", "hypergeom", "poisson", "binomial", "hypergeometric", "futureDistribution"]) {
    const expected = {binom: "binomial", hypergeom: "hypergeometric"}[distribution] || distribution;
    assert.deepEqual(migrate("CreateAttributePlan", "0.96.4", {distribution, ".meta": {distribution: {type: "string"}}}),
                     {distribution: expected, ".meta": {distribution: {type: "string"}}});
    checks++;
}
assert.deepEqual(migrate("UnknownAnalysis", "0.96.0", {aql: 0.03}), {aql: 0.03});
checks++;
console.log("PASS: " + checks + " migration scenarios; 5 analyses, 85 key renames, nested stages, values and metadata.");
