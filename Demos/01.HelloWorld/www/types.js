async function stringTest() {
    const result = await fetch('/rest/app/params/str/lorem%20ipsum');
    const data = await result.text();
    log('Expected: lorem ipsum\nGot: ' + data);
}

async function integerTest() {
    const result = await fetch('/rest/app/params/int/12');
    const data = await result.text();
    log('Expected: 13\nGot: ' + data);
}

async function floatTest() {
    const result = await fetch('/rest/app/params/float/1.12');
    const data = await result.text();
    log('Expected: 0.112\nGot: ' + data);
}

async function booleanTest() {
    const result = await fetch('/rest/app/params/bool/true');
    const data = await result.text();
    log('Expected: false\nGot: ' + data);
}

async function enumTest() {
    const result = await fetch('/rest/app/params/enum/Second');
    const data = await result.text();
    log('Expected: First\nGot: ' + data);
}

async function simpleObjectTest() {
    const result = await fetch('/rest/app/params/object/prefix.value');
    const data = await result.text();
    log('Expected: Prefix: [prefix] - Value [value]\nGot: ' + data);
}

async function dateTest() {
    const result = await fetch('/rest/app/params/date/2025-09-30');
    const data = await result.text();
    log('Expected: 2026-09-30\nGot: ' + data);
}

async function dateTimeTest() {
    const result = await fetch('/rest/app/params/datetime/2025-09-30T14:30:00Z');
    const data = await result.text();
    log('Expected: 2025-10-01T02:30:00.000Z\nGot: ' + data);
}