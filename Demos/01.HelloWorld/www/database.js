async function getData() {
    const result = await fetch('/rest/app/database/db');
    const data = await result.json();
    const table = data.map(item => `<tr><td>${item.id}</td><td>${item.value}</td></tr>`).join('\n');
    document.getElementById('table-body').innerHTML = table;
}

