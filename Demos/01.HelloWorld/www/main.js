showModule('core');

function log(message) {
    const textarea = document.getElementById("textarea");
    textarea.value += '------------------------------------\n' + message + '\n';
}

async function showModule(name) {
    // Load HTML content
    const container = document.getElementById('container');
    container.innerHTML = await (await fetch(name + '.html')).text();

    // Execute scripts in the loaded HTML
    container.querySelectorAll('script').forEach(old => {
        const script = document.createElement('script');
        script.textContent = old.textContent;
        if (old.src) script.src = old.src;
        old.replaceWith(script);
    });
}
