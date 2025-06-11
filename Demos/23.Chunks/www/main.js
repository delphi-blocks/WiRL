let evtSource;

async function handleSSEClick() {
    evtSource = new EventSource("http://localhost:8080/rest/app/streaming/sse");
    const output = document.getElementById('output');
    output.value = '';

    evtSource.onmessage = (event) => {
        output.value = output.value + event.data + '\n';
        output.scrollTop = output.scrollHeight;
    };

    evtSource.addEventListener("ping", (event) => {
        output.value = output.value + event.data + '\n';
        output.scrollTop = output.scrollHeight;
    });

    evtSource.onerror = (err) => {
        output.value = output.value + 'CONNECTION CLOSED BY THE SERVER!' + '\n';
        output.scrollTop = output.scrollHeight;
    };    
}

async function handleChunkClick() {
    const output = document.getElementById('output');
    output.value = '';
    //const response = await fetch('http://127.0.0.1:5000/api/stream');
    //const response = await fetch('http://localhost:5005/');
    const response = await fetch('http://localhost:8080/rest/app/streaming/chunks');

    const reader = response.body.getReader();
    const decoder = new TextDecoder();

    while (true) {
        const { value, done } = await reader.read();
        if (done) {
            break;
        }

        // Converte il chunk in testo
        const chunk = decoder.decode(value);

        // Ogni riga è un JSON separato
        const lines = chunk.split('\n');

        for (const line of lines) {
            if (line.trim() === '') continue;

            output.value = output.value + line + '\n';
            output.scrollTop = output.scrollHeight;
        }
    }
    output.value = output.value + 'DONE\n';
}

function handleSSEStopClick() {
    if (evtSource) {
        evtSource.close();
        evtSource = null;
    }
}