async function echoText() {
    try {
        const result = await fetch('/rest/app/helloworld/echostring/test');
        const data = await result.text();
        log(data);
    } catch (error) {
        log("Error: " + error.message);
    }
}

async function reverseText() {
    try {
        const result = await fetch('/rest/app/helloworld/reversestring/test');
        const data = await result.text();
        log(data);
    } catch (error) {
        log("Error: " + error.message);
    }
}

async function postString() {
    try {
        const response = await fetch('/rest/app/helloworld/poststring', {
            method: 'POST',
            headers: {
                'Content-Type': 'text/plain'
            },
            body: 'Hello, World!'
        });
        const data = await response.text();
        log(data);
    } catch (error) {
        log("Error: " + error.message);
    }
}

async function postStream() {
    const binaryData = new Uint8Array([0x01, 0x02, 0x03, 0x04, 0x05, 0x00, 0xf0, 0xff]);
    try {
        const response = await fetch('/rest/app/helloworld/poststream', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/octet-stream'
            },
            body: binaryData
        });
        const data = await response.text();
        log(data);
    } catch (error) {
        log("Error: " + error.message);
    }
}   

async function getObject() {
    try {
        const response = await fetch('/rest/app/helloworld/person?id=12');
        const person = await response.json();
        log(
            'Name: ' + person.Name + '\n' +
            'Age: ' + person.Age + '\n' +
            'Detail: ' + person.Detail
        );
    } catch (error) {
        log("Error: " + error.message);
    }
}

async function getException() {
    try {
        const response = await fetch('/rest/app/helloworld/exception');
        const error = await response.json();
        log(
            'Success: ' + response.ok + '\n' +
            'Info: ' + error.message
        );
    } catch (error) {
        log("Error: " + error.message);
    }
}

async function getImage() {
    const imgElement = document.getElementById("image");
    imgElement.src = '/rest/app/entity/image';
    imgElement.style.display = 'block';
}

async function postMultiPart() {
    const binaryArray = new Uint8Array([0x01, 0x02, 0x03, 0x04, 0x05, 0x00, 0xf0, 0xff]);
    const binaryData = new Blob([binaryArray], { type: 'application/octet-stream' });

    const formData = new FormData();
    formData.append('AValue', new Blob(['Hello, World!'], { type: 'text/plain' }));
    formData.append('AContent', binaryData, 'hello.txt');
    formData.append('AJSON', new Blob(['{"message": "Hello, World!"}'], { type: 'application/json' }), 'hello.json');

    try {
        const response = await fetch('/rest/app/helloworld/multipart', {
            method: 'POST',
            body: formData
        });
        const data = await response.json();
        log(
            'AValue: ' + data.AValue + '\n' +
            'ContentSize: ' + data.ContentSize + '\n' +
            'FileName: ' + data.FileName + '\n' +
            'Content: ' + data.Content + '\n' +
            'JSON: ' + data.JSON
        );
    } catch (error) {
        log("Error: " + error.message);
    }
}