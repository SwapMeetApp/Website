const messageWithText = text => {
    const p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    return p 
}

const ws = new WebSocket("ws://localhost:8000/websocket")
ws.onclose = event => {}

ws.onerror = event => {}

ws.onopen = event => {}

ws.onmessage = event => {
    const messages = document.getElementById("messages")
    messages.appendChild(messageWithText(event.data))
    messages.scrollTop = messages.scrollHeight
}
const sendMessage = () => {
    ws.send(chat.value)
}