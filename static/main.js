const ws = new WebSocket("ws://localhost:8000/websocket")
ws.onclose = event => {}

ws.onerror = event => {}

ws.onopen = event => {}

ws.onmessage = event => {
    console.log(event.data)
}