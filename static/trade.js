const tradeButton = (trade, container) => {
    let creatingTrade = false
    const button = document.createElement("button");
    button.appendChild(document.createTextNode("trade"));
    button.onclick = () => {
        if (creatingTrade === false) {
            fetch('/trade', { method: 'POST', body: JSON.stringify(trade) })
                .then(response => response.json())
                .then(data => container.appendChild(successMessage()));
        }
        creatingTrade = true
        button.setAttribute("disabled", true)
    }
    return button
}

const successMessage = () => {
    const message = document.createElement("p")
    message.appendChild(document.createTextNode("Trade Initiated. Check out status in ownership page"))
    return message
}

window.onload = () => {
    const trade = document.getElementById("trade")
    trade.appendChild(tradeButton({ side1: ownedBookId, side2: bookId, state: "initiated" }, trade))
}


