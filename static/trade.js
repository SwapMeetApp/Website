const tradeButton = (trade) => {
    const button = document.createElement("button");
    button.appendChild(document.createTextNode("trade"));
    button.onclick = () => {
        fetch('/trade', { method: 'POST', body: JSON.stringify(trade) })
            .then(response => response.json())
            .then(data => console.log(data));
    }
    return button
}

window.onload = () => {
    const trade = document.getElementById("trade")
    trade.appendChild(tradeButton({ side1: ownedBookId, side2: bookId }))
}

