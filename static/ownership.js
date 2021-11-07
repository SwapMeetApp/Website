const tradeDetails = (trade) => {
    const container = document.createElement("div")
    const tradeColumn = (value) => {
        const span = document.createElement("span")
        span.appendChild(document.createTextNode(value))
        return span
    }
    container.append(
        tradeColumn(trade.side2),
        tradeColumn(trade.state)
    )
    container.id = trade.id
    return container
}

const emptyTradesMessage = () => {
    const container = document.createElement("div")
    container.appendChild(document.createTextNode("No trades yet"))
    return container
}

const main = () => {
    fetch("/trades", {
        method: 'POST',
        body: JSON.stringify({ side1: ownedBookId })
    })
        .then(res => res.json())
        .then(trades => {
            const side1 = document.getElementById("side1")
            if (trades.length === 0) {
                side1.appendChild(emptyTradesMessage())
            } else {
                side1.append.apply(side1, trades.map(tradeDetails))
            }

        })
}
// side 2  similar message to side 1
// books api

window.onload = main
