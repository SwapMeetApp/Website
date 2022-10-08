const tradeFurtherAction = (trade) => {
    /* User owns this book */
    if (ownedBookId === trade.side1) {
        return document.createTextNode(trade.state)
    } else {
        switch (trade.state) {
            case "initiated": return acceptButton(trade);
            case "accepted": return document.createTextNode("Waiting");
            case "completed": return document.createTextNode("Trade completed");
            default: return document.createTextNode(`unknown state ${trade.state}`);
        }
    }
}


const acceptButton = (trade) => {
    let done = false
    const button = document.createElement("button");
    button.appendChild(document.createTextNode("accept?"));
    button.onclick = () => {
        if (done === false) {
            fetch(`/trade/${trade.id}`, { method: 'PUT', body: JSON.stringify({ ...trade, state: 'accepted' }) })
                .then(response => response.ok ? alert("trade accepted") : alert("server error"));
        }
        done = true
        button.setAttribute("disabled", true)
    }
    return button
}

const tradeDetails = (tradeDetails) => {
    const container = document.createElement("div")
    const column = (el) => {
        const span = document.createElement("span")
        span.appendChild(el)
        span.style = "margin-right: 2rem;"
        return span
    }
    container.append(
        column(document.createTextNode(tradeDetails.book.title)),
        column(document.createTextNode(tradeDetails.trade.state)),
        column(tradeFurtherAction(tradeDetails.trade))
    )
    container.id = tradeDetails.id
    return container
}

const emptyTradesMessage = () => {
    const container = document.createElement("div")
    container.appendChild(document.createTextNode("No trades yet"))
    return container
}

const findTrades = (side, id) =>
    fetch("/trades", {
        method: 'POST',
        body: JSON.stringify({ [side]: id })
    })
        .then(res => res.json())

const attachTrades = (trades, element) => {
    if (trades.length === 0) {
        element.appendChild(emptyTradesMessage())
    } else {
        element.append.apply(element, trades.map(tradeDetails))
    }
}

const getBook = (id) =>
    fetch(`/books/${id}`).then(res => res.json())

const main = () => {
    findTrades("side1", ownedBookId)
        .then(trades => Promise.all(trades.map(t => getBook(t.side2).then(book => ({ book, trade: t })))))
        .then(trades => attachTrades(trades, document.getElementById("side1")))

    findTrades("side2", ownedBookId)
        .then(trades => Promise.all(trades.map(t => getBook(t.side2).then(book => ({ book, trade: t })))))
        .then(trades => attachTrades(trades, document.getElementById("side2")))
}


// books api

window.onload = main
