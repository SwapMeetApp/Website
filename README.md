# Swapmeet Website
A brokerage for trading items. It has:
- homepage
    + with chat
    + book search
- browsing page
## Future Features
Programmatic API
## Running The App
You need `racket` and `sqlite3` on your `$PATH`
```powershell
$Env:API_KEY=(Get-Content .env); racket website.rkt --no-ssl --port 8000
```
