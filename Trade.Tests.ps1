Describe 'trade api' {
        BeforeAll {
                try {
                        docker run -it -p 5432:5432 --rm --env POSTGRES_PASSWORD='hello' postgres:13.3-alpine 
                } catch {
                }
                $Env:API_KEY=(Get-Content .env)
                $Env:PG_PASSWORD="hello"
                $Env:PG_HOST="localhost"
                Start-Job -ScriptBlock { racket website.rkt --no-ssl --port 8000 }
        }

        It 'creates a trade' {
                $newTrade = Invoke-WebRequest -Method POST -Body '{"side1":"7dbddfa7-9c1d-4c7a-ad71-4f04a993bfc6" , "side2": "83f5e712-6f82-4685-9f23-fe9d3a1aee92"}' http://localhost:8000/trade
                echo $newTrade
        }
}
