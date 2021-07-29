Describe 'trade api' {
        BeforeAll {
                try {
                        $existing = docker container ls -a -f ancestor=postgres:13.3-alpine --format "{{json .}}" | ConvertFrom-Json
                        docker container start $existing[0].ID
                } catch {
                        docker run -it -p 5432:5432 --rm --env POSTGRES_PASSWORD='hello' postgres:13.3-alpine 
                }
                $Env:API_KEY = (Get-Content .env)
                $Env:PG_PASSWORD = "hello"
                $Env:PG_HOST = "localhost"
                $serverlog = New-TemporaryFile
                $serverjob = Start-Job -ScriptBlock { Set-Location $using:PWD; racket website.rkt --no-ssl --port 8000 }
        }
        AfterAll {
                Remove-Job -Force $serverjob
        }
        It 'starts up' { 
                $start = (Get-Date)
                while (((Get-Date) - $start).TotalSeconds -le 30.0) {
                        if (Receive-Job -Keep $serverjob | Select-String -Pattern  "Your Web application is running at"){
                                break
                        }
                }
                Receive-Job -Keep $serverjob | Select-String -Pattern  "Your Web application is running at" | Should Match "Your Web application is running at"
                ((Get-Date) - $start).TotalSeconds | Should BeLessThan 30.0
        }

        It 'creates a trade' {
                $newTrade = Invoke-WebRequest -Method POST -Body '{"side1":"7dbddfa7-9c1d-4c7a-ad71-4f04a993bfc6" , "side2": "83f5e712-6f82-4685-9f23-fe9d3a1aee92"}' http://localhost:8000/trade
                echo $newTrade
        }
}
