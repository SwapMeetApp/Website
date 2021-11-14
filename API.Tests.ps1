Describe 'trade api' {
        BeforeAll {
                try {
                        $existing = docker container ls -a -f ancestor=postgres:13.3-alpine --format "{{json .}}" | ConvertFrom-Json
                        docker container start $existing[0].ID
                }
                catch {
                        docker run -it -p 5432:5432 --rm --env POSTGRES_PASSWORD='hello' postgres:13.3-alpine 
                }
                $testcase = @{
                        side1 = "7dbddfa7-9c1d-4c7a-ad71-4f04a993bfc6" ;
                        side2 = "83f5e712-6f82-4685-9f23-fe9d3a1aee92" ;
                        state = "initiated"
                } 
                $serverjob = Start-Job -ScriptBlock {
                        Set-Location $using:PWD
                        $Env:API_KEY = (Get-Content .env)
                        $Env:PG_PASSWORD = "hello"
                        $Env:PG_HOST = "localhost"
                        racket website.rkt --no-ssl --port 8000
                }
        }
        AfterAll {
                Write-Host (Receive-Job $serverjob) 
                Remove-Job -Force $serverjob
        }
        It 'starts up' { 
                $start = (Get-Date)
                while (((Get-Date) - $start).TotalSeconds -le 30.0) {
                        if (Receive-Job -Keep $serverjob | Select-String -Pattern  "Your Web application is running at") {
                                break
                        }
                }
                Receive-Job -Keep $serverjob | Select-String -Pattern  "Your Web application is running at" | Should -Match "Your Web application is running at"
                ((Get-Date) - $start).TotalSeconds | Should -BeLessThan 30.0
        }

        It 'creates a trade' {
                $newTrade = Invoke-WebRequest -Method POST -Body (ConvertTo-Json $testcase) http://localhost:8000/trade
                $uuidString = $newTrade.Content | ConvertFrom-Json 
                $script:created = New-Object -TypeName System.Guid -ArgumentList $uuidString 
        }
        
        It 'reads a trade' {
                $script:created | Should -Not -Be $null
                $uri = "http://localhost:8000/trade/$script:created"
                $existingTrade = (Invoke-WebRequest $uri).Content | ConvertFrom-Json | ConvertTo-Json
                $expected = ConvertTo-Json $testcase
                $existingTrade["side1"] | Should -BeExactly $expected["side1"]
                $existingTrade["side2"] | Should -BeExactly $expected["side2"]
                $existingTrade["state"] | Should -BeExactly $expected["state"]
        }
        It 'updates a trade' {
                $existingTrade = $testcase
                $updatedTrade = ConvertTo-Json @{side1 = $existingTrade.side2; side2 = $existingTrade.side1 ; state = "accepted" }
                $actualTrade = Invoke-WebRequest -Method PUT -Body $updatedTrade http://localhost:8000/trade/$script:created
                $uuidString = $actualTrade.Content | ConvertFrom-Json
                $script:created | Should -Be $uuidString
        }
        It 'deletes a trade' {
                $script:created | Should -Not -Be $null
                $uri = "http://localhost:8000/trade/$script:created"
                $deletedTrade = (Invoke-WebRequest -Method DELETE $uri).Content | ConvertFrom-Json
                $script:created | Should -Be $deletedTrade
        }
        It 'searches a trade' {
                $body = @{side1 = $testcase.side1 }
                $uri = "http://localhost:8000/trades"
                $searchTrade = (Invoke-WebRequest -Method POST -Body (ConvertTo-Json $body) $uri).Content | ConvertFrom-Json
                $searchTrade.Where{ $_.side1 -eq $testcase.side1 }.length  | Should -BeGreaterThan 0 
        }
}
