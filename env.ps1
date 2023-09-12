$env_vars = Get-Content .\.env
foreach ($var in $env_vars) {
    # Skip comments
    if ($var -match "^[#]") {
        continue
    }

    # Skip empty lines or lines without an equals sign
    if (-not $var -or $var -notmatch "=") {
        continue
    }

    $pair = $var -split "=", 2

    # Skip if the variable name or value is empty
    if ([string]::IsNullOrEmpty($pair[0]) -or [string]::IsNullOrEmpty($pair[1])) {
        Write-Host "Skipping empty variable or value."
        continue
    }

    [System.Environment]::SetEnvironmentVariable($pair[0], $pair[1], [System.EnvironmentVariableTarget]::Process)
    Write-Host "Set variable $($pair[0]) to $($pair[1])"
}
