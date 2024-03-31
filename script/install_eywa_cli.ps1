# Determine OS and Architecture
$OS = if ([Environment]::OSVersion.Platform -eq "Win32NT") { "windows" } else { "unknown" }
$ARCH = if ([Environment]::Is64BitOperatingSystem) { "x64" } else { "x86" }

# URL for the latest version
$LATEST_VERSION_URL = "https://s3.eu-central-1.amazonaws.com/eywa.public/eywa_cli/latest"

# Fetch the latest version
$VERSION = Invoke-RestMethod -Uri $LATEST_VERSION_URL

# Parse command-line arguments for --version
$VersionArgument = $null
for ($i = 0; $i -lt $args.Count; $i++) {
    if ($args[$i] -eq "--version") {
        $VersionArgument = $args[$i + 1]
        break
    }
}

if ($null -ne $VersionArgument) {
    $VERSION = $VersionArgument
}

Write-Host "Using version: $VERSION"

$LOCATION = "eywa_cli/$OS/$ARCH/$VERSION/eywa.exe"
$S3URL = "https://s3.eu-central-1.amazonaws.com/eywa.public/$LOCATION"

# Define install location
$INSTALL_DIR = Join-Path -Path $HOME -ChildPath ".eywa\bin\"

# Create the installation directory if it doesn't exist
if (-not (Test-Path -Path $INSTALL_DIR)) {
    New-Item -ItemType Directory -Path $INSTALL_DIR -Force | Out-Null
}

# Check if Eywa is already installed
if (Test-Path -Path (Join-Path -Path $INSTALL_DIR -ChildPath "eywa.exe")) {
    Write-Host "Eywa is already installed at $INSTALL_DIR`eywa."
    exit
}

# Use Invoke-WebRequest to check if the file is accessible
try {
    $response = Invoke-WebRequest -Uri $S3URL -Method Head -ErrorAction Stop
    if ($response.StatusCode -ne 200) {
        throw "The file $S3URL does not exist or is not accessible."
    }
}
catch {
    Write-Host "The file $S3URL does not exist or is not accessible."
    Write-Host $_.Exception.Message
    exit
}

# Download the file
Invoke-WebRequest -Uri $S3URL -OutFile (Join-Path -Path $INSTALL_DIR -ChildPath "eywa.exe")

Write-Host "$LOCATION installed successfully. Please ensure $INSTALL_DIR is in your PATH."


# Invoke-WebRequest -Uri "https://s3.eu-central-1.amazonaws.com/eywa.public/eywa_cli/install_eywa_cli.ps1" -OutFile eywa_cli_install.ps1
# ./eywa_cli_install.ps1
# Add %USERPROFILE%\.eywa\bin to PATH environment variable
