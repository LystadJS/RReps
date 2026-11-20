# Examples: .\scripts\start.ps1, .\scripts\start.ps1 scripts/setup.R
param(
    [Parameter(Position = 0)][string]$Script = 'run.R',
    [Parameter(Position = 1, ValueFromRemainingArguments = $true)][string[]]$ScriptArgs
)
$ErrorActionPreference = 'Stop'
$project = Split-Path -Parent $PSScriptRoot
$pathCommand = Get-Command Rscript.exe -CommandType Application -ErrorAction SilentlyContinue | Select-Object -First 1
$candidates = @()
if ($pathCommand) { $candidates += $pathCommand.Source }

foreach ($key in 'HKCU:\SOFTWARE\R-core\R', 'HKLM:\SOFTWARE\R-core\R') {
    $install = Get-ItemProperty -LiteralPath $key -Name InstallPath -ErrorAction SilentlyContinue
    if ($install.InstallPath) { $candidates += Join-Path $install.InstallPath 'bin\Rscript.exe' }
}
$installationRoot = Join-Path $env:ProgramFiles 'R'
$installations = Get-ChildItem -LiteralPath $installationRoot -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -match '^R-\d+\.\d+\.\d+$' } |
    Sort-Object { [version]$_.Name.Substring(2) } -Descending
foreach ($installation in $installations) { $candidates += Join-Path $installation.FullName 'bin\Rscript.exe' }
$rscript = $candidates | Where-Object { Test-Path -LiteralPath $_ -PathType Leaf } | Select-Object -First 1
if (!$rscript) { throw 'Rscript.exe was not found. Install R 4.5.3 or add its bin directory to PATH.' }

Push-Location -LiteralPath $project
try {
    if (!(Test-Path -LiteralPath $Script -PathType Leaf)) { throw "R script does not exist: $Script" }
    & $rscript --vanilla $Script @ScriptArgs
    $result = $LASTEXITCODE
} finally {
    Pop-Location
}
exit $result
