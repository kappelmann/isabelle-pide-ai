#
# Author: Kevin Kappelmann
#
# Client for Isabelle PIDE AI servers.

function usage {
  [Console]::Error.WriteLine(@'

Usage: pide_ai_cli PORT [OPTIONS] TOOL [JSON | KEY=STRING ... | KEY:=JSON ...]

  Options are:
    -p           report progress

  Prints result on stdout, progress and errors on stderr. Exits 1 on error.
  list_extensions show_input_schema:=true lists tools with their arguments.

'@)
  exit 2
}

function flatten([string] $s) { ($s -replace "`r", '') -replace "`n", ' ' }

function json_string([string] $s) { ConvertTo-Json -Compress -InputObject $s }

$rest = [string[]]$args
if ($rest.Count -lt 1 -or $rest[0] -notmatch '^[0-9]+$') { usage }
$port = [int]$rest[0]
$rest = @($rest | Select-Object -Skip 1)

$meta = ''
while ($rest.Count -gt 0 -and $rest[0] -like '-*') {
  switch -CaseSensitive ($rest[0]) {
    '-p' { $meta = ',"_meta":{"progressToken":1}' }
    default { usage }
  }
  $rest = @($rest | Select-Object -Skip 1)
}

if ($rest.Count -lt 1) { usage }
$tool = $rest[0]
$rest = @($rest | Select-Object -Skip 1)

if ($rest.Count -eq 0) {
  $json = '{}'
}
elseif ($rest.Count -eq 1 -and $rest[0] -match '^\s*\{') {
  $json = flatten $rest[0]
}
else {
  $json = ''
  foreach ($arg in $rest) {
    if ($arg -notlike '*=*') { usage }
    $key, $value = $arg -split '=', 2
    if ($key.EndsWith(':')) {
      $key = $key.Substring(0, $key.Length - 1)
      $value = flatten $value
    }
    else {
      $value = json_string $value
    }
    if ($json) { $json += ',' }
    $json += (json_string $key) + ':' + $value
  }
  $json = '{' + $json + '}'
}
$request = '{"name":' + (json_string $tool) + ',"arguments":' + $json + $meta + '}'

$utf8 = [System.Text.UTF8Encoding]::new($false)
[Console]::OutputEncoding = $utf8
try { $client = [System.Net.Sockets.TcpClient]::new('127.0.0.1', $port) }
catch { [Console]::Error.WriteLine($_.Exception.Message); exit 2 }
$stream = $client.GetStream()
$bytes = $utf8.GetBytes("cmd $((Get-Location).ProviderPath)`n$request`n")
$stream.Write($bytes, 0, $bytes.Length)
$reader = [System.IO.StreamReader]::new($stream, $utf8)
$first = $reader.ReadLine()
if ($first -ne 'OK') {
  if ($null -ne $first) { [Console]::Error.WriteLine($first) }
  exit 1
}
$rc = 1
while ($null -ne ($line = $reader.ReadLine())) {
  if ($line.StartsWith('{"result":')) {
    $line
    $rc = 0
  }
  else {
    [Console]::Error.WriteLine($line)
  }
}
exit $rc
