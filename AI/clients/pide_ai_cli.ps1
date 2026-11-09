#
# Author: Kevin Kappelmann
#
# Client for Isabelle PIDE AI servers.

function usage {
  [Console]::Error.WriteLine(@'

Usage: pide_ai_cli PORT [OPTIONS] TOOL [JSON]

  Options are:
    -p           report progress

  list_extensions '{"show_input_schema":true}' lists tools with their arguments.

'@)
  exit 2
}

if ($args.Count -lt 1 -or $args[0] -eq '-?') { usage }
$port = [int]$args[0]
$rest = @($args | Select-Object -Skip 1)

$meta = ''
while ($rest.Count -gt 0 -and $rest[0] -like '-*') {
  switch -CaseSensitive ($rest[0]) {
    '-p' { $meta = ',"_meta":{"progressToken":1}' }
    default { usage }
  }
  $rest = @($rest | Select-Object -Skip 1)
}
if ($rest.Count -lt 1 -or $rest.Count -gt 2) { usage }

$tool = $rest[0]
$json = if ($rest.Count -eq 2) { $rest[1] } else { '{}' }
$json = $json -replace "`r", ''
$json = $json -replace "`n", ' '

try { $client = [System.Net.Sockets.TcpClient]::new('127.0.0.1', $port) }
catch { [Console]::Error.WriteLine($_.Exception.Message); exit 2 }
$utf8 = [System.Text.UTF8Encoding]::new($false)
$stream = $client.GetStream()
$cwd = (Get-Location).ProviderPath
$request = $utf8.GetBytes("cmd $cwd`n{`"name`":`"$tool`",`"arguments`":$json$meta}`n")
$stream.Write($request, 0, $request.Length)
$reader = [System.IO.StreamReader]::new($stream, $utf8)
$first = $reader.ReadLine()
if ($first -ne 'OK') {
  if ($null -ne $first) { $first }
  $client.Close()
  exit 1
}
$rc = 1
while ($null -ne ($line = $reader.ReadLine())) {
  $line
  if ($line.StartsWith('{"result":')) { $rc = 0 }
}
$client.Close()
exit $rc
