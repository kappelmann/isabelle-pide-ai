#
# Author: Kevin Kappelmann
#
# Client for Isabelle PIDE AI servers.

function usage {
  [Console]::Error.WriteLine('Usage: pide_ai_cli PORT TOOL [JSON]')
  exit 2
}

if ($args.Count -lt 2 -or $args.Count -gt 3) { usage }

$port = [int]$args[0]
$tool = $args[1]
$json = if ($args.Count -eq 3) { $args[2] } else { '{}' }
$json = $json -replace "`r", ''
$json = $json -replace "`n", ' '

try { $client = [System.Net.Sockets.TcpClient]::new('127.0.0.1', $port) }
catch { [Console]::Error.WriteLine($_.Exception.Message); exit 2 }
$utf8 = [System.Text.UTF8Encoding]::new($false)
$stream = $client.GetStream()
$request = $utf8.GetBytes("cmd`n$tool $json`n")
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
