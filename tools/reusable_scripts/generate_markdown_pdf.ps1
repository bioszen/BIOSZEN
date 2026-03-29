param(
  [Parameter(Mandatory = $true)]
  [string]$InputMarkdown,

  [string]$OutputPdf = "",
  [string]$Title = "BIOSZEN Manual",
  [string]$CssUrl = "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css",
  [string]$WorkingDirectory = "."
)

$ErrorActionPreference = "Stop"

function Resolve-AbsolutePath {
  param(
    [Parameter(Mandatory = $true)]
    [string]$Path,
    [Parameter(Mandatory = $true)]
    [string]$BaseDirectory
  )

  if ([System.IO.Path]::IsPathRooted($Path)) {
    return [System.IO.Path]::GetFullPath($Path)
  }
  return [System.IO.Path]::GetFullPath((Join-Path $BaseDirectory $Path))
}

$workDirAbs = Resolve-AbsolutePath -Path $WorkingDirectory -BaseDirectory (Get-Location).Path
$inputPath = Resolve-AbsolutePath -Path $InputMarkdown -BaseDirectory $workDirAbs

if (-not (Test-Path $inputPath)) {
  throw "Input file not found: $inputPath"
}

if (-not $OutputPdf) {
  $outputPath = [System.IO.Path]::ChangeExtension($inputPath, ".pdf")
} else {
  $outputPath = Resolve-AbsolutePath -Path $OutputPdf -BaseDirectory $workDirAbs
}

if (-not (Get-Command pandoc -ErrorAction SilentlyContinue)) {
  throw "pandoc is not installed or not in PATH."
}

$edgeCandidates = @(
  "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe",
  "C:\Program Files\Microsoft\Edge\Application\msedge.exe"
)

$edgePath = $edgeCandidates | Where-Object { Test-Path $_ } | Select-Object -First 1
if (-not $edgePath) {
  throw "Microsoft Edge executable not found in common locations."
}

$template = @'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>$if(title-prefix)$$title-prefix$ - $endif$$pagetitle$</title>
  $for(css)$
  <link rel="stylesheet" href="$css$" />
  $endfor$
  <style>
    body { margin: 0; background: #ffffff; }

    .markdown-body {
      box-sizing: border-box;
      max-width: 980px;
      margin: 0 auto;
      padding: 40px;
    }

    @media (max-width: 767px) {
      .markdown-body { padding: 16px; }
    }

    @media print {
      @page { size: A4; margin: 16mm; }

      body { margin: 0; }
      .markdown-body { max-width: none; margin: 0; padding: 0; }

      h1, h2, h3, h4, h5, h6 {
        break-after: avoid-page;
        page-break-after: avoid;
        break-inside: avoid-page;
        page-break-inside: avoid;
      }

      h1 + *, h2 + *, h3 + *, h4 + *, h5 + *, h6 + * {
        break-before: avoid-page;
        page-break-before: avoid;
      }

      h1 + p, h2 + p, h3 + p, h4 + p, h5 + p, h6 + p {
        break-after: avoid-page;
        page-break-after: avoid;
      }

      h1 + p, h2 + p, h3 + p, h4 + p, h5 + p, h6 + p,
      h1 + ul, h2 + ul, h3 + ul, h4 + ul, h5 + ul, h6 + ul,
      h1 + ol, h2 + ol, h3 + ol, h4 + ol, h5 + ol, h6 + ol,
      h1 + table, h2 + table, h3 + table, h4 + table, h5 + table, h6 + table {
        break-before: avoid-page;
        page-break-before: avoid;
      }

      blockquote, pre, table, figure {
        break-inside: avoid-page;
        page-break-inside: avoid;
      }

      ul, ol, dl {
        break-inside: auto;
        page-break-inside: auto;
      }

      li { break-inside: auto; page-break-inside: auto; }

      pre, code {
        white-space: pre-wrap;
        overflow-wrap: anywhere;
      }

      p, li {
        orphans: 3;
        widows: 3;
      }

      tr {
        break-inside: avoid-page;
        page-break-inside: avoid;
      }
    }
  </style>
  $if(highlighting-css)$
  <style>
$highlighting-css$
  </style>
  $endif$
</head>
<body class="markdown-body">
$body$
</body>
</html>
'@

$tempDir = Join-Path ([System.IO.Path]::GetTempPath()) ("md-pdf-" + [Guid]::NewGuid().ToString("N"))
$null = New-Item -ItemType Directory -Path $tempDir
$templatePath = Join-Path $tempDir "template.html"
$htmlPath = Join-Path $tempDir "preview.html"

try {
  Set-Content -Path $templatePath -Value $template -Encoding UTF8

  $outDir = Split-Path -Parent $outputPath
  if ($outDir -and -not (Test-Path $outDir)) {
    $null = New-Item -ItemType Directory -Path $outDir -Force
  }

  $pandocArgs = @(
    $inputPath,
    "--from=gfm",
    "--to=html5",
    "--standalone",
    "--embed-resources",
    "--css=$CssUrl",
    "--metadata", "title=$Title",
    "--template", $templatePath,
    "-o", $htmlPath
  )

  & pandoc @pandocArgs
  if ($LASTEXITCODE -ne 0) {
    throw "pandoc failed with exit code $LASTEXITCODE"
  }

  $htmlUri = [System.Uri]::new($htmlPath).AbsoluteUri
  $edgeArgs = @(
    "--headless",
    "--disable-gpu",
    "--no-pdf-header-footer",
    "--print-to-pdf-no-header",
    "--print-to-pdf=$outputPath",
    $htmlUri
  )

  $edgeProcess = Start-Process -FilePath $edgePath -ArgumentList $edgeArgs -Wait -PassThru
  if ($null -eq $edgeProcess) {
    throw "Edge PDF export did not start."
  }
  if ($edgeProcess.ExitCode -ne 0) {
    throw "Edge PDF export failed with exit code $($edgeProcess.ExitCode)"
  }

  if (-not (Test-Path $outputPath)) {
    throw "Edge finished but output PDF was not created: $outputPath"
  }

  Write-Host "PDF generated: $outputPath"
}
finally {
  if (Test-Path $tempDir) {
    Remove-Item -Path $tempDir -Recurse -Force
  }
}
