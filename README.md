# ACCPA -- Stella Type Checker

**Stage 1 of the ACCPA project**
Implements a type checker for the **Stella** programming language.

> The code template is based on the Rust template provided in the ACCPA repository. The tests were taken from the Java template test suite. My implementation was thoroughly tested against them and successfully passes all tests.
------------------------------------------------------------------------
*I’m using Windows PowerShell, so this is how I built the project and ran the tests in that environment.*

## Build

Build the project in release mode:

``` powershell
cargo build --release
```

This generates the executable:

    target\release\typechecker.exe

------------------------------------------------------------------------

## Running a Test File (PowerShell)

The type checker reads Stella programs from **standard input (stdin)**.

Run a single test file:

``` powershell
Get-Content tests\ill-typed\bad-if-1.stella | .\target\release\typechecker.exe
```

### Example Output

    ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION:
      expected type
        Bool
      but found type
        Nat
      for expression
        x

------------------------------------------------------------------------
>**Note**: if a the test is well typed the output will be empty 

## Checking the Exit Code

After running a test, check the exit code:

``` powershell
echo $LASTEXITCODE
```

-   `0` → Program is well-typed
-   Non-zero → A type error was detected

------------------------------------------------------------------------

## Running All Tests in a Folder

### Run All Ill-Typed Tests

``` powershell
Get-ChildItem tests\ill-typed\*.stella | ForEach-Object {
    Write-Host "`nRunning $($_.Name)"
    Get-Content $_.FullName | .\target\release\typechecker.exe
    Write-Host "Exit Code: $LASTEXITCODE"
}
```

------------------------------------------------------------------------

### Run All Well-Typed Tests

``` powershell
Get-ChildItem tests\well-typed\*.stella | ForEach-Object {
    Write-Host "`nRunning $($_.Name)"
    Get-Content $_.FullName | .\target\release\typechecker.exe
    Write-Host "Exit Code: $LASTEXITCODE"
}
```
