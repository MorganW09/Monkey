## Unit Test Project

### Running specific unit tests

```
dotnet test --filter DisplayName=ParserTests.CanTestFunctionalLiteralParsing
dotnet test --filter DisplayName=EvaluatorTests.CanTestFunctionApplications
```

### Printing to file for debugging

```fsharp

    let debugPrint message =
        System.IO.File.AppendAllText("./file.txt", sprintf "%s%s" message System.Environment.NewLine);

```