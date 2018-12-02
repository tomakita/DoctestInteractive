namespace Doctest

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Diagnostics
open Interactive
open System.Diagnostics
open System.IO
open System.Reflection
open Serilog
open Serilog.Formatting.Json
open Serilog.Events
open FsCheck
open Microsoft.CodeAnalysis.Text

module Interactive = 

    let locationsEqual (l:Location) (r:Location) =
        l.SourceSpan.Start = r.SourceSpan.Start
        && l.SourceSpan.End = r.SourceSpan.End
        && l.SourceSpan.Length = r.SourceSpan.Length

    let rec findSolution (d:DirectoryInfo) = 
        if d = null
            then 
                None
            else 
                let slns = d.GetFiles("*.sln")
                if Seq.length slns > 0
                    then Some (Seq.head slns)
                    else findSolution d.Parent

    [<DiagnosticAnalyzer(LanguageNames.CSharp)>]
    type Analyzer() =
        inherit DiagnosticAnalyzer()
        
        let diagnosticId = "DTI"
        let title = "Spec error"
        let messageFormat = "This part of the spec has been violated."
        let description = messageFormat
        let category = "Specification";
        let rule = new DiagnosticDescriptor(diagnosticId, title, messageFormat, category, DiagnosticSeverity.Error, true, description, null, null);
        let mutable invalidateCache = false

        let logger = (new LoggerConfiguration())
                       .MinimumLevel
                       .Debug()
                       .WriteTo
                       .RollingFile(
                           formatter = new JsonFormatter(),
                           pathFormat = Directory.GetCurrentDirectory() + @"\analyzer_log.txt",
                           retainedFileCountLimit = new Nullable<int>(14)) 
                       .CreateLogger()

        let reportDiagnostics (xs:seq<offset>) (context:SyntaxTreeAnalysisContext) messagePrefix severity = 
            Seq.iter 
                (fun (x:offset) -> 
                    let messageSuffix = x.name
                    let tree = getTree [context.Tree] x.location.SourceTree.FilePath

                    match tree with
                        | Some t -> 
                            let span = new TextSpan(x.location.SourceSpan.Start - x.runningOffset, x.location.SourceSpan.Length - x.offset)
                            let it = Location.Create(t, span)
                            let d = new DiagnosticDescriptor(diagnosticId, title, messagePrefix + messageSuffix, category, severity, true, description, null, null);
                            let diagnostic = Diagnostic.Create(d, it)
                            context.ReportDiagnostic(diagnostic)
                        | None -> ()) 
                xs

        let AnalyzeTree (context:SyntaxTreeAnalysisContext) = 
            let generator = new Generator()

            // it may seem odd to have these callbacks attached here, instead of 
            // in DiagnosticAnalyzer.Initialize, but they need to be attached here, because
            // there is a 1:1 correspondence between Generator instances and calls to this
            // AnalyzeTree function, and that isn't the case of Generator instances and 
            // calls to DiagnosticAnalyzer.Initialize.
            generator.add_ErrorOccurred(
                Handler 
                    (fun _ (l:(seq<offset> * SyntaxTreeAnalysisContext)) -> 
                        let c = snd l
                        let offsets = fst l
                        let hasErrors = Seq.length offsets > 0

                        let makeMessageLocationPairs offsetList = 
                            Seq.map 
                                (fun o -> { name = o.name; location = o.location; runningOffset = o.runningOffset; offset = o.offset })
                                offsetList

                        if hasErrors
                            then
                                let messageLocationPairs = makeMessageLocationPairs offsets
                                reportDiagnostics messageLocationPairs c "Error: " DiagnosticSeverity.Warning
                            else
                                ()))
            
            generator.add_TestsComplete(
                Handler 
                    (fun _ (l:(seq<(offset * (string * Outcome))> * SyntaxTreeAnalysisContext)) -> 
                        let c = snd l
                        let diagnostics = fst l
                        let hasFailedTests = Seq.length diagnostics > 0

                        let makeMessageLocationPairs diagnosticList = 
                            Seq.map
                                (fun p ->
                                    let offset = fst p
                                    let messageComponents = snd p
                                    let input = fst messageComponents
                                    let outcome = snd messageComponents
                                    let message =
                                        match outcome with
                                            | Outcome.Exception e -> 
                                                input + " and throws " + e.GetType().Name + ": " + e.Message
                                            | _ -> 
                                                input
                                    { name = message; location = offset.location; runningOffset = offset.runningOffset; offset = offset.offset })
                                diagnosticList

                        if hasFailedTests
                            then
                                let messageLocationPairs = makeMessageLocationPairs diagnostics
                                reportDiagnostics messageLocationPairs c "Specification fails with input: " DiagnosticSeverity.Error
                            else
                                ()))

            // get path to sln, and then call generator on it
            // this basically takes no time (< 1ms, though who knows how
            // long it will take on other machines?), but should still be cached.
            let rootFile = new FileInfo(context.Tree.FilePath)
            let rootDirectory = rootFile.Directory
            let sln = findSolution rootDirectory
            match sln with 
                | Some s -> 
                    try
                        generator.Generate s.FullName context false invalidateCache
                        invalidateCache <- false
                    with
                        | ex ->
                            if ex.Message = generator.SyntaxError || ex.Message = generator.InitialCompilationError
                                then ()
                                else if ex.Message = generator.ReferenceError
                                    then invalidateCache <- true
                                    else logger.Write(LogEventLevel.Error, ex, "")
                | None -> 
                    ()

        override DiagnosticAnalyzer.get_SupportedDiagnostics () = ImmutableArray.Create(rule)
        override DiagnosticAnalyzer.Initialize (context:AnalysisContext) = 
            let binLocation = typeof<Analyzer>.Assembly.Location.Split('\\')
            let pieces = Seq.length binLocation
            let bin = String.Join("\\", binLocation.Take(pieces - 1))
            let di = new DirectoryInfo(bin)
            let files = di.EnumerateFiles()
            AppDomain.CurrentDomain.add_AssemblyResolve(
                ResolveEventHandler 
                    (fun _ args ->
                        let searchName = args.Name.Split(',').[0]
                        let searchResult = files |> Seq.filter (fun f -> f.Name = searchName + ".dll")
                        if (Seq.length searchResult > 0) 
                            then 
                                let file = Seq.head searchResult
                                Assembly.LoadFile(file.FullName)
                            else 
                                null))

            context.RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext> AnalyzeTree)
