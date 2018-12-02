namespace Doctest

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Xml.Linq
open System.Collections.Immutable
open FsCheck
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis.Diagnostics
open System.Diagnostics

module Interactive =
    
    type ComparableLocation = {
        Start:int;
        End:int}
    type NodeSwap = { 
        LastNode:SyntaxNode; 
        TestNode:SyntaxNode }
    type MaybeSpecWithLocation = { 
        MaybeSpec:option<string>; 
        Location:Location }
    type SpecWithLocation = { 
        Spec:string; 
        Location:Location }
    type snippetsWithMetadata = { 
        snippets: seq<SpecWithLocation>; 
        methodNode:MemberDeclarationSyntax; 
        classNode: SyntaxNode; 
        namespaceNode: NamespaceDeclarationSyntax }
    type snippetWithMetadata = {
        snippet: SpecWithLocation; 
        methodNode:MemberDeclarationSyntax; 
        classNode: SyntaxNode; 
        namespaceNode: NamespaceDeclarationSyntax }
    type SF = SyntaxFactory
    type OnTestsComplete = delegate of (obj * List<string>) -> unit
    type offset = {
        name: string;
        location: Location;
        runningOffset: int;
        offset: int }

    // possible bug: what if tree doesn't contain any of the nodes in nodePairs?
    // we try to insert via transform, and...what would happen?  exception?
    // pretty sure it would throw an exception, yes.
    let rec apply (tree:SyntaxTree) (transform) (nodePairs:seq<NodeSwap>) = 
        let p = 
            if Seq.length nodePairs = 0 
                then None 
                else Some (Seq.head nodePairs)
        match p with
            | None -> tree
            | Some n -> 
                let mappedLastNode = tree.GetRoot().GetCurrentNode(n.LastNode)
                if mappedLastNode = null // means n.LastNode is not in this tree
                    then apply 
                            tree
                            transform
                            (Seq.skip 1 nodePairs)
                    else apply 
                            (transform tree mappedLastNode n.TestNode) 
                            transform 
                            (Seq.skip 1 nodePairs)

    let getName (node:MemberDeclarationSyntax) =
        let kind = node.Kind()
        match kind with
            | SyntaxKind.PropertyDeclaration -> 
                (node :?> PropertyDeclarationSyntax).Identifier.ToString()
            | SyntaxKind.MethodDeclaration -> 
                (node :?> MethodDeclarationSyntax).Identifier.ToString()
            | SyntaxKind.ConstructorDeclaration -> 
                let ctorAsString = (node :?> ConstructorDeclarationSyntax).ToString()
                let uniqueRepresentation = ctorAsString.GetHashCode().ToString().Replace("-", "n")
                "ctor" + uniqueRepresentation
            | _ -> failwith "unsupported node type"

    let makeMethodFromSnippet (s:string) = 
        // this assumes that the method is, in fact, a bool, so compilation will fail if the method
        // doesn't return bool.
        let root = CSharpSyntaxTree
                    .ParseText("public static " + s)
                    .GetRoot() 
                    :?> CompilationUnitSyntax
        root.Members.Single() 

    let makeMethodFromSnippetForValidation (s:string) = 
        // this assumes that the method is, in fact, a bool, so compilation will fail if the method
        // doesn't return bool.
        let root = CSharpSyntaxTree
                    .ParseText(s)
                    .GetRoot() 
                    :?> CompilationUnitSyntax
        try
            Some (root.Members.Single())
        with _ ->
            None

    let makeId (s:snippetsWithMetadata) (spec:string) =
        let methodName = 
            if s.methodNode = null
                then ""
                else getName s.methodNode
        let id = s.namespaceNode.Name.ToString()
                    + (s.classNode :?> ClassDeclarationSyntax).Identifier.ToString()
                    + methodName
                    + spec
        id.GetHashCode().ToString()

    let makeTestClass 
        (s:snippetsWithMetadata) 
        (tokens:seq<SyntaxToken>) 
        (methodNameGenerator:snippetsWithMetadata -> string)
        (transform) = 
        let validMethods = 
            Seq.map 
                (fun i -> 
                    let node = transform i.Spec s
                    node.WithAdditionalAnnotations(new SyntaxAnnotation("id", makeId s i.Spec))) 
                s.snippets

        SF.ClassDeclaration(methodNameGenerator s)
          .WithModifiers(SF.TokenList(tokens))
          .AddMembers(Seq.toArray validMethods)

    let rec insertClasses (c:seq<ClassDeclarationSyntax>) (root:SyntaxNode) (last:SyntaxNode) = 
        if Seq.length c = 0
            then 
                root
            else 
                let classNode = (Seq.head c) :> SyntaxNode
                insertClasses 
                    (c.Skip(1)) 
                    (root.InsertNodesAfter(last, [classNode])) 
                    (Seq.head c)
    
    let rec getUsings (n:SyntaxNode) =
        // does this return the transitive closure of all nested namespaces?
        // is a CompilationUnitSyntax necessarily the root of the tree?
        // what is the difference between CompilationUnitSyntax and NamespaceDeclarationSyntax?
        if (n.IsKind(SyntaxKind.CompilationUnit)) 
            then (n :?> CompilationUnitSyntax).Usings 
            else getUsings(n.Parent)

    let rec getNearestNamespace (n:SyntaxNode) =
        if (n.IsKind(SyntaxKind.NamespaceDeclaration))
            then n :?> NamespaceDeclarationSyntax
            else 
                if n.Parent = null
                    // first branch is if there is no namespace in the tree -- in this case,
                    // we simply insert a sentinel namespace node, since it will be ignored anyway
                    // later on in the program.
                    then SF.NamespaceDeclaration(SF.IdentifierName("$CompilationUnit$"))
                    else getNearestNamespace(n.Parent)

    let flattenSnippets (s:seq<snippetsWithMetadata>) = 
        let mapToIndividual (m:snippetsWithMetadata) = 
            Seq.map 
                (fun n -> { 
                    snippet = n; 
                    methodNode = m.methodNode; 
                    classNode = m.classNode; 
                    namespaceNode = m.namespaceNode }) 
                m.snippets
        Seq.collect 
            (fun (m:snippetsWithMetadata) -> mapToIndividual m) 
            s

    //let makeName (s:snippetWithMetadata) = 
    //    let namespaceName = s.namespaceNode.Name.ToString() 
    //    let className = (s.classNode :?> ClassDeclarationSyntax).Identifier.ToString() 
    //    let nestedClassName = getName s.methodNode
    //    let signaturePrefix = s.snippet.Spec.Split('(').First()
    //    let methodName = signaturePrefix.Split(' ').[1]

    //    namespaceName + "." + className + "." + nestedClassName + "_Doctest." + methodName

    let makeName (s:snippetWithMetadata) = 
        let namespaceName = s.namespaceNode.Name.ToString() 
        let nestedClassName = getName s.methodNode
        let signaturePrefix = s.snippet.Spec.Split('(').First()
        let methodName = signaturePrefix.Split(' ').[1]

        namespaceName + "." + nestedClassName + "_Doctest." + methodName

    let makeFullyQualifiedName (s:snippetsWithMetadata) = 
        let namespaceName = s.namespaceNode.Name.ToString()
        let className = (s.classNode :?> ClassDeclarationSyntax).Identifier.ToString()
        let methodName = getName s.methodNode

        namespaceName + "." + className + "." + methodName + "_Doctest"
    
    let makeArbClassName (s:snippetsWithMetadata) =
                    let className = (s.classNode :?> ClassDeclarationSyntax).Identifier.ToString()
                    className + "Generator"

    // e.g. getNameLocationMap (flattenSnippets snippetClassPairs)
    let getNameLocationOffsetTriples (s:seq<snippetWithMetadata>) = 
        Seq.map 
            (fun m ->
                let resultingOffset = m.snippet.Spec.Count(fun c -> c = '>' || c = '<')
                (makeName m, (m.snippet.Location, resultingOffset * 3)))
            s

    let getOffsetTriplesWithBlankName (s:seq<snippetWithMetadata>) = 
        Seq.map 
            (fun m ->
                let resultingOffset = m.snippet.Spec.Count(fun c -> c = '>' || c = '<')
                (Guid.NewGuid().ToString() , (m.snippet.Location, resultingOffset * 3)))
            s

    // e.g. tag = "spec"
    let transformXml tag (xml:string) =
        let openingTag = "<" + tag + ">"
        let closingTag = "</" + tag + ">"

        if xml.Contains(openingTag)
            then 
                let fronts = xml.Split([|openingTag|], StringSplitOptions.None)
                let prefix =
                    if fronts.[0].Contains(closingTag)
                        then ""
                        else fronts.[0]

                let backs =
                    Seq.map
                        (fun (s:string) ->
                            let components = s.Split([|closingTag|], StringSplitOptions.None)
                            let spec = components.[0]
                            let legalSpec = spec.Replace("<", "&lt;").Replace(">", "&gt;")
                
                            let finalSpec =
                                if Seq.length components > 1
                                    then legalSpec + closingTag + components.[1]
                                    else legalSpec
                            finalSpec)
                        (fronts.Skip(1))

                let aBack = String.Join(openingTag, backs)
                let aReconstructed = prefix + openingTag + aBack
                aReconstructed
            else
                xml

    let getXmlElements (trivia:seq<SyntaxTrivia>) (nodeNames:seq<string>) = 
        match Seq.length trivia with
            | 0 -> Seq.empty
            | _ -> 
                let contents = 
                    Seq.map 
                        (fun (t:SyntaxTrivia) -> 
                            let comment = (t.GetStructure() :?> DocumentationCommentTriviaSyntax)
                            comment.Content) 
                        trivia
                    |> Seq.collect id
                let elements = 
                    Seq.filter 
                        (fun (n:XmlNodeSyntax) -> n.IsKind SyntaxKind.XmlElement) 
                        contents
                    |> Seq.map
                        (fun e -> e :?> XmlElementSyntax)
                Seq.filter 
                    (fun (n:XmlElementSyntax) -> nodeNames.Contains(n.StartTag.Name.ToString())) 
                    elements

    let getPreprocessedXml (specs:seq<XmlElementSyntax>) (xmlTag:string) =
        Seq.map 
            (fun (s:XmlElementSyntax) -> 
                let location = s.GetLocation()
                let originalText = s.GetText().ToString()
                let startTag = "<" + xmlTag + ">"
                let endTag = "</" + xmlTag + ">"
                // not robust enough for the case in which there are more than one
                // instance of startTag or endTag in originalText.
                let detagged = originalText.Replace(startTag, "").Replace(endTag, "")
                if detagged.Contains("<") || detagged.Contains(">")
                    then 
                        let decommented = detagged.Replace(@"///", "")
                        let decommentedNode = new SyntaxList<XmlNodeSyntax>(SF.XmlText(decommented))
                        let preprocessed = SF.XmlElement(xmlTag, decommentedNode)
                        (preprocessed, location)
                    else
                        (s, location))
            specs

    let getTestSnippets (specs:seq<XmlElementSyntax>) (xmlTag:string) =
        let preprocessedSpecs = Seq.map (fun (s:XmlElementSyntax) -> (s, s.GetLocation())) specs

        let getSpecCodeString (l:SyntaxTokenList) = 
            Seq.map 
                (fun (t:SyntaxToken) -> t.ValueText)
                l 
            |> String.concat ""
        let getSpecCode (e:XmlElementSyntax) = 
            if e.Content.Count > 0
            then 
                let text = e.GetText().ToString()
                let spec = e.Content.Single() :?> XmlTextSyntax
                Some (getSpecCodeString spec.TextTokens)
            else 
                None
        let possibleSpecsWithLocation = 
            Seq.map 
                (fun e -> 
                    let spec = fst e
                    let location = snd e
                    { MaybeSpec = (getSpecCode spec); Location = location })
                preprocessedSpecs
        let specsWithLocation = 
            Seq.filter 
                (fun (s:MaybeSpecWithLocation) -> 
                    match s.MaybeSpec with
                        | Some x -> true
                        | None -> false) 
                possibleSpecsWithLocation
        Seq.map 
            (fun s -> { Spec = s.MaybeSpec.Value; Location = s.Location })
            specsWithLocation

    let getMatchingNodesInCompilation<'a when 'a :> MemberDeclarationSyntax> (compilation:Compilation) = 
        let s = 
            Seq.map 
                (fun tree -> compilation.GetSemanticModel(tree)) 
                compilation.SyntaxTrees 
            |> Seq.collect 
                (fun model -> model.SyntaxTree.GetRoot().DescendantNodes().OfType<'a>())
        s.ToImmutableList()

    let getMatchingNodesInSubtree<'a when 'a :> MemberDeclarationSyntax> (n:SyntaxNode) =
        n.DescendantNodes().OfType<'a>().ToImmutableList();

    let getMatchingNodesInTree<'a> (t:SyntaxTree) =
        t.GetRoot().DescendantNodes().OfType<'a>().ToImmutableList();

    let getMatchingNodesInTrees<'a when 'a :> MemberDeclarationSyntax> (ts:seq<SyntaxTree>) = 
        let s = 
            Seq.collect 
                (fun (t:SyntaxTree) -> t.GetRoot().DescendantNodes().OfType<'a>())
                ts
        s.ToImmutableList()

    // proj x is getting a reference to itself added here...don't do that
    let getReferencesByProject (solution:Solution) (compilations:Map<string, seq<Compilation>>) = 
        Map.map
            (fun _ v -> 
                Seq.collect
                    (fun (c:Compilation) -> c.ExternalReferences)
                    v)
            compilations

    // outer location is fst, inner is snd
    let getOuterAndInnerLocations (x:Diagnostic) (ys:seq<Location * Location>) =
        // precondition: there should never be more than 1 element y in ys
        // such that x is enclosed in y.
        let matches = 
            Seq.filter
                (fun (y:(Location * Location)) ->
                    let methodLocation = fst y
                    let methodStart = methodLocation.SourceSpan.Start
                    let methodEnd = methodLocation.SourceSpan.End

                    x.Location.SourceSpan.Start >= methodStart
                    && x.Location.SourceSpan.End <= methodEnd)
                ys
        if Seq.length matches > 0
            then 
                let pair = Seq.head matches
                Some (x, snd pair)
            else
                None

    let getSourceErrorLocations 
        (ds:seq<Diagnostic>) 
        (locationPairs:seq<(Location * Location)>) 
        (offsets:seq<offset>) =
        Seq.map
            (fun (d:Diagnostic) -> getOuterAndInnerLocations d locationPairs)
            ds 
        |> Seq.filter 
                (fun p -> 
                    match p with
                        | Some s -> true
                        | None -> false)
        |> Seq.map 
            (fun o -> 
                let (d, l) = o.Value
                let offset = offsets.Single((fun o -> o.location.SourceSpan.Start = l.SourceSpan.Start))
                { name = d.GetMessage(); location = l; runningOffset = offset.runningOffset; offset = offset.offset })

    // for use with buildalyzer
    //let makeSolution (slnFilePath:string) = 
    //    let manager = new AnalyzerManager(slnFilePath)
    //    let workspace = manager.GetWorkspace()
    //    workspace.CurrentSolution

    let makeSolution (slnFilePath:string) = async {
        let workspace = MSBuildWorkspace.Create()
        return! workspace.OpenSolutionAsync(slnFilePath) |> Async.AwaitTask
    }

    let getCompilation (p:Project) = async {
        return! p.GetCompilationAsync() |> Async.AwaitTask
    }

    let makeCompilations (projects:seq<Project>) = async {
        return! Seq.map getCompilation projects |> Async.Parallel
    }

    let filterTriviaList ts = 
        Seq.filter
            (fun (t:SyntaxTrivia) -> t.IsKind SyntaxKind.SingleLineDocumentationCommentTrivia)
            ts

    let getTrivia<'a when 'a :> MemberDeclarationSyntax> (ts:IEnumerable<SyntaxTree>) = 
        getMatchingNodesInTrees<'a> ts 
        |> Seq.map 
            (fun n -> (n, n.GetLeadingTrivia()))
        |> Seq.filter 
            (fun (p:('a * SyntaxTriviaList)) -> 
                let triviaList = snd p
                let filteredTriviaList = filterTriviaList triviaList
                let xmlElements = getXmlElements filteredTriviaList ["spec";"refinement"]
                Seq.length xmlElements > 0)

    let makePairs<'a when 'a :> MemberDeclarationSyntax> (ts:seq<SyntaxTree>) (xmlTag:string) = 
        Seq.collect
            (fun t ->
                getMatchingNodesInTree<'a> t 
                |> Seq.map 
                    (fun n -> 
                        let trivia = n.GetLeadingTrivia()
                        let filteredTriviaWithoutXml = filterTriviaList trivia
                        let filteredTrivia = getXmlElements filteredTriviaWithoutXml [xmlTag]
                        let snippets = 
                            if Seq.length filteredTrivia = 0 
                                then null 
                                else getTestSnippets filteredTrivia xmlTag

                        (snippets, n)))
            ts

    let getTree (ts:seq<SyntaxTree>) (filePath:string) =
        let trees = 
            Seq.filter 
                (fun (t:SyntaxTree) -> t.FilePath = filePath)
                ts
        if Seq.length trees > 0
            then Some (trees |> Seq.head)
            else None

    let makeFilteredSnippets (s:snippetsWithMetadata) (transform) =
        Seq.filter 
            (fun (i:SpecWithLocation) -> 
                let maybeSnippet = makeMethodFromSnippetForValidation (transform i.Spec s)
                match maybeSnippet with
                    | Some some -> true
                    | None -> false) 
            s.snippets

    let filterSnippetClassPairs (s:seq<snippetsWithMetadata>) = 
        Seq.filter 
            (fun (p:snippetsWithMetadata) -> 
                p.snippets <> null
                && p.classNode.IsKind SyntaxKind.ClassDeclaration)
            s

    let makeMethodPair (snippets:seq<SpecWithLocation>) (node:MemberDeclarationSyntax) = { 
        snippets = snippets; 
        methodNode = node;
        classNode = node.Parent; 
        namespaceNode = getNearestNamespace(node.Parent) }

    let makeClassPair (snippets:seq<SpecWithLocation>) (node:MemberDeclarationSyntax) = { 
        snippets = snippets; 
        methodNode = null; 
        classNode = node; 
        namespaceNode = getNearestNamespace(node.Parent) }

    let makeSnippetNodePair (pairs:seq<seq<SpecWithLocation> * #MemberDeclarationSyntax>) pairMaker =
        Seq.map 
            (fun p -> 
                let snippets = fst p
                let node = snd p
                pairMaker snippets node) 
            pairs

    let makeSnippetClassPairs<'a when 'a :> MemberDeclarationSyntax> 
        (treesByProject:Map<string, seq<SyntaxTree>>) 
        (xmlTag:string) 
        pairMaker = 
        Map.map 
            (fun _ (v:seq<SyntaxTree>) ->
                    let ve = v.ToList()
                    let pairs = makePairs<'a> ve xmlTag

                    makeSnippetNodePair pairs pairMaker 
                        |> filterSnippetClassPairs) 
            treesByProject

    let replaceIndividualSnippet (s:seq<snippetsWithMetadata>) transform =
        Seq.map 
            (fun (s:snippetsWithMetadata) ->
                let filtered = makeFilteredSnippets s transform
                { s with snippets = filtered })
            s

    let replaceWithFilteredSnippets (snippetPairs:Map<string, seq<snippetsWithMetadata>>) transform = 
        Map.map 
            (fun _ v -> replaceIndividualSnippet v transform)
            snippetPairs

    let filterEmptySnippets (s:seq<snippetsWithMetadata>) = 
        Seq.filter 
            (fun (s:snippetsWithMetadata) -> not (Seq.isEmpty s.snippets))
            s

    let removeEmptySnippets (snippetPairs:Map<string, seq<snippetsWithMetadata>>) = 
        Map.map 
            (fun _ v -> filterEmptySnippets v) 
            snippetPairs

    let Throws<'ex when 'ex :> Exception> (a:Action) = 
        try
            a.Invoke()
            false
        with
            | e -> e.GetType() = typeof<'ex>

    let getTriviaPairs<'a when 'a :> MemberDeclarationSyntax> trees = 
        getTrivia<'a> trees 
        |> Seq.map (fun p -> ((fst p) :> MemberDeclarationSyntax, snd p))

    let makeTreesWithChangedTrivia<'a when 'a :> SyntaxNode> trees (triviaPairsByTree:Map<string, seq<('a * SyntaxTriviaList)>>) =
        let makeNodeMap triviaPairs = 
            Seq.groupBy
                (fun (p:'a * SyntaxTriviaList) -> (fst p).ToFullString())
                triviaPairs
            |> Map.ofSeq
        Seq.map
            (fun (t:SyntaxTree) -> 
                if triviaPairsByTree.ContainsKey(t.FilePath)
                    then
                        let originalNodes = Seq.map fst triviaPairsByTree.[t.FilePath]
                        let nodeMap = makeNodeMap triviaPairsByTree.[t.FilePath]
                        let root = t.GetRoot()
                        let replacement = 
                            Func<'a, 'a, SyntaxNode>(
                                fun (o:'a) (o2:'a) -> 
                                    let trivia = nodeMap.[o.ToFullString()] |> Seq.head |> snd
                                    let newNode = o.WithLeadingTrivia(trivia)
                                    newNode :> SyntaxNode)
                        let newTree = root.ReplaceNodes(originalNodes, replacement).SyntaxTree
                        newTree
                    else
                        t)
            trees

    let replaceTrivia<'a when 'a :> SyntaxNode> treesByProject (triviaPairsByTree:Map<string, seq<('a * SyntaxTriviaList)>>) =
        Map.map
            (fun _ (v:seq<SyntaxTree>) -> 
                let newTrees = makeTreesWithChangedTrivia<'a> v triviaPairsByTree
                newTrees.ToList() :> IEnumerable<SyntaxTree>)
            treesByProject

    type Runner(count) =  
        let testCount = count
        let mutable results = new List<(string * string * Outcome)>()
        let mutable tested = 0
        let mutable prefix = ""
        let testsComplete = new Event<_>()

        member this.Results
            with get () = results

        [<CLIEvent>]
        member this.TestsComplete = testsComplete.Publish
                        
        interface IRunner with
            member this.OnArguments(arg1: int, arg2: obj list, arg3: int -> obj list -> string) = ()
            member this.OnFinished(testName: string, testResult: TestResult) =
                match testResult with
                    | TestResult.False (d, l1, l2, o, s) ->
                        let name = testName.Split('.').[1]
                        let identifier = prefix + "." + name
                        let inputList = 
                            Seq.map 
                                (fun l -> 
                                    if l <> null
                                    then l.ToString()
                                    else "null")
                                l1
                        let inputs = String.Join(", ", inputList.ToArray())
                        let error = (identifier, inputs, o)
                        results.Add(error)

                        tested <- tested + 1
                        if tested = testCount 
                            then 
                                testsComplete.Trigger(results)
                            else 
                                ()
                    | _ ->
                        tested <- tested + 1
                        if tested = testCount 
                            then 
                                testsComplete.Trigger(results)
                            else
                                ()
            member this.OnShrink(arg1: obj list, arg2: obj list -> string) = ()
            member this.OnStartFixture(fixtureName: Type) = 
                prefix <- fixtureName.ToString().Replace('+', '.')

    and Generator() = 
        // these are mutable so that they can be used for caching, but...   
        static let mutable solution = null
        static let mutable allCompilations = null
        static let mutable allCompilationsByProject = Map []
        static let mutable treeToCompilation = Map []
        static let mutable testTreeCacheByProject = new Dictionary<string, Map<string, SyntaxTree>>()
        static let mutable systemReferences = seq []
        let mutable nameLocationMap = Map [] // ...why is this mutable?
        let doctestUsing = 
            SF.UsingDirective(
                SF.QualifiedName(
                    SF.IdentifierName("Doctest"), 
                    SF.IdentifierName("Interactive")))
             .WithStaticKeyword(
                SF.Token(SyntaxKind.StaticKeyword))
        let fscheckUsing = SF.UsingDirective(SF.IdentifierName("FsCheck"))
        let testsComplete = new Event<_>()
        let errorOccurred = new Event<_>()
        let syntaxError = "SYNTAX ERROR"
        let referenceError = "REFERENCE ERROR"
        let initialCompilationError = "INITIAL COMPILATION ERROR"

        // try to make assembly streams, and emit syntax errors as events if there are any.
        let makeAssemblyStream 
            (compilation:Compilation) 
            (locationPairs:seq<(Location * Location)>) 
            (context:SyntaxTreeAnalysisContext) 
            (offsetsByTree:Map<string, seq<offset>>) = 
            use ms = new MemoryStream()
            let result = compilation.Emit ms
            if result.Success 
                then 
                    ms.Seek(0L, SeekOrigin.Begin) |> ignore
                    ms.ToArray()
                else
                    let rootError = 
                        Seq.filter
                            (fun (d:Diagnostic) -> d.Severity = DiagnosticSeverity.Error)
                            result.Diagnostics
                        |> Seq.head
                    let offsets = offsetsByTree.[rootError.Location.SourceTree.FilePath]

                    let errorLocations = getSourceErrorLocations (seq [rootError]) locationPairs offsets
                    errorOccurred.Trigger((errorLocations, context)) // our heuristic to avoid "error pollution"
                    // is to only take the first error, for now...

                    if rootError.Id = "CS0246" || rootError.Id = "CS0234" || rootError.Id = "CS1061"
                        then failwith referenceError
                        else failwith syntaxError // it would probably be better to handle this with 
                    // first-class control flow, rather than with exceptions...

        let makeCachedSystemReferences = 
            if Seq.length systemReferences = 0
                then 
                    let types = [ 
                        typeof<obj>; 
                        typeof<Check>; 
                        typeof<Runner>; 
                        typeof<unit>;]
                    systemReferences <- 
                        Seq.map 
                            (fun (t:Type) -> 
                                let location = t.Assembly.Location
                                let reference = MetadataReference.CreateFromFile location
                                reference :> MetadataReference)
                            types
                else
                    ()

        let updateTestTreeCache newTreesByProject generateAll invalidateCache = 
            let cachedCount = 
                Seq.map
                    (fun (kvp:KeyValuePair<string, Map<string, SyntaxTree>>) -> Map.count kvp.Value)
                    testTreeCacheByProject
                |> Seq.sum

            let newCount = 
                Seq.map
                    (fun (kvp:KeyValuePair<string, seq<SyntaxTree>>) -> Seq.length kvp.Value)
                    newTreesByProject
                |> Seq.sum

            if cachedCount < newCount || generateAll || invalidateCache
                then 
                    let makePathTreePairs trees = 
                        Seq.map
                            (fun (t:SyntaxTree) -> (t.FilePath, t))
                            trees
                    let treesByPath = 
                        Map.map
                            (fun _ v -> makePathTreePairs v |> Map.ofSeq)
                            newTreesByProject
                    testTreeCacheByProject <- Dictionary(treesByPath)
                else 
                    let updateCache trees projectName =
                        Seq.iter
                            (fun (t:SyntaxTree) -> 
                                let mutable existing = testTreeCacheByProject.[projectName]
                                existing <- existing.Remove(t.FilePath)
                                testTreeCacheByProject.[projectName] <- existing.Add(t.FilePath, t))
                            trees
                    Map.iter
                        (fun k v -> updateCache v k)
                        newTreesByProject

        let makeCachedSolutionAndAllCompilations 
            (slnPath:string) 
            (context:SyntaxTreeAnalysisContext) 
            invalidateCache = 
            let mapContainsTree = treeToCompilation.ContainsKey(context.Tree.FilePath)
            let shouldRebuild = solution = null || not mapContainsTree || invalidateCache
            if shouldRebuild
                then 
                    try
                        solution <- makeSolution slnPath |> Async.RunSynchronously
                        allCompilations <- makeCompilations solution.Projects |> Async.RunSynchronously
                        allCompilationsByProject <-
                            Seq.groupBy 
                                (fun (c:Compilation) -> c.AssemblyName)
                                allCompilations
                            |> Map.ofSeq

                        let makePathCompilationPairs trees compilation =
                            Seq.map
                                (fun (t:SyntaxTree) -> (t.FilePath, compilation))
                                trees
                        treeToCompilation <-
                            Seq.collect
                                (fun (c:Compilation) -> makePathCompilationPairs c.SyntaxTrees c)
                                allCompilations
                            |> Map.ofSeq
                        true
                    with
                        | ex -> false // will throw if a build for any project is already in progress, so simply catch and return false
                else true

        member this.SyntaxError = syntaxError
        member this.ReferenceError = referenceError
        member this.InitialCompilationError = initialCompilationError

        [<CLIEvent>]
        member this.TestsComplete = testsComplete.Publish
        [<CLIEvent>]
        member this.ErrorOccurred = errorOccurred.Publish

        member this.Generate (slnPath:string) (context:SyntaxTreeAnalysisContext) generateAll invalidateCache = 
            // in here, make all compilations, AND make treeToCompilation!
            let isInitialCompilationSuccessful = makeCachedSolutionAndAllCompilations slnPath context invalidateCache
            if isInitialCompilationSuccessful
                then ()
                else failwith initialCompilationError
            
            let shouldInvalidate = generateAll || testTreeCacheByProject.Count = 0 || invalidateCache
            // here, just either use the compilations cached using makeCachedSolution, or
            // key into treeToCompilation
            let compilations = 
                if shouldInvalidate
                    then makeCompilations solution.Projects |> Async.RunSynchronously
                    else [|treeToCompilation.[context.Tree.FilePath]|]

            let affectedTreesByProject = 
                if shouldInvalidate
                    then 
                        Seq.map
                            (fun (c:Compilation) -> 
                                (c.AssemblyName, c.SyntaxTrees))
                            compilations
                        |> Map.ofSeq
                    else
                        [(treeToCompilation.[context.Tree.FilePath].AssemblyName, seq [context.Tree])]
                        |> Map.ofSeq

            // here, filter out trees that we don't care about?
            // but then how will we compile later without all trees?  or do we need all trees?
            // think about this.
            let treesForMethods = 
                Seq.collect
                    (fun (kvp:KeyValuePair<string, IEnumerable<SyntaxTree>>) -> kvp.Value)
                    affectedTreesByProject

            // should filter our non-spec/refinement pairs here...need to figure out how to do this.
            let triviaForMethods = getTriviaPairs<MethodDeclarationSyntax> treesForMethods
            let triviaForProperties = getTriviaPairs<PropertyDeclarationSyntax> treesForMethods
            let triviaForConstructors = getTriviaPairs<ConstructorDeclarationSyntax> treesForMethods
            let combinedTrivia = 
                Seq.append 
                    triviaForMethods 
                    triviaForProperties
                |> Seq.append 
                    triviaForConstructors

            // eventually, this should process both methods and classes -- so it would be 
            // (MemberDeclarationSyntax * SyntaxTriviaList)
            let triviaPairsForMethods =
                Seq.map
                    (fun (p:(MemberDeclarationSyntax * SyntaxTriviaList)) ->
                        let t = snd p
                        let n = fst p
                        let triviaString = t.ToFullString()
                        let triviaStringTransformed =
                            transformXml "spec" triviaString
                            |> transformXml "refinement"
                        let triviaTransformed = SF.ParseLeadingTrivia(triviaStringTransformed)
                        let t' = triviaTransformed
                        (n, t'))
                    combinedTrivia
            let triviaPairsByTreeForMethods = 
                Seq.groupBy
                    (fun (p:(MemberDeclarationSyntax * SyntaxTriviaList)) -> (fst p).SyntaxTree.FilePath)
                    triviaPairsForMethods
                |> Map.ofSeq
                        
            let compilationsByProject = 
                Seq.groupBy 
                    (fun (c:Compilation) -> c.AssemblyName)
                    compilations
                |> Map.ofSeq

            // again, we eventually need this to be for both 
            // MemberDeclarationSyntax AND ClassDeclarationSyntax...
            let treesWithReplacedTriviaForMethodsByProject = 
                replaceTrivia<MemberDeclarationSyntax> 
                    affectedTreesByProject
                    triviaPairsByTreeForMethods

            let treesForClasses = 
                Seq.collect
                    (fun (kvp:KeyValuePair<string, seq<SyntaxTree>>) -> kvp.Value)
                    treesWithReplacedTriviaForMethodsByProject
            let triviaForClasses = getTrivia<ClassDeclarationSyntax> treesForClasses
            let triviaPairsForClasses =
                Seq.map
                    (fun (p:(ClassDeclarationSyntax * SyntaxTriviaList)) ->
                        let t = snd p
                        let n = fst p
                        let triviaString = t.ToFullString()
                        let triviaStringTransformed =
                            transformXml "spec" triviaString
                            |> transformXml "refinement"
                        let triviaTransformed = SF.ParseLeadingTrivia(triviaStringTransformed)
                        let t' = triviaTransformed
                        (n, t'))
                    triviaForClasses
            let triviaPairsByTreeForClasses = 
                Seq.groupBy
                    (fun (p:(ClassDeclarationSyntax * SyntaxTriviaList)) -> (fst p).SyntaxTree.FilePath)
                    triviaPairsForClasses
                |> Map.ofSeq

            let treesWithReplacedTriviaByProject = 
                replaceTrivia<ClassDeclarationSyntax>
                    treesWithReplacedTriviaForMethodsByProject
                    triviaPairsByTreeForClasses
            // ^^^ now we'll need to create the snippets from this, etc....
            
            // find the references that are used by each project.  a reference could be a direct reference,
            // or it could be a project reference.  if it's a project reference, we find the actual assembly
            // to reference by simply looking in the project's bin folder.
            let referencesByProject = getReferencesByProject solution allCompilationsByProject
    
            // for each project, find all doctest snippets, and associate them with the classes in 
            // which they are contained, and also with the namespace in which those classes are contained.
            let rawSnippetClassPairsByProject = 
                makeSnippetClassPairs<MemberDeclarationSyntax>
                    treesWithReplacedTriviaByProject
                    "spec"
                    makeMethodPair
            // modulo snippets that failed to compile
            let snippetClassPairsWithFilteredSnippets = 
                replaceWithFilteredSnippets 
                    rawSnippetClassPairsByProject 
                    (fun snippet _ -> "public static " + snippet)
            let snippetClassPairsByProject = removeEmptySnippets snippetClassPairsWithFilteredSnippets
            
            // vvv FOR CLASSES (FsCheck Arbs)

            // NOTE: (this also goes for the method use-case) We construct the actual method node for validation,
            //       so why don't we just save that for later use?  That way we wouldn't have to construct it
            //       again.

            let rawSnippetClassPairsByProjectForClasses = 
                makeSnippetClassPairs<ClassDeclarationSyntax> 
                    treesWithReplacedTriviaByProject 
                    "refinement" 
                    makeClassPair
            let transform (snippet:string) (metadata:snippetsWithMetadata) = 
                "public static " + snippet

            let snippetClassPairsWithFilteredSnippetsForClasses = 
                replaceWithFilteredSnippets 
                    rawSnippetClassPairsByProjectForClasses 
                    transform
            let snippetClassPairsByProjectForClasses = removeEmptySnippets snippetClassPairsWithFilteredSnippetsForClasses

            let transformToMethod snippet metadata =
                let methodText = transform snippet metadata
                let maybeMethod = makeMethodFromSnippetForValidation methodText
                // we can assume that .Value will never throw, because at
                // this point in the program, we know that we've filtered
                // out all of the invalid method definitions
                maybeMethod.Value 

            // ^^^ FOR CLASSES (FsCheck Arbs)
                                            
            let filterByTreeMembership (t:SyntaxTree) (nodes:seq<SyntaxNode>) = 
                let descendants = t.GetRoot().DescendantNodes()
                Seq.filter 
                    (fun n -> descendants.Contains(n))
                    nodes

            // construct the new syntax trees for each project.  these new trees will include the
            // test methods that contain our doctests and class generators for fscheck.
            let makeNewTrees projMethods projClasses treesForCompilation =
                let nodePairsForMethods = 
                    Seq.map 
                        (fun (p:snippetsWithMetadata) -> 
                            let namespacesFromTopToBottom = getMatchingNodesInSubtree<ClassDeclarationSyntax> p.namespaceNode
                            let lastClass = Seq.last namespacesFromTopToBottom
                            let keywords = [SF.Token(SyntaxKind.PublicKeyword)]
                            let nameMaker (metadata:snippetsWithMetadata) = 
                                let name = getName metadata.methodNode
                                name + "_Doctest"
                            let transformer snippet _ = makeMethodFromSnippet snippet
                            let testMethod = makeTestClass p keywords nameMaker transformer
                            { LastNode = lastClass; TestNode = testMethod })
                        projMethods
                let nodePairsForClasses = 
                    Seq.map 
                        (fun (p:snippetsWithMetadata) -> 
                            let namespacesFromTopToBottom = getMatchingNodesInSubtree<ClassDeclarationSyntax> p.namespaceNode
                            let lastNamespace = Seq.last namespacesFromTopToBottom
                            let keywords = [SF.Token(SyntaxKind.PublicKeyword); SF.Token(SyntaxKind.PartialKeyword)]
                            let nameMaker _ = makeArbClassName p
                            let testClass = makeTestClass p keywords nameMaker transformToMethod
                            { LastNode = lastNamespace; TestNode = testClass })
                        projClasses
                let nodePairsForTests = Seq.append nodePairsForMethods nodePairsForClasses

                // where doesn't contain "// <auto-generated" OR "// <autogenerated"
                // see https://github.com/DotNetAnalyzers/StyleCopAnalyzers/issues/1430 for details
                let nonAutogeneratedTrees = 
                    Seq.filter
                        (fun (t:SyntaxTree) -> 
                            let treeText = t.ToString()
                            not (treeText.Contains("// <auto-generated")) && not (treeText.Contains("// <autogenerated")))
                        treesForCompilation

                let nodePairsForStandardUsings = 
                    Seq.map
                        (fun t -> 
                            // what if there are no usings present in the tree?  the way i handle this (with options)
                            // breaks the use-case in which no 'usings' are present in the file, but that should be very rare.
                            // actual solution to this is to getroot and then insert BEFORE that, rather than AFTER.
                            // the problem with that approach is that it would require special handling of inserting 
                            // these 'using' nodes...

                            let usings = getMatchingNodesInTree<UsingDirectiveSyntax> t
                            if Seq.isEmpty usings
                                then None
                                else 
                                    let firstUsing = Seq.head usings
                                    let doctestPair = { LastNode = firstUsing; TestNode = doctestUsing }
                                    let fscheckPair = { LastNode = firstUsing; TestNode = fscheckUsing }
                                    Some [doctestPair; fscheckPair])
                        nonAutogeneratedTrees
                    |> Seq.choose id // turn list of options into a list of vals, exluding the Nones
                    |> Seq.collect
                        (fun p -> p)
                
                let nodePairs = Seq.append nodePairsForTests nodePairsForStandardUsings                

                let insertionNodes = 
                    Seq.map 
                        (fun (p:NodeSwap) -> p.LastNode) 
                        nodePairs
                let newTrees = 
                    Seq.map 
                        (fun (t:SyntaxTree) -> 
                            let validNodes = filterByTreeMembership t insertionNodes
                            let newRoot = t.GetRoot().TrackNodes(validNodes)
                            newRoot.SyntaxTree) 
                        treesForCompilation
                let transform (t:SyntaxTree) (last:SyntaxNode) (test:SyntaxNode) = 
                    let newRoot = t.GetRoot().InsertNodesAfter(last, [ test ])
                    newRoot.SyntaxTree

                Seq.map (fun t -> (apply t transform nodePairs)) newTrees 

            // for each compilation, find list of last methods in each class,
            // make test methods, insert tests after lasts, and return modified tree.
            let newTreesByProject = 
                Map.map 
                    (fun (k:string) (v:seq<SyntaxTree>) -> 
                        let projMethods = snippetClassPairsByProject.[k]
                        let projClasses = snippetClassPairsByProjectForClasses.[k]
                        let treesForCompilation = v
                        if Seq.isEmpty projMethods && Seq.isEmpty projClasses
                            then 
                                treesForCompilation
                            else
                                makeNewTrees projMethods projClasses treesForCompilation)
                    treesWithReplacedTriviaByProject

            let namespaceNameClassNamePairs = 
                Seq.collect
                    (fun (kvp:KeyValuePair<string, seq<snippetsWithMetadata>>) -> kvp.Value)
                    snippetClassPairsByProjectForClasses
                |> Seq.map
                    (fun (s:snippetsWithMetadata) -> (s.namespaceNode.Name.ToString(), makeArbClassName s))

            let combinedSnippets = 
                Map.map 
                    (fun k _ -> 
                        Seq.append 
                            snippetClassPairsByProject.[k] 
                            snippetClassPairsByProjectForClasses.[k])
                    snippetClassPairsByProject
            let makeIdLocationPairs snippetsAndMetadata = 
                Seq.map 
                    (fun snippet -> (makeId snippetsAndMetadata snippet.Spec, snippet.Location))
                    snippetsAndMetadata.snippets
            let makeIdLocationMap snippets = 
                Seq.collect 
                    (fun s -> makeIdLocationPairs s)
                    snippets 
                |> Map.ofSeq
            let snippetLocationsByAnnotationByProject =
                Map.map
                    (fun _ v -> makeIdLocationMap v)
                    combinedSnippets
            
            // for each proj, get the annotated nodes in each tree, 
            // and use the annotation data to key into snippetLocationsByAnnotationByProject...
            let testMethodLocationsByProject = 
                let makeNameLocationPairs nodes =
                    Seq.map 
                        (fun (n:SyntaxNode) -> 
                            let methodName = n.GetAnnotations("id").Single().Data
                            let methodLocation = n.GetLocation()
                            (methodName, methodLocation))
                        nodes
                let getTestMethodLocations ts =
                    Seq.collect
                            (fun (t:SyntaxTree) -> 
                                t.GetRoot().GetAnnotatedNodes("id") 
                                |> makeNameLocationPairs)
                            ts
                Map.map
                    (fun _ v -> getTestMethodLocations v)
                    newTreesByProject

            // sourceMapByProject is used to map syntax error locations to locations in the file being
            // edited, i.e. to doctest locations:
            // when emitting, get the location(s) of the error(s), and look in fst of each pair, and find
            // which methodLocation the error locations are inside of.  then do snd pair to get the snippet
            // location.  then do more computation to find the exact location within the snippet to color.
            let makeSourceMapPairs annotationLocationPairs projectName = 
                Seq.map
                    (fun p ->
                        let snippetLocationsByAnnotation = snippetLocationsByAnnotationByProject.[projectName]
                        let annotation = fst p
                        let snippetLocation = snippetLocationsByAnnotation.[annotation]
                        let methodLocation:Location = snd p
                        (methodLocation, snippetLocation))
                    annotationLocationPairs
            let sourceMapByProject =
                Map.map
                    (fun k (v:seq<(string * Location)>) -> makeSourceMapPairs v k)
                    testMethodLocationsByProject

            // offsets for methods and classes

            let snippetClassPairs = 
                Map.toSeq snippetClassPairsByProject
                    |> Seq.map snd 
                    |> Seq.collect id

            let flattenedSnippets = flattenSnippets snippetClassPairs

            let flattenedSnippetsByTree = 
                Seq.groupBy 
                    (fun s -> s.methodNode.SyntaxTree.FilePath)
                    flattenedSnippets
                |> Map.ofSeq
            let nameLocationOffsetMap = 
                Map.map
                    (fun _ v -> getNameLocationOffsetTriples v)
                    flattenedSnippetsByTree

            // classes should merge into nameLocationOffsetMap here, and then the
            // combined collection should be sorted.
            let snippetClassPairsForClasses = 
                Map.toSeq snippetClassPairsByProjectForClasses
                    |> Seq.map snd 
                    |> Seq.collect id
            let flattenedSnippetsForClasses = flattenSnippets snippetClassPairsForClasses

            // group by tree.FilePath -- make dict
            // then sort by location, then do the offset computation
            let flattenedSnippetsForClassesByTree =
                Seq.groupBy
                    (fun s -> s.classNode.SyntaxTree.FilePath)
                    flattenedSnippetsForClasses
                |> Map.ofSeq            
            
            let nameLocationOffsetMapForClasses = 
                Map.map
                    (fun _ v -> getOffsetTriplesWithBlankName v)
                    flattenedSnippetsForClassesByTree

            // some keys might only exist in the methods dict, and others might only exist
            // in the classes dict (because both method snippets and class snippets might
            // not necessarily exist in the same trees), so need to be careful when merging.
            let partiallyMergedNameLocationOffsetMap = 
                Map.map
                    (fun k v -> 
                        if (nameLocationOffsetMapForClasses.ContainsKey(k))
                            then Seq.append v nameLocationOffsetMapForClasses.[k]
                            else v)
                    nameLocationOffsetMap
            let exclusiveToClasses = 
                Map.filter
                    (fun k _ -> not (nameLocationOffsetMap.ContainsKey(k)))
                    nameLocationOffsetMapForClasses
            let mergedNameLocationOffsetMap = 
                Seq.append
                    (Map.toSeq exclusiveToClasses)
                    (Map.toSeq partiallyMergedNameLocationOffsetMap)
                |> Map.ofSeq

            let sortedNameLocationOffsetMap = 
                let sortByLocation nameLocationPairs = 
                    Seq.sortWith 
                        (fun (x:string * (Location * int)) (y:string * (Location * int)) ->
                            let xLocation = snd x |> fst
                            let yLocation = snd y |> fst
                            if xLocation.SourceSpan.Start < yLocation.SourceSpan.Start
                                then -1
                                else 1) 
                        nameLocationPairs
                Map.map 
                    (fun _ v -> sortByLocation v)
                    mergedNameLocationOffsetMap

            let computedOffsetsByTree =
                let aggregateOffsets firstOffset nameLocationPairs = 
                    Seq.scan
                        (fun (running:offset) (t:(string * (Location * int))) -> 
                            let offset = snd t |> snd
                            let location = snd t |> fst
                            { name = fst t; location = location; runningOffset = running.runningOffset + running.offset; offset = offset })
                        { name = ""; location = null; runningOffset = 0; offset = firstOffset }
                        nameLocationPairs
                Map.map
                    (fun _ (v:seq<string * (Location * int)>) ->
                        let (firstName, firstLocationPair) = v.First()
                        let firstLocation = fst firstLocationPair
                        let firstOffset = snd firstLocationPair
                        let computedOffsets = aggregateOffsets firstOffset (v.Skip(1))
                        // then need to add back the first el to the front of computedOffsets
                        let firstComputedOffset = { name = firstName; location = firstLocation; runningOffset = 0; offset = firstOffset}
                        let allComputedOffsets = Seq.append [firstComputedOffset] (computedOffsets.Skip(1))
                        allComputedOffsets)
                    sortedNameLocationOffsetMap
            // unwrap/collect from dict and then put into nameLocationMap
            let flattenedOffsets = 
                Seq.collect
                    (fun (kvp:KeyValuePair<string, seq<offset>>) -> kvp.Value)
                    computedOffsetsByTree
            nameLocationMap <- 
                Seq.map
                    (fun o -> (o.name, o))
                    flattenedOffsets
                |> Map.ofSeq
            
            // invalidate/replace newly edited tree in cache
            updateTestTreeCache newTreesByProject generateAll invalidateCache |> ignore
            
            // create references that are required by the test driver console app --
            // these references are just mscorlib, which is the minimal reference required by a console
            // app, along with the references to our test assemblies.  and also the FsCheck assembly.
            makeCachedSystemReferences |> ignore

            // for each project, make a new compilation using the new syntax trees which
            // contain the doctest methods.
            let testCompilationsByProject = 
                Map.map 
                    (fun k _ ->
                        let testCompilationName = k + "_Doctest" + Guid.NewGuid().ToString()
                        let trees = Seq.map (fun (kvp:KeyValuePair<string, SyntaxTree>) -> kvp.Value) testTreeCacheByProject.[k]
                        CSharpCompilation.Create(
                            testCompilationName,
                            trees, 
                            (Seq.append referencesByProject.[k] systemReferences),
                            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)))
                    compilationsByProject
            //let newTreeExample = testCompilationsByProject.First().Value.SyntaxTrees.First().GetText().ToString()
            // write each compilation (there will be one per project being tested) to a dll
            // in memory
            let testAssemblyStreamsByProject = 
                Seq.map 
                    (fun (kvp:KeyValuePair<string,CSharpCompilation>) -> 
                        let assemblyName = kvp.Value.AssemblyName
                        let assemblyBytes = makeAssemblyStream kvp.Value sourceMapByProject.[kvp.Key] context computedOffsetsByTree
                        (assemblyName, assemblyBytes)) 
                    testCompilationsByProject
                |> Map.ofSeq

            // vvv test driver construction

            // what if the user gives 2 or more tests the exact same name?
            // i think that would cause an error...
            let testCountByMethod = 
                Seq.map 
                    (fun s -> (makeFullyQualifiedName s, Seq.length s.snippets))
                    snippetClassPairs 
                |> Map.ofSeq

            let testsCompleteCallback testMetadata = 
                if Seq.isEmpty testMetadata
                    then 
                        ()
                    else 
                        let locationWithCounterexample = 
                            Seq.map 
                                (fun metadata -> 
                                    let name, input, exc = metadata
                                    (nameLocationMap.[name], (input, exc)))
                                testMetadata
                        testsComplete.Trigger((locationWithCounterexample, context))
            let configsByMethod = 
                Map.map 
                    (fun _ v -> 
                        let r = new Runner(v)
                        r.TestsComplete.Add(testsCompleteCallback)
                        { Config.Default with Runner = r }) 
                    testCountByMethod
            
            // we load the test assembly(s) into a new appdomain, 
            // because we wouldn't be able to unload it later, otherwise.
            // it's important to unload it later, because otherwise we'll end up having
            // many different versions of the test assembly loaded, and that could cause 
            // problems when resolving types, and would also be a memory leak.
            
            let adsetup = new AppDomainSetup()
            adsetup.ApplicationBase <- System.Environment.CurrentDirectory
            let adevidence = AppDomain.CurrentDomain.Evidence
            let domain = 
                AppDomain.CreateDomain(
                    Guid.NewGuid().ToString(),
                    adevidence, 
                    adsetup)
            
            // help the CLR to resolve the test assembly(s)
            AppDomain.CurrentDomain.add_AssemblyResolve(
                ResolveEventHandler 
                    (fun _ args ->
                        let name = args.Name.Split(',').[0]
                        let isNameInTestAssembly = testAssemblyStreamsByProject.ContainsKey name
                        if isNameInTestAssembly 
                            then Assembly.Load(testAssemblyStreamsByProject.[name])
                            else null))

            let testAssembliesByProject = 
                Map.map
                    (fun _ (v:byte[]) -> domain.Load(v))
                    testAssemblyStreamsByProject

            let makeClrName (s:string) = 
                let pieces = s.Split('.')
                let originatingClass = pieces.[1]
                s.Replace(originatingClass + ".", "")

            let getType name (assembliesByProject:Map<string, Assembly>) = 
                let containingAssembly = 
                    Map.map 
                        (fun _ (v:Assembly) ->
                            let types = v.GetTypes()
                            let typeNames = 
                                Seq.map
                                    (fun (t:Type) -> t.FullName)
                                    types
                            typeNames.Contains(name))
                        assembliesByProject 
                    |> Seq.filter (fun kvp -> kvp.Value = true) 
                    |> Seq.head

                let containingAssemblyName = containingAssembly.Key
                assembliesByProject.[containingAssemblyName].GetType(name)

            // these are the nested classes which contain our spec/tests
            let types = 
                Seq.map 
                    (fun (kvp:KeyValuePair<string, Config>) ->
                        let clrName = makeClrName kvp.Key
                        let testType = getType clrName testAssembliesByProject
                        (testType, kvp.Value))
                    configsByMethod
            // these are the types which contain our FsCheck Generator definitions,
            // which help FsCheck to know how to generate test data for custom types.
            let arbTypes = 
                Seq.map
                    (fun p -> 
                        let name = (fst p) + "." + (snd p)
                        getType name testAssembliesByProject)
                    namespaceNameClassNamePairs
            
            // run all Arb.Register methods
            Seq.iter 
                (fun (t:Type) -> 
                    let empty = [||]
                    let method = typeof<Arb.Default>.DeclaringType.GetMethod("Register", empty)
                    let genericMethod = method.MakeGenericMethod(t)
                    genericMethod.Invoke(this, empty) |> ignore) 
                arbTypes

            // run all Check.All methods
            Seq.iter 
                (fun (p:(Type * Config)) -> 
                    let method = typeof<Check>.GetMethod("All", [| typeof<Config> |])
                    let genericMethod = method.MakeGenericMethod(fst p)
                    genericMethod.Invoke(this, [| snd p |]) |> ignore) 
                types
            
            AppDomain.Unload(domain)