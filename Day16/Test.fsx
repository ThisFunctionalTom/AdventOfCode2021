type DependencyManagerAttribute() =
  inherit System.Attribute()

module Attributes =
    [<assembly: DependencyManagerAttribute()>]
    do ()


/// The results of ResolveDependencies
type ResolveDependenciesResult (success: bool, stdOut: string array, stdError: string array, resolutions: string seq, sourceFiles: string seq, roots: string seq) =

    /// Succeded?
    member _.Success = success

    /// The resolution output log
    member _.StdOut = stdOut

    /// The resolution error log (* process stderror *)
    member _.StdError = stdError

    /// The resolution paths
    member _.Resolutions = resolutions

    /// The source code file paths
    member _.SourceFiles = sourceFiles

    /// The roots to package directories
    member _.Roots = roots[<DependencyManager>]

type PaketDependencyManagerProvider(outputDir: string option) =
  member x.Name = "test"
  member x.Key = "test"
  member x.ResolveDependencies(scriptDir: string, mainScriptName: string, scriptName: string, packageManagerTextLines: string seq, targetFramework: string) : ResolveDependenciesResult =
    ResolveDependenciesResult(false, [|"Hello, World"|], [||], [], [], [])