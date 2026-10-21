namespace YAMLicious

/// Constructors for the comments of the tree.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlComment =

    /// A comment that a program constructs. `text` is everything after the `#`, so a comment written as
    /// `# note` has the text " note". The comment has no span.
    let create (text: string) : YamlComment =
        { Text = text
          Span = None }

/// Constructors and the identifier of tags.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlTag =

    /// The prefix of the tags that the YAML specification defines, which `!!` stands for by default.
    let coreNamespace = "tag:yaml.org,2002:"

    // The identifier of a tag with an empty suffix would read as another tag. `!` alone is the non-specific
    // tag, and `tag:yaml.org,2002:` alone names nothing.
    let private requireName (suffix: string) =
        if System.String.IsNullOrEmpty suffix then
            failwith "A tag needs a name behind its handle."

    /// A tag of the core schema, as `core "str"` for `!!str`. The suffix must not be empty.
    let core (suffix: string) : YamlTag =
        requireName suffix
        YamlTag.Named
            { Prefix = coreNamespace
              Suffix = suffix }

    /// A local tag, as `local "custom"` for `!custom`. The suffix must not be empty, because `!` alone is
    /// the non-specific tag.
    let local (suffix: string) : YamlTag =
        requireName suffix
        YamlTag.Named
            { Prefix = "!"
              Suffix = suffix }

    /// The full identifier of the tag: the prefix and the suffix joined, or `!` for the non-specific tag.
    let identifier (tag: YamlTag) : string =
        match tag with
        | YamlTag.NonSpecific -> "!"
        | YamlTag.Named name -> name.Prefix + name.Suffix
