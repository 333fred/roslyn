Outline:

* Intro
* Show backing attributes/extraneous `abstract` diff
* BoundNodes -> `closed`
    * src/Compilers/CSharp/Portable/BoundTree/BoundNode.cs:15
    * src/Tools/CompilerGeneratorTools/Source/BoundTreeGenerator/BoundNodeClassWriter.cs:300
    * `./eng/generate-compiler-code.cs`
    * Show generated diff
    * Fixup CSharpOperationFactory
        * src/Compilers/CSharp/Portable/Operations/CSharpOperationFactory.cs:1550
        * src/Compilers/CSharp/Portable/Operations/CSharpOperationFactory.cs:2313
* LocalRewriter simple union example:
    * src/Compilers/CSharp/Portable/Lowering/LocalRewriter/LocalRewriter_IndexerAccess.cs:113
    * Create `union IndexerOrInitializer(BoundIndexerAccess, BoundObjectInitializerMember)`
    * Mark readonly
    * Swap in for BoundExpression
    * Make `Type` property for convenience
    * Show creation sites
* `OneOrMany` complex union example:
    * Add the attribute
    * Create IUnionMembers (full spit below)
        * Start with create and Value prop
        * Mention boxing issues
        * Implement TryGetValue
    * Remove HasOneItem
    * Start moving a few representative examples to unions:
        * Add
        * AddRange
        * CastUp
        * SequenceEqual
    * Copy/paste the full new file in, talk_notes_oneormany.md
* Labeled break/continue
    * Convert the for loop to use labeled break
    * Other window
* Extension indexer
    * Move ElementAt to indexer
    * Other window
    * Add an element to the collection and use `with(4)`
    * `strings` HashSet with ignorecase comparer
* Unsafe
    * Thank #allow-unsafe-blocks for their horrific contributions
    * 


```cs
        public interface IUnionMembers
        {
            public static OneOrMany<T> Create(T one)
            {
                return OneOrMany.Create(one);
            }

            public static OneOrMany<T> Create(ImmutableArray<T> many)
            {
                return OneOrMany.Create(many);
            }

            public object Value { get; }
            public bool TryGetValue([MaybeNullWhen(false)] out T one);
            public bool TryGetValue(out ImmutableArray<T> many);
        }
```
