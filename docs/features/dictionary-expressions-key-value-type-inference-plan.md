# Dictionary expressions: key-value type inference plan

This plan covers the remaining prototype work for dictionary-expression method type inference, specifically key/value lower-bound and output inference for collection expressions whose target element type is `KeyValuePair<TKey, TValue>`.

The current implementation recognizes dictionary expressions and key-value pair conversions, but `src/Compilers/CSharp/Portable/Binder/Semantics/OverloadResolution/MethodTypeInference.cs` still skips key-value-shaped elements in collection-expression input and output inference:

* `MakeCollectionExpressionTypeInferences(...)` skips `BoundKeyValuePairElement` and `BoundKeyValuePairConversion`.
* `MakeOutputTypeInferences(... BoundUnconvertedCollectionExpression ...)` only recurses into `BoundExpression` elements and explicitly does not handle `key:value` elements.

The dedicated coverage is in `src/Compilers/CSharp/Test/CSharp15/DictionaryExpressionTests.cs`. `TypeInference` currently carries the expected failure for `Identity([1:default, default:"2"])` and should become the first passing regression test once input inference is implemented.

## Implementation outline

1. Add a `TypeWithAnnotations`-preserving `ConversionsBase.IsKeyValuePairType(...)` overload usable from method type inference.
   * It should accept `TypeWithAnnotations` and return key/value `TypeWithAnnotations`, not just `TypeSymbol`, so nullable annotations and tuple names flow through inference correctly.
   * It should reuse the existing `ConversionsBase.IsKeyValuePairType(...)` shape and avoid losing annotations by using `NamedTypeSymbol.TypeArgumentsWithAnnotationsNoUseSiteDiagnostics`.

2. Update input type inference for unconverted collection expressions.
   * After `TryGetCollectionIterationType(...)`, check whether the target element type is `KeyValuePair<K_e, V_e>`.
   * For `BoundKeyValuePairElement`, call the existing expression input-inference path separately:
     * `MakeExplicitParameterTypeInferences(binder, element.Key, K_e, kind, ref useSiteInfo)`
     * `MakeExplicitParameterTypeInferences(binder, element.Value, V_e, kind, ref useSiteInfo)`
   * For `BoundKeyValuePairConversion`, infer from its `Expression` source shape rather than from the converted `KeyValuePair<,>` as a single invariant struct.
   * For ordinary expression elements whose type is `KeyValuePair<K_i, V_i>`, make lower-bound inferences from `K_i` to `K_e` and from `V_i` to `V_e`.
   * For spread elements whose enumerator element type is `KeyValuePair<K_i, V_i>`, make lower-bound inferences from `K_i` to `K_e` and from `V_i` to `V_e`.
   * If the target element type is not `KeyValuePair<,>`, preserve existing collection-expression inference exactly: expression elements recurse to the element type, spread elements lower-bound the iteration type, and key-value elements do not contribute.

3. Update output type inference for unconverted collection expressions.
   * If the target element type is `KeyValuePair<K_e, V_e>`, recurse only into `BoundKeyValuePairElement.Key` and `.Value` with the corresponding key/value target types.
   * Do not add output inference for ordinary expression elements or spread elements in the `KeyValuePair<,>` path; the current proposal says they contribute no output inference there.
   * If the target element type is not `KeyValuePair<,>`, preserve existing behavior for expression elements and continue ignoring spread elements.

4. Be explicit about lower-bound vs exact behavior.
   * Do not rely on `LowerBoundInference(KeyValuePair<K_i,V_i>, KeyValuePair<K_e,V_e>)`: because `KeyValuePair<,>` is an invariant struct, existing lower-bound constructed inference would drive exact type-argument inference, which is not the dictionary-expression rule.
   * Key and value components should use the normal lower-bound machinery individually so reference conversions, nullable annotations, tuple element names, and variance inside nested generic types are handled by the existing algorithm.

5. Keep dependency analysis unchanged unless tests expose a cycle.
   * The current output-inference phase already gates on `HasUnfixedParamInOutputType(...)` and `HasUnfixedParamInInputType(...)` at the collection-expression argument level.
   * Key-value output inference should therefore plug into the existing phase rather than adding new dependency rules.

## Tests to add or update

1. Update `DictionaryExpressionTests.TypeInference` so `Identity([1:default, default:"2"])` succeeds for `IDictionary<K,V>`, `IReadOnlyDictionary<K,V>`, and `Dictionary<K,V>`, while the cases with only `default` for an unfixed key or value still fail.

2. Add input-inference cases for mixed element kinds:
   * `Identity(["mads": 21, kvp])` where `kvp` is `KeyValuePair<object, long>`, expecting `K=object`, `V=long`.
   * `Identity(["a": 1, ..existing])` where `existing` is `Dictionary<object, long>` or `KeyValuePair<object,long>[]`, expecting component lower-bound inference from the spread iteration type.
   * The reverse ordering of those elements to ensure inference is order independent.

3. Add output-inference cases matching the proposal:
   * `OutputType(["b": () => 2])` for `static void OutputType<T>(Dictionary<string, Func<T>> d)`.
   * A corresponding `IDictionary<string, Func<T>>` and `IReadOnlyDictionary<string, Func<T>>` target.
   * A negative case proving ordinary expression elements and spread elements do not contribute output inference in the `KeyValuePair<,>` path.

4. Preserve non-dictionary collection behavior.
   * Keep `TypeInference_KeyValuePairConversions` passing: when the method parameter is `IEnumerable<T>`, `T[]`, or `List<T>`, the compiler must not decompose `KeyValuePair<K,V>` just because `T` later happens to be a `KeyValuePair<,>`.
   * Add a key-value element to a non-`KeyValuePair<,>` target, if not already covered, to confirm it still contributes no inference and fails through normal applicability/conversion diagnostics.

5. Cover nullable and tuple identity-equivalence cases.
   * `Dictionary<(int X, int Y), object?>` from elements whose key/value annotations or tuple element names differ should infer through the same component types used by conversion binding.
   * Include one nullable warning baseline if the new inference changes nullability diagnostics.

## Speclet follow-ups needed

The current proposal contains enough direction to implement the core behavior, but several details should be confirmed or tightened before removing prototype comments:

1. The type-inference section should state whether `T has an element type KeyValuePair<K_e,V_e>` includes all compiler-recognized dictionary interface targets, custom dictionary types with an indexer, collection-builder targets, array/span/list targets of `KeyValuePair<,>`, and nullable value-type wrappers. The implementation should match `TryGetCollectionIterationType(...)`, but the speclet should say that directly.

2. The input-inference rule for `BoundKeyValuePairConversion`-equivalent cases should clarify whether inference is based on the source element shape before conversion or on the converted `KeyValuePair<,>` type. The compiler should infer from source key/value components so it does not collapse to invariant `KeyValuePair<,>` exact inference.

3. The output-inference rule says expression and spread elements contribute no inference in the `KeyValuePair<,>` path. Confirm that this intentionally excludes lambdas or method groups inside `KeyValuePair` expression elements, even if the target value type is a delegate containing an unfixed type parameter.

4. The speclet should include examples for collection-builder dictionary targets and custom dictionary types with indexers, not just `Dictionary<,>` and dictionary interfaces, so implementation decisions are testable against the intended target set.

5. The existing prototype comments about `IReadOnlyDictionary<TKey,TValue>` construction with `Dictionary<TKey,TValue>` comparer constructors are adjacent but separate from type inference. The collection-expression-arguments speclet should explicitly confirm whether `with(IEqualityComparer<TKey>)` for read-only dictionary interface targets is specified as construction of a concrete `Dictionary<TKey,TValue>` followed by `ReadOnlyDictionary<TKey,TValue>` wrapping.

## Suggested implementation sequence

1. Add failing tests for input inference and output inference in `DictionaryExpressionTests`.
2. Implement annotated `KeyValuePair<,>` component extraction through `ConversionsBase`.
3. Implement input inference for key-value elements, key-value expression elements, and spread iteration elements.
4. Implement output inference for key-value elements only.
5. Update existing baselines and add targeted negative tests for non-dictionary collection behavior.
6. Remove the two `PROTOTYPE` comments in `MethodTypeInference.cs` once tests cover the finalized behavior.
7. Follow up on the speclet questions above before removing adjacent constructor/ref-like prototype comments in `Binder_Conversions.cs`.
