// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Linq;
using System.IO;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.Test.Utilities;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests;

public class UnionCaseAttributesTests : CSharpTestBase
{
    [Fact]
    public void AttributesOnDifferentCases()
    {
        var source = """
            using System;

            [AttributeUsage(AttributeTargets.Parameter)]
            public class AAttribute : Attribute
            {
                public AAttribute(int value) { }
            }
            [AttributeUsage(AttributeTargets.Parameter)]
            public class BAttribute : Attribute;
            public class DefaultAttribute : Attribute;

            public union U([A(1)] int, [param: B] string, bool, [Default] long);
            """;

        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        verify(compilation.GetTypeByMetadataName("U"));
        foreach (var reference in new MetadataReference[] { compilation.ToMetadataReference(), compilation.EmitToImageReference() })
        {
            var consumer = CreateCompilation("", references: [reference], parseOptions: TestOptions.Regular15);
            consumer.VerifyEmitDiagnostics();
            verify(consumer.GetTypeByMetadataName("U"));
        }

        static void verify(NamedTypeSymbol type)
        {
            var constructors = type.InstanceConstructors.Where(c => c.Parameters.Length == 1).ToArray();
            var intParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Int32).Parameters[0];
            var stringParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_String).Parameters[0];
            var boolParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Boolean).Parameters[0];
            var longParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Int64).Parameters[0];

            var attribute = Assert.Single(intParameter.GetAttributes(), a => a.AttributeClass.Name == "AAttribute");
            Assert.Equal("AAttribute", attribute.AttributeClass.Name);
            Assert.Equal(1, attribute.ConstructorArguments.Single().Value);
            var stringAttribute = Assert.Single(stringParameter.GetAttributes(), a => a.AttributeClass.Name == "BAttribute");
            if (attribute.ApplicationSyntaxReference is { } sourceAttribute)
            {
                Assert.Equal("A(1)", sourceAttribute.GetSyntax().ToString());
                Assert.Equal("B", stringAttribute.ApplicationSyntaxReference.GetSyntax().ToString());
            }
            else
            {
                Assert.Null(stringAttribute.ApplicationSyntaxReference);
            }

            Assert.Single(longParameter.GetAttributes(), a => a.AttributeClass.Name == "DefaultAttribute");
            Assert.DoesNotContain(boolParameter.GetAttributes(), a => a.AttributeClass.Name is "AAttribute" or "BAttribute" or "DefaultAttribute");
            Assert.DoesNotContain(type.GetAttributes(), a => a.AttributeClass.Name is "AAttribute" or "BAttribute" or "DefaultAttribute");
            Assert.All(type.InstanceConstructors, c => Assert.DoesNotContain(c.GetAttributes(), a => a.AttributeClass.Name is "AAttribute" or "BAttribute" or "DefaultAttribute"));
            Assert.All(type.GetMembers("Value"), m => Assert.DoesNotContain(m.GetAttributes(), a => a.AttributeClass.Name is "AAttribute" or "BAttribute" or "DefaultAttribute"));
        }
    }

    [Theory]
    [InlineData("int value", "value")]
    [InlineData("int name = 0", "name")]
    public void RejectedCaseNameDoesNotChangeTheParameter(string caseText, string rejectedToken)
    {
        var source = $$"""
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] {{caseText}});
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_UnexpectedToken, rejectedToken).WithArguments(rejectedToken)
                .WithLocation(100, 13 + caseText.IndexOf(rejectedToken, System.StringComparison.Ordinal)));
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Equal("value", parameter.Name);
        Assert.False(parameter.IsOptional);
        Assert.False(parameter.HasExplicitDefaultValue);
    }

    [Fact]
    public void MissingDefaultExpressionAfterAttributedCase()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] int = );
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "=").WithArguments("=").WithLocation(100, 17),
            Diagnostic(ErrorCode.ERR_InvalidExprTerm, ")").WithArguments(")").WithLocation(100, 19));
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.False(parameter.IsOptional);
        Assert.Single(parameter.GetAttributes());
    }

    [Theory]
    [InlineData("union U([A], [B] string);", 0, ",")]
    [InlineData("union U(bool, [A], [B] string);", 1, ",")]
    [InlineData("union U(bool, [A]);", 1, ")")]
    public void MissingAttributedCaseTypeKeepsItsNeighbors(string declaration, int missingIndex, string token)
    {
        var source = $$"""
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class BAttribute : System.Attribute;
            #line 100
            {{declaration}}
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        var column = declaration.IndexOf("]", System.StringComparison.Ordinal) + 2;
        compilation.VerifyDiagnostics(Diagnostic(ErrorCode.ERR_TypeExpected, token).WithLocation(100, column));

        var tree = compilation.SyntaxTrees.First();
        var cases = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters;
        var model = compilation.GetSemanticModel(tree);
        var parameter = model.GetDeclaredSymbol(cases[missingIndex]);
        Assert.NotNull(parameter);
        Assert.Equal(TypeKind.Error, parameter.Type.TypeKind);
        Assert.Equal("AAttribute", Assert.Single(parameter.GetAttributes()).AttributeClass.Name);
        Assert.All(cases.Where((_, i) => i != missingIndex), p => Assert.NotNull(model.GetDeclaredSymbol(p)));
    }

    [Theory]
    [InlineData("ref", "ref")]
    [InlineData("out", "out")]
    [InlineData("in", "in")]
    [InlineData("ref readonly", "ref")]
    [InlineData("scoped ref", "scoped")]
    [InlineData("params", "params")]
    [InlineData("this", "this")]
    public void CaseParameterModifiersRemainInvalid(string modifiers, string firstModifier)
    {
        var source = $$"""
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] {{modifiers}} int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_UnexpectedToken, firstModifier).WithArguments(firstModifier).WithLocation(100, 13));
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Equal(RefKind.None, parameter.RefKind);
        Assert.False(parameter.IsParams);
        Assert.Single(parameter.GetAttributes());
    }

    [Fact]
    public void InvalidCaseTypeStillReportsConversionError()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] System.ArgIterator, [A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_NoImplicitConversionToObject, "System.ArgIterator").WithArguments("System.ArgIterator").WithLocation(100, 13));
    }

    [Theory]
    [InlineData("void")]
    [InlineData("int*")]
    [InlineData("delegate*<void>")]
    [InlineData("R")]
    public void InvalidCaseTypesWithAttributesMatchPlainCases(string type)
    {
        var declarations = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            ref struct R;
            """;
        var attributed = CreateCompilation([declarations, $"unsafe union U([A] {type});", UnionAttributeSource, IUnionSource],
            options: TestOptions.UnsafeReleaseDll, parseOptions: TestOptions.RegularPreview);
        var plain = CreateCompilation([declarations, $"unsafe union U({type});", UnionAttributeSource, IUnionSource],
            options: TestOptions.UnsafeReleaseDll, parseOptions: TestOptions.RegularPreview);
        var expected = GetComparableDiagnostics(plain);
        Assert.NotEmpty(expected);
        Assert.Equal(expected, GetComparableDiagnostics(attributed));
        var tree = attributed.SyntaxTrees.ElementAt(1);
        var syntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var parameter = attributed.GetSemanticModel(tree).GetDeclaredSymbol(syntax);
        Assert.NotNull(parameter);
        Assert.Single(parameter.GetAttributes());
    }

    [Fact]
    public void InvalidConstrainedCaseType()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] System.Nullable<string>);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_NoImplicitConversionToObject, "System.Nullable<string>").WithArguments("string?").WithLocation(100, 13),
            Diagnostic(ErrorCode.ERR_ValConstraintNotSatisfied, "System.Nullable<string>").WithArguments("System.Nullable<T>", "T", "string").WithLocation(100, 13));
    }

    [Fact]
    public void InaccessibleCaseTypeWithAttributes()
    {
        var declarations = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            class Outer { private class Hidden; }
            """;
        var attributed = CreateCompilation([declarations, "union U([A] Outer.Hidden);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var plain = CreateCompilation([declarations, "union U(Outer.Hidden);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var expected = GetComparableDiagnostics(plain);
        Assert.Contains(expected, d => d.Id == "CS0122");
        Assert.Equal(expected, GetComparableDiagnostics(attributed));

        var tree = attributed.SyntaxTrees.ElementAt(1);
        var syntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        Assert.Single(attributed.GetSemanticModel(tree).GetDeclaredSymbol(syntax).GetAttributes());
    }

    [Fact]
    public void ScopedWithoutRefRecoversAsType()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] scoped int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "scoped").WithArguments("scoped").WithLocation(100, 13),
            Diagnostic(ErrorCode.ERR_SyntaxError, "int").WithArguments(",").WithLocation(100, 20));
        var cases = compilation.SyntaxTrees.First().GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters;
        Assert.Equal(2, cases.Count);
        Assert.Single(cases[0].AttributeLists);
        Assert.Empty(cases[1].AttributeLists);
    }

    [Fact]
    public void AttributeArgumentsAndMultiplicity()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter, AllowMultiple = true)]
            public class AAttribute : Attribute
            {
                public AAttribute(Type type, int[] numbers, DayOfWeek day, string name, object value) { }
                public int Property { get; set; }
                public int Field;
            }
            public union U(
                [A(typeof(int), new int[] { 1, 2 }, DayOfWeek.Friday, nameof(value), null, Property = 7, Field = 8)]
                [param: A(typeof(string), new int[] { 3 }, DayOfWeek.Sunday, "other", 42, Property = 9, Field = 10)] int,
                bool);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()]));

        static void verify(CSharpCompilation compilation)
        {
            var type = compilation.GetTypeByMetadataName("U");
            var parameter = type.InstanceConstructors.Single(c => c.Parameters.Length == 1 &&
                c.Parameters[0].Type.SpecialType == SpecialType.System_Int32).Parameters[0];
            var attributes = parameter.GetAttributes().Where(a => a.AttributeClass.Name == "AAttribute").ToArray();
            Assert.Equal(2, attributes.Length);
            var first = attributes[0].ConstructorArguments.ToArray();
            Assert.Equal("Int32", ((ITypeSymbol)first[0].Value).Name);
            Assert.Equal(new object[] { 1, 2 }, first[1].Values.Select(v => v.Value));
            Assert.Equal(5, first[2].Value);
            Assert.Equal("value", first[3].Value);
            Assert.Null(first[4].Value);
            var firstNamed = attributes[0].NamedArguments.ToDictionary(a => a.Key, a => a.Value.Value);
            Assert.Equal(7, firstNamed["Property"]);
            Assert.Equal(8, firstNamed["Field"]);

            var second = attributes[1].ConstructorArguments.ToArray();
            Assert.Equal("String", ((ITypeSymbol)second[0].Value).Name);
            Assert.Equal(new object[] { 3 }, second[1].Values.Select(v => v.Value));
            Assert.Equal(0, second[2].Value);
            Assert.Equal("other", second[3].Value);
            Assert.Equal(42, second[4].Value);
            var secondNamed = attributes[1].NamedArguments.ToDictionary(a => a.Key, a => a.Value.Value);
            Assert.Equal(9, secondNamed["Property"]);
            Assert.Equal(10, secondNamed["Field"]);
        }
    }

    [Fact]
    public void AttributesRemainOnTheirDeclaredOwners()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]
            class TypeMarkerAttribute : Attribute;
            [AttributeUsage(AttributeTargets.Parameter)]
            class CaseMarkerAttribute : Attribute
            {
                public CaseMarkerAttribute(int value) { }
            }
            [TypeMarker]
            class CaseType;
            class Outer
            {
                [TypeMarker]
                public union U([CaseMarker(1)] CaseType)
                {
                    U([CaseMarker(2)] int value, bool unused) : this(new CaseType()) { }
                }
            }
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var caseType = compilation.GetTypeByMetadataName("CaseType");
        var union = compilation.GetTypeByMetadataName("Outer+U");
        Assert.Equal("TypeMarkerAttribute", Assert.Single(caseType.GetAttributes()).AttributeClass.Name);
        Assert.Equal("TypeMarkerAttribute", Assert.Single(union.GetAttributes()).AttributeClass.Name);
        var implicitConstructor = union.InstanceConstructors.Single(c => c.Parameters.Length == 1);
        var explicitConstructor = union.InstanceConstructors.Single(c => c.Parameters.Length == 2);
        Assert.Equal(1, Assert.Single(implicitConstructor.Parameters[0].GetAttributes()).ConstructorArguments.Single().Value);
        Assert.Equal(2, Assert.Single(explicitConstructor.Parameters[0].GetAttributes()).ConstructorArguments.Single().Value);
        Assert.DoesNotContain(implicitConstructor.GetAttributes(), a => a.AttributeClass.Name == "CaseMarkerAttribute");
        Assert.DoesNotContain(union.GetMembers().OfType<FieldSymbol>().SelectMany(f => f.GetAttributes()), a => a.AttributeClass.Name == "CaseMarkerAttribute");
        Assert.DoesNotContain(union.GetMembers("Value").SelectMany(m => m.GetAttributes()), a => a.AttributeClass.Name == "CaseMarkerAttribute");
    }

    [Fact]
    public void AttributeOverloadAndObsoleteConstructor()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                [Obsolete("old")]
                public AAttribute(int value) { }
                public AAttribute(string value) { }
            }
            #line 100
            union U([A(1)] int, [A("text")] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_DeprecatedSymbolStr, "A(1)").WithArguments("AAttribute.AAttribute(int)", "old").WithLocation(100, 10));

        var constructors = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1);
        var intAttribute = Assert.Single(constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Int32).Parameters[0].GetAttributes());
        var stringAttribute = Assert.Single(constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_String).Parameters[0].GetAttributes());
        Assert.Equal(SpecialType.System_Int32, intAttribute.AttributeConstructor.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, stringAttribute.AttributeConstructor.Parameters[0].Type.SpecialType);
    }

    [Fact]
    public void ObsoleteAttributeClass()
    {
        var source = """
            using System;
            [Obsolete("old type")]
            [AttributeUsage(AttributeTargets.Parameter)]
            class OldAttribute : Attribute;
            #line 100
            union U([Old] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_DeprecatedSymbolStr, "Old").WithArguments("OldAttribute", "old type").WithLocation(100, 10));
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Equal("OldAttribute", Assert.Single(parameter.GetAttributes()).AttributeClass.Name);
    }

    [Fact]
    public void DynamicallyAccessedMembersAttribute()
    {
        var source = """
            using System;
            using System.Diagnostics.CodeAnalysis;
            public union U(
                [DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicMethods)] Type,
                [DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicMethods)] string,
                [DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicMethods)] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()], targetFramework: TargetFramework.NetCoreApp));

        static void verify(CSharpCompilation compilation)
        {
            var parameters = compilation.GetTypeByMetadataName("U").InstanceConstructors
                .Where(c => c.Parameters.Length == 1).Select(c => c.Parameters[0]).ToArray();
            Assert.Equal(3, parameters.Length);
            Assert.All(parameters, parameter =>
            {
                var attribute = Assert.Single(parameter.GetAttributes(), a => a.AttributeClass.Name == "DynamicallyAccessedMembersAttribute");
                Assert.Equal((int)System.Diagnostics.CodeAnalysis.DynamicallyAccessedMemberTypes.PublicMethods, attribute.ConstructorArguments.Single().Value);
            });
        }
    }

    [Fact]
    public void CaseSyntaxMapsToConstructorParameter()
    {
        var source = """
            union U(int, string);
            """;

        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var constructors = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1).ToArray();

        foreach (var syntax in declaration.ParameterList.Parameters)
        {
            var parameter = model.GetDeclaredSymbol(syntax);
            Assert.NotNull(parameter);
            Assert.Equal("value", parameter.Name);
            Assert.Equal(0, parameter.Ordinal);
            Assert.Contains(constructors, constructor => SymbolEqualityComparer.Default.Equals(constructor.Parameters[0].GetPublicSymbol(), parameter));
        }
    }

    [Fact]
    public void LanguageVersion()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            #line 100
            union U([A] int, [param: A] string);
            """;

        var preview = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        preview.VerifyEmitDiagnostics();

        var csharp15 = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        csharp15.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_FeatureInPreview, "[A]").WithArguments("attributes on union case types").WithLocation(100, 9),
            Diagnostic(ErrorCode.ERR_FeatureInPreview, "[param: A]").WithArguments("attributes on union case types").WithLocation(100, 18));

        var plain = CreateCompilation(["union U(int);", UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        plain.VerifyEmitDiagnostics();
    }

    [Fact]
    public void AttributeUsageDiagnostics()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Method)]
            class MethodOnlyAttribute : Attribute;
            #line 100
            union U([MethodOnly] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);

        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "MethodOnly").WithArguments("MethodOnly", "method").WithLocation(100, 10));
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "MethodOnly").WithArguments("MethodOnly", "method").WithLocation(100, 10));
    }

    [Theory]
    [InlineData("Constructor", "constructor")]
    [InlineData("Method", "method")]
    [InlineData("Property", "property, indexer")]
    [InlineData("Field", "field")]
    [InlineData("ReturnValue", "return")]
    [InlineData("Struct", "struct")]
    public void InvalidAttributeUsage(string target, string targetDescription)
    {
        var source = $$"""
            using System;
            [AttributeUsage(AttributeTargets.{{target}})]
            class AAttribute : Attribute;
            #line 100
            union U([A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", targetDescription).WithLocation(100, 10));
    }

    [Fact]
    public void LanguageVersionWithIgnoredTarget()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            #line 100
            union U([field: A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_FeatureInPreview, "[field: A]").WithArguments("attributes on union case types").WithLocation(100, 9),
            Diagnostic(ErrorCode.WRN_AttributeLocationOnBadDeclaration, "field").WithArguments("field", "param").WithLocation(100, 10));
    }

    [Theory]
    [InlineData("[field: Missing]", "field", false)]
    [InlineData("[bogus: Missing]", "bogus", true)]
    public void LanguageVersionWithUnresolvedIgnoredAttribute(string attributeList, string target, bool unknown)
    {
        var source = $$"""
            #line 100
            union U({{attributeList}} int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_FeatureInPreview, attributeList).WithArguments("attributes on union case types").WithLocation(100, 9),
            Diagnostic(unknown ? ErrorCode.WRN_InvalidAttributeLocation : ErrorCode.WRN_AttributeLocationOnBadDeclaration, target)
                .WithArguments(target, "param").WithLocation(100, 10));
    }

    [Fact]
    public void LanguageVersionWithMalformedAttribute()
    {
        var source = """
            #line 100
            union U([] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_IdentifierExpected, "]").WithLocation(100, 10),
            Diagnostic(ErrorCode.ERR_FeatureInPreview, "[]").WithArguments("attributes on union case types").WithLocation(100, 9));
    }

    [Theory]
    [InlineData("field")]
    [InlineData("property")]
    [InlineData("method")]
    [InlineData("return")]
    [InlineData("type")]
    [InlineData("assembly")]
    [InlineData("module")]
    [InlineData("typevar")]
    [InlineData("event")]
    public void InvalidExplicitAttributeTargets(string target)
    {
        var source = $$"""
            #line 100
            union U([{{target}}: Missing] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_AttributeLocationOnBadDeclaration, target).WithArguments(target, "param").WithLocation(100, 10));
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Empty(parameter.GetAttributes());
    }

    [Fact]
    public void InvalidTargetsAndDuplicateAttributes()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            #line 100
            union U([field: Missing][A][param: A] int, [bogus: A][A] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);

        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_AttributeLocationOnBadDeclaration, "field").WithArguments("field", "param").WithLocation(100, 10),
            Diagnostic(ErrorCode.ERR_DuplicateAttribute, "A").WithArguments("A").WithLocation(100, 36),
            Diagnostic(ErrorCode.WRN_InvalidAttributeLocation, "bogus").WithArguments("bogus", "param").WithLocation(100, 45));
    }

    [Fact]
    public void ReservedTypeEncodingAttributes()
    {
        var source = """
            using System.Runtime.CompilerServices;
            union U(
            #line 100
                [Dynamic] object,
            #line 200
                [TupleElementNames(new[] { "x", "y" })] (int x, int y),
            #line 300
                [Nullable(1)] int);
            """;
        var compilation = CreateCompilation([source, NullableAttributeDefinition, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_ExplicitDynamicAttr, "Dynamic").WithLocation(100, 6),
            Diagnostic(ErrorCode.ERR_ExplicitTupleElementNamesAttribute, """TupleElementNames(new[] { "x", "y" })""").WithLocation(200, 6),
            Diagnostic(ErrorCode.ERR_ExplicitNullableAttribute, "Nullable(1)").WithLocation(300, 6));
    }

    [Theory]
    [InlineData("[Missing]")]
    [InlineData("[NotAnAttribute]")]
    [InlineData("[Abstract]")]
    [InlineData("[A(\"text\")]")]
    [InlineData("[A(Constants.NonConstant)]")]
    [InlineData("[A(1, Unknown = 3)]")]
    [InlineData("[A()]")]
    [InlineData("[A(1, ReadOnly = 2)]")]
    [InlineData("[A(1, Property = 2, Property = 3)]")]
    [InlineData("[Private]")]
    [InlineData("[Ambiguous]")]
    [InlineData("[BadParam(1m)]")]
    [InlineData("[A(1, Hidden = 2)]")]
    [InlineData("[A(1), A(2)]")]
    [InlineData("[return: A(1)]")]
    public void AttributeDiagnosticsMatchExplicitConstructor(string attributes)
    {
        var declarations = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                public AAttribute(int value) { }
                public int Property { get; set; }
                public int ReadOnly => 0;
                private int Hidden { get; set; }
            }
            class BadParamAttribute : Attribute { public BadParamAttribute(decimal value) { } }
            class NotAnAttribute { }
            abstract class AbstractAttribute : Attribute { }
            class PrivateAttribute : Attribute { private PrivateAttribute() { } }
            class Ambiguous : Attribute { }
            class AmbiguousAttribute : Attribute { }
            static class Constants { public static int NonConstant => 1; }
            """;
        var union = CreateCompilation([declarations, $"union U({attributes} int);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([declarations, $"class U {{ public U({attributes} int value) {{ }} }}", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);

        var expected = GetComparableDiagnostics(control);
        Assert.NotEmpty(expected);
        Assert.Equal(expected, GetComparableDiagnostics(union));
    }

    [Fact]
    public void NullableDefaultAttributeMatchesExplicitConstructor()
    {
        var prefix = "#nullable enable\nusing System.Runtime.InteropServices;\n";
        var union = CreateCompilation([prefix + "union U([Optional, DefaultParameterValue(null)] string);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([prefix + "class U { public U([Optional, DefaultParameterValue(null)] string value) { } }", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var expected = Assert.Single(GetComparableDiagnostics(control));
        var actual = Assert.Single(GetComparableDiagnostics(union));
        Assert.Equal("CS8625", expected.Id);
        Assert.Equal((expected.Id, expected.Severity, expected.Message), (actual.Id, actual.Severity, actual.Message));
        Assert.Equal("[Optional, DefaultParameterValue(null)] string value", expected.Text);
        Assert.Equal("[Optional, DefaultParameterValue(null)] string", actual.Text);
    }

    [Theory]
    [InlineData("[System.ParamArray]", "int[]", "CS0674")]
    [InlineData("[System.Runtime.CompilerServices.CallerLineNumber]", "int", "CS4020")]
    [InlineData("[System.Runtime.CompilerServices.CallerFilePath]", "string", "CS4021")]
    [InlineData("[System.Runtime.CompilerServices.CallerMemberName]", "string", "CS4022")]
    [InlineData("[System.Runtime.CompilerServices.CallerArgumentExpression(\"missing\")]", "string", "CS8964")]
    [InlineData("[System.Runtime.CompilerServices.EnumeratorCancellation]", "System.Threading.CancellationToken", "CS8424")]
    [InlineData("[System.Diagnostics.CodeAnalysis.UnscopedRef]", "int", "CS9063")]
    [InlineData("[System.Runtime.InteropServices.MarshalAs(System.Runtime.InteropServices.UnmanagedType.ByValArray)]", "int[]", "CS7055")]
    [InlineData("[System.Runtime.InteropServices.DefaultParameterValue(\"bad\")]", "int", "CS1908")]
    [InlineData("[System.Runtime.InteropServices.DefaultParameterValue(null)]", "int", "CS1908")]
    [InlineData("[System.Runtime.CompilerServices.CallerLineNumber]", "string", "CS4017")]
    public void WellKnownAttributeDiagnosticsMatchExplicitConstructor(string attributes, string type, string expectedId)
    {
        var union = CreateCompilation([$"union U({attributes} {type});", UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([$"class U {{ public U({attributes} {type} value) {{ }} }}", UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);

        var expected = GetComparableDiagnostics(control);
        Assert.Contains(expected, d => d.Id == expectedId);
        Assert.Equal(expected, GetComparableDiagnostics(union));
    }

    [Theory]
    [InlineData("[A(typeof(T))]", "CS0416")]
    [InlineData("[Generic<T>]", "CS8968")]
    public void GenericAttributeDiagnosticsMatchExplicitConstructor(string attributes, string expectedId)
    {
        var declarations = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute { public AAttribute(Type value) { } }
            [AttributeUsage(AttributeTargets.Parameter)]
            class GenericAttribute<T> : Attribute;
            """;
        var union = CreateCompilation([declarations, $"union U<T>({attributes} T);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([declarations, $"class U<T> {{ public U({attributes} T value) {{ }} }}", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);

        var expected = GetComparableDiagnostics(control);
        Assert.Contains(expected, d => d.Id == expectedId);
        Assert.Equal(expected, GetComparableDiagnostics(union));
    }

    [Fact]
    public void NullableAttributeArgumentsMatchExplicitConstructor()
    {
        var declarations = """
            #nullable enable
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute
            {
                public AAttribute(string value) { }
                public string Property { get; set; } = "";
            }
            """;
        var union = CreateCompilation([declarations, "#nullable enable\nunion U([A(null, Property = null)] string);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([declarations, "#nullable enable\nclass U { public U([A(null, Property = null)] string value) { } }", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var expected = GetComparableDiagnostics(control);
        Assert.Equal(2, expected.Length);
        Assert.All(expected, diagnostic => Assert.Equal("CS8625", diagnostic.Id));
        Assert.Equal(expected, GetComparableDiagnostics(union));
    }

    private static (string Id, DiagnosticSeverity Severity, string Message, string Text)[] GetComparableDiagnostics(CSharpCompilation compilation)
        => compilation.GetDiagnostics().Where(d => d.Severity != DiagnosticSeverity.Hidden)
            .Select(d => (d.Id, d.Severity, d.GetMessage(), d.Location.SourceTree.GetText().ToString(d.Location.SourceSpan))).ToArray();

    [Fact]
    public void InvalidCaseParameterSyntax()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            union U(
            #line 100
                [A] out int,
            #line 200
                [A] string name,
            #line 300
                [A] long = 0);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "out").WithArguments("out").WithLocation(100, 9),
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "name").WithArguments("name").WithLocation(200, 16),
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "=").WithArguments("=").WithLocation(300, 14));

        var constructors = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1);
        foreach (var constructor in constructors)
        {
            var parameter = constructor.Parameters[0];
            Assert.Equal("value", parameter.Name);
            Assert.Equal(RefKind.None, parameter.RefKind);
            Assert.False(parameter.IsOptional);
            Assert.False(parameter.HasExplicitDefaultValue);
            Assert.Single(parameter.GetAttributes());
        }
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void ArglistAtCaseListBoundary(bool first)
    {
        var caseList = first ? "[A] __arglist, [A] int" : "[A] int, [A] __arglist";
        var source = $$"""
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U({{caseList}});
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_IllegalVarArgs, "__arglist").WithLocation(100, 9 + caseList.IndexOf("__arglist", System.StringComparison.Ordinal)));

        var tree = compilation.SyntaxTrees.First();
        var cases = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters;
        var model = compilation.GetSemanticModel(tree);
        var arglist = cases.Single(p => p.IsArgList);
        var valid = cases.Single(p => !p.IsArgList);
        Assert.Null(model.GetDeclaredSymbol(arglist));
        var parameter = model.GetDeclaredSymbol(valid);
        Assert.Equal(0, parameter.Ordinal);
        Assert.Single(parameter.GetAttributes());
    }

    [Fact]
    public void ErrorCaseTypeRetainsItsOwnAttributes()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] MissingType, [A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "MissingType").WithArguments("MissingType").WithLocation(100, 13));

        var tree = compilation.SyntaxTrees.First();
        var cases = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters;
        var model = compilation.GetSemanticModel(tree);
        var errorParameter = model.GetDeclaredSymbol(cases[0]);
        var validParameter = model.GetDeclaredSymbol(cases[1]);
        Assert.Equal(TypeKind.Error, errorParameter.Type.TypeKind);
        Assert.Equal(SpecialType.System_Int32, validParameter.Type.SpecialType);
        Assert.False(SymbolEqualityComparer.Default.Equals(errorParameter, validParameter));
        Assert.Single(errorParameter.GetAttributes());
        Assert.Single(validParameter.GetAttributes());
    }

    [Fact]
    public void OptionalAndDefaultValueAttributes()
    {
        var source = """
            using System;
            using System.Runtime.InteropServices;
            public union U([Optional, DefaultParameterValue(7)] int, [Optional] string, [DefaultParameterValue(true)] bool);
            class Program
            {
                static void Main()
                {
                    Console.Write(new U().Value ?? "null");
                    Console.Write(" ");
                    Console.Write(new U(value: 4).Value);
                }
            }
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            options: TestOptions.ReleaseExe, parseOptions: TestOptions.RegularPreview);
        CompileAndVerify(compilation, expectedOutput: "null 4").VerifyDiagnostics();

        verify(compilation.GetTypeByMetadataName("U"));
        foreach (var reference in new MetadataReference[] { compilation.ToMetadataReference(), compilation.EmitToImageReference() })
        {
            var consumer = CreateCompilation("", references: [reference], parseOptions: TestOptions.Regular15);
            verify(consumer.GetTypeByMetadataName("U"));
        }

        static void verify(NamedTypeSymbol type)
        {
            var constructors = type.InstanceConstructors.Where(c => c.Parameters.Length == 1).ToArray();
            var intParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Int32).Parameters[0];
            var stringParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_String).Parameters[0];
            var boolParameter = constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Boolean).Parameters[0];
            Assert.True(intParameter.IsOptional);
            Assert.True(intParameter.HasExplicitDefaultValue);
            Assert.Equal(7, intParameter.ExplicitDefaultValue);
            Assert.True(stringParameter.IsOptional);
            Assert.False(stringParameter.HasExplicitDefaultValue);
            Assert.False(boolParameter.IsOptional);
            Assert.False(boolParameter.HasExplicitDefaultValue);
            if (boolParameter is PEParameterSymbol imported)
            {
                Assert.Equal(true, imported.ImportConstantValue().Value);
            }
        }
    }

    [Fact]
    public void DecimalAndDateTimeDefaults()
    {
        var source = """
            using System;
            using System.Runtime.CompilerServices;
            using System.Runtime.InteropServices;
            public union U(
                [Optional, DecimalConstant(0, 0, 0, 0, 7)] decimal,
                [Optional, DateTimeConstant(42)] DateTime);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()]));

        static void verify(CSharpCompilation compilation)
        {
            var parameters = compilation.GetTypeByMetadataName("U").InstanceConstructors
                .Where(c => c.Parameters.Length == 1).Select(c => c.Parameters[0]).ToArray();
            var decimalParameter = parameters.Single(p => p.Type.SpecialType == SpecialType.System_Decimal);
            var dateTimeParameter = parameters.Single(p => p.Type.SpecialType == SpecialType.System_DateTime);
            Assert.True(decimalParameter.IsOptional);
            Assert.True(decimalParameter.HasExplicitDefaultValue);
            Assert.Equal(7m, decimalParameter.ExplicitDefaultValue);
            verifyConstantAttribute(decimalParameter, AttributeDescription.DecimalConstantAttribute, "DecimalConstantAttribute");
            Assert.True(dateTimeParameter.IsOptional);
            Assert.True(dateTimeParameter.HasExplicitDefaultValue);
            Assert.Equal(new System.DateTime(42), dateTimeParameter.ExplicitDefaultValue);
            verifyConstantAttribute(dateTimeParameter, AttributeDescription.DateTimeConstantAttribute, "DateTimeConstantAttribute");
        }

        static void verifyConstantAttribute(ParameterSymbol parameter, AttributeDescription description, string name)
        {
            if (parameter is PEParameterSymbol imported)
            {
                var module = (PEModuleSymbol)imported.ContainingModule;
                var attributes = module.Module.MetadataReader.GetParameter(imported.Handle).GetCustomAttributes();
                Assert.Single(attributes, handle => module.AttributeMatchesFilter(handle, description));
            }
            else
            {
                Assert.Single(parameter.GetAttributes(), a => a.AttributeClass.Name == name);
            }
        }
    }

    [Theory]
    [InlineData("DefaultParameterValue(1), DecimalConstant(0, 0, 0, 0, 2)")]
    [InlineData("DecimalConstant(0, 0, 0, 0, 2), DefaultParameterValue(1)")]
    public void ConflictingDefaultAttributesMatchExplicitConstructor(string attributes)
    {
        var prefix = "using System.Runtime.InteropServices;\nusing System.Runtime.CompilerServices;\n";
        var union = CreateCompilation([prefix + $"union U([Optional, {attributes}] decimal);", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([prefix + $"class U {{ public U([Optional, {attributes}] decimal value) {{ }} }}", UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);

        Assert.Equal(GetComparableDiagnostics(control), GetComparableDiagnostics(union));
        var expected = control.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        var actual = union.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Equal(expected.IsOptional, actual.IsOptional);
        Assert.Equal(expected.HasExplicitDefaultValue, actual.HasExplicitDefaultValue);
        if (expected.HasExplicitDefaultValue)
        {
            Assert.Equal(expected.ExplicitDefaultValue, actual.ExplicitDefaultValue);
        }
    }

    [Fact]
    public void AttributeDefaultIsIndependentOfRejectedSyntaxDefault()
    {
        var source = """
            using System.Runtime.InteropServices;
            #line 100
            union U([Optional, DefaultParameterValue(7)] int = 0);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "=").WithArguments("=").WithLocation(100, 50));

        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.True(parameter.IsOptional);
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(7, parameter.ExplicitDefaultValue);
    }

    [Fact]
    public void NullableParameterAttributesInSourceAndMetadata()
    {
        var unions = """
            #nullable enable
            using System.Diagnostics.CodeAnalysis;
            public union Allows([AllowNull] string);
            public union Disallows([DisallowNull] string?);
            """;
        var use = """
            #nullable enable
            class C
            {
                static void M(string? value)
                {
                    _ = new Allows(value);
                    Allows a = value;
            #line 100
                    _ = new Disallows(value);
                }
                static void N(string? value)
                {
            #line 200
                    Disallows d = value;
                }
            }
            """;
        var producer = CreateCompilation([unions, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        producer.VerifyEmitDiagnostics();

        verify(CreateCompilation([unions, use, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview));
        foreach (var reference in new MetadataReference[] { producer.ToMetadataReference(), producer.EmitToImageReference() })
        {
            verify(CreateCompilation(use, references: [reference], targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.Regular15));
        }

        static void verify(CSharpCompilation compilation)
        {
            compilation.VerifyDiagnostics(
                Diagnostic(ErrorCode.WRN_NullReferenceArgument, "value").WithArguments("value", "Disallows.Disallows(string? value)").WithLocation(100, 27),
                Diagnostic(ErrorCode.WRN_NullReferenceArgument, "value").WithArguments("value", "Disallows.Disallows(string? value)").WithLocation(200, 23));
        }
    }

    [Fact]
    public void UserAttributesAndSynthesizedTypeAttributes()
    {
        var source = """
            #nullable enable
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            public class AAttribute : Attribute;
            public struct Control { public Control(nint value) { } }
            public union U([A] dynamic, [A] (int x, string y), [A] nint, [A] string?);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);

        var consumer = CreateCompilation("", references: [compilation.EmitToImageReference()]);
        verify(consumer);

        static void verify(CSharpCompilation compilation)
        {
            var type = compilation.GetTypeByMetadataName("U");
            var parameters = type.InstanceConstructors.Where(c => c.Parameters.Length == 1).Select(c => c.Parameters[0]).ToArray();
            Assert.Equal(4, parameters.Length);
            Assert.All(parameters, p => Assert.Single(p.GetAttributes(), a => a.AttributeClass.Name == "AAttribute"));
            Assert.Contains(parameters, p => p.Type.TypeKind == TypeKind.Dynamic);
            var tuple = (NamedTypeSymbol)Assert.Single(parameters, p => p.Type.IsTupleType).Type;
            Assert.Equal("x", tuple.TupleElements[0].Name);
            Assert.Equal("y", tuple.TupleElements[1].Name);
            var controlType = compilation.GetTypeByMetadataName("Control").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0].Type;
            Assert.Contains(parameters, p => p.Type.SpecialType == SpecialType.System_IntPtr &&
                p.Type.IsNativeIntegerType == controlType.IsNativeIntegerType);
            Assert.Contains(parameters, p => p.Type.SpecialType == SpecialType.System_String &&
                p.TypeWithAnnotations.NullableAnnotation == NullableAnnotation.Annotated);
        }
    }

    [Fact]
    public void GenericUnionAttributesSurviveSubstitution()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            public class AAttribute : Attribute
            {
                public AAttribute(string value) { }
            }
            public union U<T>([A(nameof(T))] T, [A("other")] int) where T : class;
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        foreach (var reference in new MetadataReference[] { compilation.ToMetadataReference(), compilation.EmitToImageReference() })
        {
            verify(CreateCompilation("", references: [reference], parseOptions: TestOptions.Regular15));
        }

        static void verify(CSharpCompilation compilation)
        {
            var generic = compilation.GetTypeByMetadataName("U`1");
            var constructed = generic.Construct(compilation.GetSpecialType(SpecialType.System_String));
            var parameter = constructed.InstanceConstructors.Single(c => c.Parameters.Length == 1 &&
                c.Parameters[0].Type.SpecialType == SpecialType.System_String).Parameters[0];
            Assert.Equal("T", Assert.Single(parameter.GetAttributes(), a => a.AttributeClass.Name == "AAttribute").ConstructorArguments.Single().Value);
            Assert.Equal("value", parameter.Name);
        }
    }

    [Fact]
    public void AliasedAndGenericAttributes()
    {
        var library = """
            using System;
            namespace Attributes
            {
                [AttributeUsage(AttributeTargets.Parameter)]
                public class AAttribute : Attribute;
                [AttributeUsage(AttributeTargets.Parameter)]
                public class GenericAttribute<T> : Attribute;
            }
            """;
        var reference = CreateCompilation(library).EmitToImageReference().WithAliases(["attributeLibrary"]);
        var source = """
            extern alias attributeLibrary;
            using Alias = attributeLibrary::Attributes.AAttribute;
            union U([Alias] int, [attributeLibrary::Attributes.Generic<int>] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], references: [reference], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var firstCase = declaration.ParameterList.Parameters[0];
        var model = compilation.GetSemanticModel(tree);
        var alias = model.GetAliasInfo((IdentifierNameSyntax)firstCase.AttributeLists.Single().Attributes.Single().Name);
        Assert.Equal("Alias", alias.Name);
        Assert.Equal("AAttribute", alias.Target.Name);
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(firstCase.Type).Type.SpecialType);
        Assert.Null(model.GetDeclaredSymbol(firstCase.Type));
        Assert.Null(model.GetDeclaredSymbol(declaration.ParameterList));

        var parameters = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1)
            .Select(c => c.Parameters[0]).ToArray();
        Assert.Equal("AAttribute", Assert.Single(parameters.Single(p => p.Type.SpecialType == SpecialType.System_Int32).GetAttributes()).AttributeClass.Name);
        var generic = Assert.Single(parameters.Single(p => p.Type.SpecialType == SpecialType.System_String).GetAttributes()).AttributeClass;
        Assert.Equal("GenericAttribute", generic.Name);
        Assert.Equal(SpecialType.System_Int32, generic.TypeArguments().Single().SpecialType);
    }

    [Fact]
    public void InteropParameterAttributes()
    {
        var source = """
            using System.Runtime.InteropServices;
            public struct Control
            {
                public Control([In, Out, MarshalAs(UnmanagedType.I4)] int value) { }
            }
            public union U([In, Out, MarshalAs(UnmanagedType.I4)] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()]));

        static void verify(CSharpCompilation compilation)
        {
            var control = compilation.GetTypeByMetadataName("Control").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            var actual = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.True(control.IsMetadataIn);
            Assert.True(control.IsMetadataOut);
            Assert.Equal(control.IsMetadataIn, actual.IsMetadataIn);
            Assert.Equal(control.IsMetadataOut, actual.IsMetadataOut);
            Assert.Equal(System.Runtime.InteropServices.UnmanagedType.I4, control.MarshallingType);
            Assert.Equal(control.MarshallingType, actual.MarshallingType);
            if (actual is PEParameterSymbol)
            {
                Assert.False(actual.MarshallingDescriptor.IsDefaultOrEmpty);
                Assert.Equal(control.MarshallingDescriptor.ToArray(), actual.MarshallingDescriptor.ToArray());
            }
        }
    }

    [Fact]
    public void NullableFlowAnnotationsAreDecoded()
    {
        var source = """
            #nullable enable
            using System.Diagnostics.CodeAnalysis;
            public union U([NotNull] string?);
            public union V([MaybeNull] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics(
            Diagnostic(ErrorCode.WRN_ParameterDisallowsNull, ";").WithArguments("value").WithLocation(3, 34));
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()], targetFramework: TargetFramework.NetCoreApp));

        static void verify(CSharpCompilation compilation)
        {
            var u = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            var v = compilation.GetTypeByMetadataName("V").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.True((u.FlowAnalysisAnnotations & FlowAnalysisAnnotations.NotNull) != 0);
            Assert.True((v.FlowAnalysisAnnotations & FlowAnalysisAnnotations.MaybeNull) != 0);
        }
    }

    [Theory]
    [InlineData("NotNullWhen(true)", "string?", (int)FlowAnalysisAnnotations.NotNullWhenTrue)]
    [InlineData("MaybeNullWhen(false)", "string", (int)FlowAnalysisAnnotations.MaybeNullWhenFalse)]
    [InlineData("DoesNotReturnIf(true)", "bool", (int)FlowAnalysisAnnotations.DoesNotReturnIfTrue)]
    public void ConditionalFlowAnnotationsMatchConstructor(string attribute, string type, int expected)
    {
        var source = $$"""
            #nullable enable
            using System.Diagnostics.CodeAnalysis;
            public class Control { public Control([{{attribute}}] {{type}} value) { } }
            public union U([{{attribute}}] {{type}});
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()], targetFramework: TargetFramework.NetCoreApp));

        void verify(CSharpCompilation compilation)
        {
            var control = compilation.GetTypeByMetadataName("Control").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            var actual = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.Equal((FlowAnalysisAnnotations)expected, actual.FlowAnalysisAnnotations);
            Assert.Equal(control.FlowAnalysisAnnotations, actual.FlowAnalysisAnnotations);
        }
    }

    [Fact]
    public void NotNullIfNotNullParameterAttribute()
    {
        var source = """
            #nullable enable
            using System.Diagnostics.CodeAnalysis;
            public union U([NotNullIfNotNull("value")] string?);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()], targetFramework: TargetFramework.NetCoreApp));

        static void verify(CSharpCompilation compilation)
        {
            var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.Contains("value", parameter.NotNullIfParameterNotNull);
        }
    }

    [Fact]
    public void NotNullParameterUpdatesCallerFlow()
    {
        var producerSource = """
            #nullable enable
            #pragma warning disable CS8777
            using System.Diagnostics.CodeAnalysis;
            public union U([NotNull] string?);
            """;
        var consumerSource = """
            #nullable enable
            class Consumer
            {
                static int M(string? value)
                {
                    _ = new U(value);
                    return value.Length;
                }
            }
            """;
        var producer = CreateCompilation([producerSource, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        producer.VerifyEmitDiagnostics();
        CreateCompilation([producerSource, consumerSource, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview).VerifyEmitDiagnostics();
        foreach (var reference in new MetadataReference[] { producer.ToMetadataReference(), producer.EmitToImageReference() })
        {
            CreateCompilation(consumerSource, references: [reference], targetFramework: TargetFramework.NetCoreApp,
                parseOptions: TestOptions.Regular15).VerifyEmitDiagnostics();
        }
    }

    [Theory]
    [InlineData("")]
    [InlineData("missing")]
    [InlineData("value")]
    public void InterpolatedStringHandlerDiagnosticsMatchExplicitConstructor(string argument)
    {
        var declarations = """
            using System.Runtime.CompilerServices;
            [InterpolatedStringHandler]
            struct Handler
            {
                public Handler(int literalLength, int formattedCount) { }
            }
            """;
        var attribute = $"[System.Runtime.CompilerServices.InterpolatedStringHandlerArgument(\"{argument}\")]";
        var union = CreateCompilation([declarations, $"union U({attribute} Handler);", UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        var control = CreateCompilation([declarations, $"class U {{ public U({attribute} Handler value) {{ }} }}", UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);

        var expected = GetComparableDiagnostics(control);
        Assert.NotEmpty(expected);
        Assert.Equal(expected, GetComparableDiagnostics(union));
    }

    [Fact]
    public void FeatureGateOnFactoryCreatedUnion()
    {
        var declaration = SyntaxFactory.ParseCompilationUnit("union U([A] int);", options: TestOptions.RegularPreview)
            .Members.OfType<UnionDeclarationSyntax>().Single();
        var attribute = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            """;

        foreach (var parseOptions in new[] { TestOptions.Regular14, TestOptions.Regular15, TestOptions.RegularPreview })
        {
            var root = SyntaxFactory.CompilationUnit().AddMembers(declaration);
            root._syntaxTree = CSharpSyntaxTree.CreateWithoutClone(root, parseOptions);
            var compilation = CreateCompilation([attribute, UnionAttributeSource, IUnionSource], parseOptions: parseOptions).AddSyntaxTrees(root.SyntaxTree);
            if (parseOptions == TestOptions.Regular14)
            {
                compilation.VerifyDiagnostics(
                    Diagnostic(ErrorCode.ERR_FeatureNotAvailableInVersion14, "union").WithArguments("unions", "15.0").WithLocation(1, 1),
                    Diagnostic(ErrorCode.ERR_FeatureInPreview, "[A]").WithArguments("attributes on union case types").WithLocation(1, 9));
            }
            else if (parseOptions == TestOptions.Regular15)
            {
                compilation.VerifyDiagnostics(
                    Diagnostic(ErrorCode.ERR_FeatureInPreview, "[A]").WithArguments("attributes on union case types").WithLocation(1, 9));
            }
            else
            {
                compilation.VerifyEmitDiagnostics();
            }
        }
    }

    [Fact]
    public void FeatureGateOnRejectedPartialCaseList()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            partial union U(int);
            #line 200
            partial union U([A] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.Regular15);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_MultipleRecordParameterLists, "([A] string)").WithLocation(200, 16),
            Diagnostic(ErrorCode.ERR_FeatureInPreview, "[A]").WithArguments("attributes on union case types").WithLocation(200, 17));
    }

    [Fact]
    public void NameofConstructorParameter()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                public AAttribute(string name) { }
            }
            union U([A(nameof(value))] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var model = compilation.GetSemanticModel(tree);
        var parameter = model.GetDeclaredSymbol(parameterSyntax);
        var name = parameterSyntax.AttributeLists.Single().DescendantNodes().OfType<IdentifierNameSyntax>().Single(n => n.Identifier.ValueText == "value");
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter, model.GetSymbolInfo(name).Symbol));
        Assert.True(parameter.IsImplicitlyDeclared);
        Assert.Empty(parameter.DeclaringSyntaxReferences);
        Assert.Equal(0, parameter.Ordinal);
        Assert.True(parameter.ContainingSymbol.IsImplicitlyDeclared);
        Assert.Equal("value", Assert.Single(parameter.GetAttributes()).ConstructorArguments.Single().Value);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SpeculativeFirstCaseAttributeUsesConstructorScope(bool hasAttribute)
    {
        var caseText = hasAttribute ? """[A("existing")] int""" : "int";
        var source = $$"""
            union U({{caseText}})
            {
                [System.AttributeUsage(System.AttributeTargets.Parameter)]
                public class AAttribute : System.Attribute
                {
                    public AAttribute(string value, [System.Runtime.CompilerServices.CallerMemberName] string caller = "") { }
                }
            }
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var syntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var model = compilation.GetSemanticModel(tree);
        var attribute = SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("A"))
            .WithArgumentList(SyntaxFactory.ParseAttributeArgumentList("(nameof(value))"));
        foreach (var position in new[] { syntax.SpanStart, syntax.Type.SpanStart }.Distinct())
        {
            var symbol = model.GetSpeculativeSymbolInfo(position, attribute).Symbol;
            Assert.NotNull(symbol);
            Assert.Equal("AAttribute", symbol.ContainingType.Name);
            Assert.True(model.TryGetSpeculativeSemanticModel(position, attribute, out var speculativeModel));
            var name = attribute.ArgumentList.DescendantNodes().OfType<IdentifierNameSyntax>().Single(n => n.Identifier.ValueText == "value");
            Assert.True(SymbolEqualityComparer.Default.Equals(model.GetDeclaredSymbol(syntax), speculativeModel.GetSymbolInfo(name).Symbol));
            var operation = Assert.IsAssignableFrom<IAttributeOperation>(speculativeModel.GetOperation(attribute));
            var creation = Assert.IsAssignableFrom<IObjectCreationOperation>(operation.Operation);
            Assert.Equal(".ctor", creation.Arguments.Single(a => a.Parameter.Name == "caller").Value.ConstantValue.Value);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RuntimeConversionsAndPatterns(bool debug)
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            union U([A] int, [A] string);
            union G<T>([A] T, [A] int) where T : class;
            class Program
            {
                static void Main()
                {
                    U a = 42;
                    U b = "text";
                    Console.Write(a switch { int number => number, _ => 0 });
                    Console.Write("|");
                    Console.Write(b is string value ? value : "");
                    Console.Write("|");
                    Console.Write(default(U).Value is null);
                    G<string> generic = "generic";
                    Console.Write("|");
                    Console.Write(generic.Value);
                }
            }
            """;
        var options = debug ? TestOptions.DebugExe : TestOptions.ReleaseExe;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], options: options, parseOptions: TestOptions.RegularPreview);
        CompileAndVerify(compilation, expectedOutput: "42|text|True|generic").VerifyDiagnostics();
    }

    [Fact]
    public void SequencePointRemainsOnCaseType()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            partial union U;
            partial union U(
                [A]
                int, [A] bool);
            """;
        var compilation = CreateCompilation(source + "\n" + UnionAttributeSource + IUnionSource,
            options: TestOptions.DebugDll, parseOptions: TestOptions.RegularPreview);
        var verifier = CompileAndVerify(compilation).VerifyDiagnostics();
        verifier.VerifyMethodBody("U..ctor(int)", """
            {
              // Code size       14 (0xe)
              .maxstack  2
              // sequence point: <hidden>
              IL_0000:  ldarg.0
              IL_0001:  ldarg.1
              IL_0002:  box        "int"
              IL_0007:  stfld      "object U.<Value>k__BackingField"
              // sequence point: int
              IL_000c:  nop
              IL_000d:  ret
            }
            """);
    }

    [Fact]
    public void MetadataOnlyEmit()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            public class AAttribute : System.Attribute
            {
                public AAttribute(int value) { }
            }
            public union U([A(1)] int, [A(2)] string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream, options: new EmitOptions(metadataOnly: true));
        result.Diagnostics.Verify();
        Assert.True(result.Success);

        var consumer = CreateCompilation("", references: [MetadataReference.CreateFromImage(stream.ToArray())], parseOptions: TestOptions.Regular15);
        var constructors = consumer.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1).ToArray();
        Assert.Equal(1, Assert.Single(constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_Int32).Parameters[0]
            .GetAttributes(), a => a.AttributeClass.Name == "AAttribute").ConstructorArguments.Single().Value);
        Assert.Equal(2, Assert.Single(constructors.Single(c => c.Parameters[0].Type.SpecialType == SpecialType.System_String).Parameters[0]
            .GetAttributes(), a => a.AttributeClass.Name == "AAttribute").ConstructorArguments.Single().Value);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void CompletionOrder(bool concurrent, bool attributesFirst)
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Method)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            options: TestOptions.ReleaseDll.WithConcurrentBuild(concurrent), parseOptions: TestOptions.RegularPreview);
        var tree = compilation.SyntaxTrees.First();
        var syntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var model = compilation.GetSemanticModel(tree);

        if (attributesFirst)
        {
            _ = model.GetDeclaredSymbol(syntax).GetAttributes();
        }

        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 10));
        _ = model.GetDeclaredSymbol(syntax).GetAttributes();
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 10));
    }

    [Fact]
    public void EmitFirstReportsCaseAttributeDiagnostics()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Method)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        result.Diagnostics.Verify(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 10));
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 10));
    }

    [Fact]
    public void FilteredCompletionDoesNotSuppressAttributeDiagnostics()
    {
        var source = """
            #line 100
            union U([Missing] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.GetDiagnostics(CompilationStage.Declare, includeEarlierStages: true,
            symbolFilter: symbol => symbol is not SynthesizedUnionCtor, cancellationToken: default).Verify();

        var type = (SourceNamedTypeSymbol)compilation.SourceModule.GlobalNamespace.GetMembersUnordered().Single(m => m.Name == "U");
        Assert.False(type.HasComplete(CompletionPart.MembersCompleted));

        compilation.GetDeclarationDiagnostics().Verify(
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("MissingAttribute").WithLocation(100, 10),
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("Missing").WithLocation(100, 10));
    }

    [Fact]
    public void PartialUnionSpanDiagnostics()
    {
        var firstPart = """
            partial union U;
            """;
        var secondPart = """
            [System.AttributeUsage(System.AttributeTargets.Method)]
            class AAttribute : System.Attribute;
            #line 100
            partial union U([A] int);
            """;
        var compilation = CreateCompilation([firstPart, secondPart, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        var tree = compilation.SyntaxTrees.ElementAt(1);
        var syntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var model = compilation.GetSemanticModel(tree);
        model.GetDeclarationDiagnostics(syntax.Span).Verify(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 18));
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_AttributeOnBadSymbolType, "A").WithArguments("A", "method").WithLocation(100, 18));
    }

    [Fact]
    public void MovingAttributeBetweenCasesUpdatesTheCompilation()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            union U([A] int, string);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        var oldTree = compilation.SyntaxTrees.First();
        var text = oldTree.GetText().WithChanges(
            new TextChange(new TextSpan(source.IndexOf("[A] ", System.StringComparison.Ordinal), 4), ""),
            new TextChange(new TextSpan(source.IndexOf("string", System.StringComparison.Ordinal), 0), "[A] "));
        var newTree = oldTree.WithChangedText(text);
        var changed = compilation.ReplaceSyntaxTree(oldTree, newTree);

        verify(compilation, SpecialType.System_Int32);
        verify(changed, SpecialType.System_String);
        verify(compilation, SpecialType.System_Int32);

        static void verify(CSharpCompilation compilation, SpecialType attributedType)
        {
            compilation.VerifyEmitDiagnostics();
            var constructors = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1);
            foreach (var constructor in constructors)
            {
                var parameter = constructor.Parameters[0];
                Assert.Equal(parameter.Type.SpecialType == attributedType, parameter.GetAttributes().Any(a => a.AttributeClass.Name == "AAttribute"));
            }
        }
    }

    [Fact]
    public void NameofParameterAndMemberShadowing()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter, AllowMultiple = true)]
            class AAttribute : System.Attribute
            {
                public AAttribute(string value) { }
            }
            union U([A(nameof(value))][A(value)] int)
            {
                public const string value = "field";
            }
            """;
        var control = """
            [System.AttributeUsage(System.AttributeTargets.Parameter, AllowMultiple = true)]
            class AAttribute : System.Attribute
            {
                public AAttribute(string value) { }
            }
            class U
            {
                public const string value = "field";
                public U([A(nameof(value))][A(value)] int value) { }
            }
            """;
        CreateCompilation(control, parseOptions: TestOptions.RegularPreview).VerifyEmitDiagnostics();
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var model = compilation.GetSemanticModel(tree);
        var parameter = model.GetDeclaredSymbol(parameterSyntax);
        var names = parameterSyntax.AttributeLists.SelectMany(l => l.DescendantNodes().OfType<IdentifierNameSyntax>())
            .Where(n => n.Identifier.ValueText == "value").ToArray();
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter, model.GetSymbolInfo(names[0]).Symbol));
        Assert.Equal(SymbolKind.Field, model.GetSymbolInfo(names[1]).Symbol.Kind);
        var attributes = parameter.GetAttributes();
        Assert.Equal("value", attributes[0].ConstructorArguments.Single().Value);
        Assert.Equal("field", attributes[1].ConstructorArguments.Single().Value);

        var originalAttribute = parameterSyntax.AttributeLists[1].Attributes.Single();
        var replacementRoot = tree.GetRoot().ReplaceNode(originalAttribute,
            originalAttribute.WithArgumentList(SyntaxFactory.ParseAttributeArgumentList("(value)")));
        var replacement = replacementRoot.DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single()
            .AttributeLists[1].Attributes.Single();
        Assert.True(model.TryGetSpeculativeSemanticModel(originalAttribute.ArgumentList.Position, replacement, out var speculativeModel));
        var speculativeName = replacement.DescendantNodes().OfType<IdentifierNameSyntax>().Single(n => n.Identifier.ValueText == "value");
        Assert.Equal(SymbolKind.Field, speculativeModel.GetSymbolInfo(speculativeName).Symbol.Kind);
    }

    [Fact]
    public void ParameterNameIsNotInScopeOutsideNameof()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute
            {
                public AAttribute(string value) { }
            }
            #line 100
            union U([A(value)] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_NameNotInContext, "value").WithArguments("value").WithLocation(100, 12));
    }

    [Fact]
    public void AttributeBindingCanReferenceContainingUnion()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                public AAttribute(Type type, string name) { }
            }
            union U([A(typeof(U), nameof(U.Value))] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        var attribute = Assert.Single(parameter.GetAttributes());
        Assert.Equal("U", ((ITypeSymbol)attribute.ConstructorArguments.First().Value).Name);
        Assert.Equal("Value", attribute.ConstructorArguments.Last().Value);
    }

    [Fact]
    public void AttributeSemanticModelAndSpeculation()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                public AAttribute(Type type, string name) { }
                public int Property { get; set; }
            }
            union U([A(typeof(int), nameof(value), Property = 3)] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.First();
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var attribute = declaration.ParameterList.Parameters.Single().AttributeLists.Single().Attributes.Single();
        var model = compilation.GetSemanticModel(tree);
        var parameter = model.GetDeclaredSymbol(declaration.ParameterList.Parameters.Single());
        var data = Assert.Single(parameter.GetAttributes());
        Assert.Equal("AAttribute", data.AttributeClass.Name);
        Assert.Equal("value", data.ConstructorArguments[1].Value);
        Assert.Equal(3, data.NamedArguments.Single().Value.Value);
        Assert.Equal(attribute, data.ApplicationSyntaxReference.GetSyntax());
        Assert.Equal(MethodKind.Constructor, ((IMethodSymbol)model.GetSymbolInfo(attribute).Symbol).MethodKind);
        Assert.IsAssignableFrom<IAttributeOperation>(model.GetOperation(attribute));
        var namedArgument = attribute.ArgumentList.Arguments.Last();
        Assert.Equal(SymbolKind.Property, model.GetSymbolInfo(namedArgument.NameEquals.Name).Symbol.Kind);
        Assert.Equal(3, model.GetConstantValue(namedArgument.Expression).Value);
        Assert.True(SymbolEqualityComparer.Default.Equals(compilation.GetTypeByMetadataName("System.Type").GetPublicSymbol(),
            model.GetTypeInfo(attribute.ArgumentList.Arguments[0].Expression).Type));

        var replacement = attribute.WithArgumentList(SyntaxFactory.ParseAttributeArgumentList("(typeof(string), nameof(value))"));
        Assert.True(model.TryGetSpeculativeSemanticModel(attribute.ArgumentList.Position, replacement, out var speculativeModel));
        Assert.IsAssignableFrom<IAttributeOperation>(speculativeModel.GetOperation(replacement));
    }

    [Fact]
    public void InvalidAttributeOperation()
    {
        var source = """
            #line 100
            union U([Missing] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("MissingAttribute").WithLocation(100, 10),
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("Missing").WithLocation(100, 10));

        var tree = compilation.SyntaxTrees.First();
        var caseSyntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var attribute = caseSyntax.AttributeLists.Single().Attributes.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.NotNull(model.GetDeclaredSymbol(caseSyntax));
        Assert.Null(model.GetSymbolInfo(attribute).Symbol);
        var operation = Assert.IsAssignableFrom<IAttributeOperation>(model.GetOperation(attribute));
        Assert.IsAssignableFrom<IInvalidOperation>(operation.Operation);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PartialUnionUsesCaseDeclarationScope(bool listFirst)
    {
        var withCases = """
            using Alias = AAttribute;
            partial union U([Alias(nameof(value))] int, [Alias(nameof(value))] string);
            """;
        var withoutCases = """
            using Alias = WrongAttribute;
            partial union U { static System.Type Marker => typeof(Alias); }
            """;
        var attribute = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute
            {
                public AAttribute(string value) { }
            }
            [System.AttributeUsage(System.AttributeTargets.Method)]
            class WrongAttribute : System.Attribute;
            """;
        var parts = listFirst ? new[] { withCases, withoutCases } : new[] { withoutCases, withCases };
        var compilation = CreateCompilation([.. parts, attribute, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var tree = compilation.SyntaxTrees.Single(t => t.ToString() == withCases);
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var cases = declaration.ParameterList.Parameters;
        Assert.Equal(2, cases.Count);
        foreach (var syntax in cases)
        {
            var parameter = model.GetDeclaredSymbol(syntax);
            Assert.NotNull(parameter);
            Assert.Equal("value", Assert.Single(parameter.GetAttributes()).ConstructorArguments.Single().Value);
        }
    }

    [Fact]
    public void SkippedAndDuplicatePartialCaseLists()
    {
        var source = """
            using System;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute;
            #line 100
            partial union U([A] int, [A] __arglist, [A] string);
            #line 200
            partial union U([A] long);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_IllegalVarArgs, "__arglist").WithLocation(100, 30),
            Diagnostic(ErrorCode.ERR_MultipleRecordParameterLists, "([A] long)").WithLocation(200, 16));

        var tree = compilation.SyntaxTrees.First();
        var declarations = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().ToArray();
        var model = compilation.GetSemanticModel(tree);
        var firstCases = declarations[0].ParameterList.Parameters;
        var intParameter = model.GetDeclaredSymbol(firstCases[0]);
        var stringParameter = model.GetDeclaredSymbol(firstCases[2]);
        Assert.NotNull(intParameter);
        Assert.Null(model.GetDeclaredSymbol(firstCases[1]));
        Assert.NotNull(stringParameter);
        Assert.False(SymbolEqualityComparer.Default.Equals(intParameter, stringParameter));
        Assert.Single(intParameter.GetAttributes());
        Assert.Single(stringParameter.GetAttributes());
        Assert.Null(model.GetDeclaredSymbol(declarations[1].ParameterList.Parameters.Single()));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RejectedPartialListDoesNotChangeCaseAttributes(bool stringFirst)
    {
        var first = stringFirst ? "[A(2)] string" : "[A(1)] int";
        var second = stringFirst ? "[A(1)] int" : "[A(2)] string";
        var source = $$"""
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute { public AAttribute(int value) { } }
            #line 100
            partial union U({{first}});
            #line 200
            partial union U({{second}});
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_MultipleRecordParameterLists, $"({second})").WithLocation(200, 16));

        var tree = compilation.SyntaxTrees.First();
        var declarations = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().ToArray();
        var model = compilation.GetSemanticModel(tree);
        var selected = model.GetDeclaredSymbol(declarations[0].ParameterList.Parameters.Single());
        var rejected = model.GetDeclaredSymbol(declarations[1].ParameterList.Parameters.Single());
        Assert.Equal(stringFirst ? 2 : 1, Assert.Single(selected.GetAttributes()).ConstructorArguments.Single().Value);
        Assert.Null(rejected);
    }

    [Fact]
    public void ConflictingPartialKindsWithCaseAttributes()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            partial struct U;
            #line 200
            partial union U([A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_PartialTypeKindConflict, "U").WithArguments("U").WithLocation(200, 15));
        var tree = compilation.SyntaxTrees.First();
        var caseSyntax = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        _ = compilation.GetSemanticModel(tree).GetDeclaredSymbol(caseSyntax);
    }

    [Fact]
    public void DuplicateCaseTypesKeepDistinctAttributes()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute
            {
                public AAttribute(int value) { }
            }
            #line 100
            union U([A(1)] int, [A(2)] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_MemberAlreadyExists, "int").WithArguments("U", "U").WithLocation(100, 28));

        var tree = compilation.SyntaxTrees.First();
        var cases = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters;
        var model = compilation.GetSemanticModel(tree);
        var first = model.GetDeclaredSymbol(cases[0]);
        var second = model.GetDeclaredSymbol(cases[1]);
        Assert.False(SymbolEqualityComparer.Default.Equals(first, second));
        Assert.Equal(1, Assert.Single(first.GetAttributes()).ConstructorArguments.Single().Value);
        Assert.Equal(2, Assert.Single(second.GetAttributes()).ConstructorArguments.Single().Value);
    }

    [Fact]
    public void RecursiveUnionCasesWithAttributes()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            union U([A] U, [A] V);
            union V([A] U, [A] V);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            options: TestOptions.ReleaseDll.WithConcurrentBuild(false), parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        foreach (var name in new[] { "U", "V" })
        {
            var constructors = compilation.GetTypeByMetadataName(name).InstanceConstructors.Where(c => c.Parameters.Length == 1).ToArray();
            Assert.Equal(2, constructors.Length);
            Assert.All(constructors, c => Assert.Single(c.Parameters[0].GetAttributes()));
        }
    }

    [Fact]
    public void AttributesWithMissingUnionInfrastructure()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([A] int);
            """;
        var missingAttribute = CreateCompilation([source, IUnionSource], parseOptions: TestOptions.RegularPreview);
        missingAttribute.VerifyEmitDiagnostics(
            Diagnostic(ErrorCode.ERR_MissingPredefinedMember, "U").WithArguments("System.Runtime.CompilerServices.UnionAttribute", ".ctor").WithLocation(100, 7));
        Assert.Single(missingAttribute.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0].GetAttributes());

        var missingInterface = CreateCompilation([source, UnionAttributeSource], parseOptions: TestOptions.RegularPreview);
        missingInterface.VerifyEmitDiagnostics(
            Diagnostic(ErrorCode.ERR_PredefinedTypeNotFound, "U").WithArguments("System.Runtime.CompilerServices.IUnion").WithLocation(100, 7));
        Assert.Single(missingInterface.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0].GetAttributes());

        var missingObject = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        missingObject.MakeTypeMissing(SpecialType.System_Object);
        Assert.NotEmpty(missingObject.GetDiagnostics());
        _ = missingObject.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0].GetAttributes();
    }

    [Fact]
    public void InvalidAttributeDoesNotHideValidNeighbors()
    {
        var source = """
            [System.AttributeUsage(System.AttributeTargets.Parameter)]
            class AAttribute : System.Attribute;
            #line 100
            union U([Missing][A] int, [A] string);
            union V([A] bool);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("MissingAttribute").WithLocation(100, 10),
            Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Missing").WithArguments("Missing").WithLocation(100, 10));

        var parameters = compilation.GetTypeByMetadataName("U").InstanceConstructors.Where(c => c.Parameters.Length == 1)
            .Select(c => c.Parameters[0]).ToArray();
        Assert.Equal(2, parameters.Length);
        Assert.All(parameters, p => Assert.Single(p.GetAttributes(), a => a.AttributeClass.Name == "AAttribute"));
        var next = compilation.GetTypeByMetadataName("V").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        Assert.Single(next.GetAttributes(), a => a.AttributeClass.Name == "AAttribute");
    }

    [Fact]
    public void CallerArgumentExpressionSelfReference()
    {
        var source = """
            using System.Runtime.CompilerServices;
            using System.Runtime.InteropServices;
            #line 100
            union U([CallerArgumentExpression(nameof(value)), Optional, DefaultParameterValue(null)] string);
            """;
        var compilation = CreateCompilation([source, CallerArgumentExpressionAttributeDefinition, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_CallerArgumentExpressionAttributeSelfReferential, "CallerArgumentExpression").WithArguments("value").WithLocation(100, 10));
    }

    [Theory]
    [InlineData("other")]
    [InlineData("")]
    [InlineData("int")]
    public void CallerArgumentExpressionUnknownParameter(string parameterName)
    {
        var source = $$"""
            using System.Runtime.CompilerServices;
            using System.Runtime.InteropServices;
            #line 100
            union U([CallerArgumentExpression("{{parameterName}}"), Optional, DefaultParameterValue("")] string, int);
            """;
        var compilation = CreateCompilation([source, CallerArgumentExpressionAttributeDefinition, UnionAttributeSource, IUnionSource],
            parseOptions: TestOptions.RegularPreview);
        compilation.VerifyDiagnostics(
            Diagnostic(ErrorCode.WRN_CallerArgumentExpressionAttributeHasInvalidParameterName, "CallerArgumentExpression").WithArguments("value").WithLocation(100, 10));
    }

    [Theory]
    [InlineData("CallerLineNumber", "int", "0")]
    [InlineData("CallerFilePath", "string", "\"\"")]
    [InlineData("CallerMemberName", "string", "\"\"")]
    public void CallerInfoWithAttributeDefault(string attribute, string type, string defaultValue)
    {
        var source = $$"""
            using System.Runtime.CompilerServices;
            using System.Runtime.InteropServices;
            public union U([{{attribute}}, Optional, DefaultParameterValue({{defaultValue}})] {{type}});
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource],
            targetFramework: TargetFramework.NetCoreApp, parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();
        verify(compilation);
        verify(CreateCompilation("", references: [compilation.EmitToImageReference()], targetFramework: TargetFramework.NetCoreApp));

        void verify(CSharpCompilation compilation)
        {
            var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.True(parameter.IsOptional);
            Assert.True(parameter.HasExplicitDefaultValue);
            Assert.Equal(attribute == "CallerLineNumber", parameter.IsCallerLineNumber);
            Assert.Equal(attribute == "CallerFilePath", parameter.IsCallerFilePath);
            Assert.Equal(attribute == "CallerMemberName", parameter.IsCallerMemberName);
        }
    }

    [Fact]
    public void AttributeCallerMemberName()
    {
        var source = """
            using System;
            using System.Runtime.CompilerServices;
            [AttributeUsage(AttributeTargets.Parameter)]
            class AAttribute : Attribute
            {
                public AAttribute([CallerMemberName] string name = "", [CallerLineNumber] int line = 0, [CallerFilePath] string file = "") { }
            }
            #line 100
            class C { public C([A] int value) { } }
            #line 200
            union U([A] int);
            """;
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        compilation.VerifyEmitDiagnostics();

        var control = compilation.GetTypeByMetadataName("C").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        var actual = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
        var controlArguments = Assert.Single(control.GetAttributes()).ConstructorArguments.ToArray();
        var actualArguments = Assert.Single(actual.GetAttributes()).ConstructorArguments.ToArray();
        Assert.Equal(".ctor", controlArguments[0].Value);
        Assert.Equal(controlArguments[0].Value, actualArguments[0].Value);
        Assert.Equal(100, controlArguments[1].Value);
        Assert.Equal(200, actualArguments[1].Value);
        Assert.Equal(controlArguments[2].Value, actualArguments[2].Value);
        Assert.Equal(compilation.SyntaxTrees.First().FilePath, actualArguments[2].Value);
    }

    [Fact]
    public void ConditionalAttributes()
    {
        var source = """
            using System;
            using System.Diagnostics;
            [Conditional("INCLUDE")]
            [AttributeUsage(AttributeTargets.Parameter)]
            public class AAttribute : Attribute;
            public union U([A] int);
            """;

        foreach (var include in new[] { false, true })
        {
            var options = TestOptions.RegularPreview.WithPreprocessorSymbols(include ? new[] { "INCLUDE" } : System.Array.Empty<string>());
            var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: options);
            compilation.VerifyEmitDiagnostics();

            var parameter = compilation.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.Single(parameter.GetAttributes(), a => a.AttributeClass.Name == "AAttribute");

            var consumer = CreateCompilation("", references: [compilation.EmitToImageReference()]);
            var importedParameter = consumer.GetTypeByMetadataName("U").InstanceConstructors.Single(c => c.Parameters.Length == 1).Parameters[0];
            Assert.Equal(include, importedParameter.GetAttributes().Any(a => a.AttributeClass.Name == "AAttribute"));
        }
    }
}
