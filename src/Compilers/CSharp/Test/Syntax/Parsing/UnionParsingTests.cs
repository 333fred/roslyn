// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Linq;
using System.IO;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests;

public class UnionParsingTests : ParsingTests
{
    public UnionParsingTests(ITestOutputHelper output) : base(output) { }

    [Theory, CombinatorialData]
    public void UnionCaseAttributes(bool useCSharp15)
    {
        var src = """
union U(int, [A] string);
""";
        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.IntKeyword);
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.AttributeList);
                        {
                            N(SyntaxKind.OpenBracketToken);
                            N(SyntaxKind.Attribute);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "A");
                                }
                            }
                            N(SyntaxKind.CloseBracketToken);
                        }
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.StringKeyword);
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void UnionCaseAttributeLists(bool useCSharp15)
    {
        var src = """
union U(int, [A(1, typeof(System.Collections.Generic.Dictionary<int, string>)), B][param: C] (int x, string y), [D] System.Collections.Generic.List<string>);
""";
        var tree = SyntaxFactory.ParseSyntaxTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);
        tree.GetDiagnostics().Verify();

        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var cases = declaration.ParameterList.Parameters;
        Assert.Equal(3, cases.Count);
        Assert.Empty(cases[0].AttributeLists);
        Assert.Equal(2, cases[1].AttributeLists.Count);
        Assert.Equal(2, cases[1].AttributeLists[0].Attributes.Count);
        Assert.Equal("param", cases[1].AttributeLists[1].Target.Identifier.ValueText);
        Assert.IsType<TupleTypeSyntax>(cases[1].Type);
        Assert.Single(cases[2].AttributeLists);
        Assert.IsType<QualifiedNameSyntax>(cases[2].Type);
        Assert.All(cases, parameter => Assert.Equal(SyntaxKind.None, parameter.Identifier.Kind()));
        Assert.Equal(src, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void UnionCaseAttributeMissingType()
    {
        var src = """
union U([A], [B] string);
""";
        UsingTree(src, TestOptions.RegularPreview,
            Diagnostic(ErrorCode.ERR_TypeExpected, ",").WithLocation(1, 12));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.AttributeList);
                        {
                            N(SyntaxKind.OpenBracketToken);
                            N(SyntaxKind.Attribute);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "A");
                                }
                            }
                            N(SyntaxKind.CloseBracketToken);
                        }
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.AttributeList);
                        {
                            N(SyntaxKind.OpenBracketToken);
                            N(SyntaxKind.Attribute);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "B");
                                }
                            }
                            N(SyntaxKind.CloseBracketToken);
                        }
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.StringKeyword);
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory]
    [InlineData("union U([")]
    [InlineData("union U([param:")]
    [InlineData("union U([A(")]
    [InlineData("union U([A]")]
    [InlineData("union U([A] int")]
    [InlineData("union U([], [A] int);")]
    [InlineData("union U([param:] int, [A] string);")]
    [InlineData("union U([A,,B] int, [A] string);")]
    [InlineData("union U([A] int [B] string);")]
    [InlineData("union U([A(,)] int, [B] string);")]
    [InlineData("union U([A(Name = )] int, [B] string);")]
    [InlineData("union U([A(1] int, [B] string);")]
    [InlineData("union U([A int, [B] string);")]
    [InlineData("union U([A] int) { class C { }")]
    [InlineData("union U([param A] int, [B] string);")]
    [InlineData("union U([: A] int, [B] string);")]
    [InlineData("union U([param: : A] int, [B] string);")]
    [InlineData("union U(, [A] int);")]
    [InlineData("union U([A] int,, [B] string);")]
    [InlineData("union U([A] int,);")]
    public void IncompleteUnionCaseAttribute(string source)
    {
        var compilation = CreateCompilation([source, UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        var tree = compilation.SyntaxTrees.First();
        var root = tree.GetRoot();
        Assert.Equal(source, root.ToFullString());
        Assert.NotEmpty(tree.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in root.DescendantNodes().OfType<ParameterSyntax>())
        {
            _ = model.GetDeclaredSymbol(syntax)?.GetAttributes();
            if (syntax.Type is not null)
            {
                _ = model.GetTypeInfo(syntax.Type);
            }
        }

        Assert.NotEmpty(compilation.GetDiagnostics());
        using var stream = new MemoryStream();
        Assert.False(compilation.Emit(stream).Success);
    }

    [Theory]
    [InlineData("union U([")]
    [InlineData("union U([param:")]
    [InlineData("union U([A(")]
    [InlineData("union U([A]")]
    [InlineData("union U([A] int")]
    [InlineData("union U([A(,)] int, [B] string);")]
    [InlineData("union U([A(Name = )] int, [B] string);")]
    [InlineData("union U([A(1] int, [B] string);")]
    [InlineData("union U([A int, [B] string);")]
    [InlineData("union U([A] int) { class C { }")]
    [InlineData("union U([param A] int, [B] string);")]
    [InlineData("union U([: A] int, [B] string);")]
    [InlineData("union U([param: : A] int, [B] string);")]
    [InlineData("union U([A] int [B] string);")]
    [InlineData("union U(, [A] int);")]
    [InlineData("union U([A] int,, [B] string);")]
    [InlineData("union U([A] int,);")]
    public void UnionCaseRecoveryDiagnosticBaselines(string source)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source, TestOptions.RegularPreview);
        DiagnosticDescription[] expected = source switch
        {
            "union U([" =>
            [
                Diagnostic(ErrorCode.ERR_IdentifierExpected, "").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_SyntaxError, "").WithArguments("]").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_TypeExpected, "").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 10)
            ],
            "union U([param:" =>
            [
                Diagnostic(ErrorCode.ERR_IdentifierExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_SyntaxError, "").WithArguments("]").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_TypeExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 16)
            ],
            "union U([A(" =>
            [
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_SyntaxError, "").WithArguments("]").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_TypeExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 12)
            ],
            "union U([A]" =>
            [
                Diagnostic(ErrorCode.ERR_TypeExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 12)
            ],
            "union U([A] int" =>
            [
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 16),
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 16)
            ],
            "union U([A(,)] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_InvalidExprTerm, ",").WithArguments(",").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_InvalidExprTerm, ")").WithArguments(")").WithLocation(1, 13)
            ],
            "union U([A(Name = )] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_InvalidExprTerm, ")").WithArguments(")").WithLocation(1, 19)
            ],
            "union U([A(1] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "]").WithLocation(1, 13)
            ],
            "union U([A int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_SyntaxError, "int").WithArguments(",").WithLocation(1, 12),
                Diagnostic(ErrorCode.ERR_SyntaxError, "[").WithArguments("]").WithLocation(1, 17)
            ],
            "union U([A] int) { class C { }" =>
            [
                Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 31)
            ],
            "union U([param A] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_SyntaxError, "A").WithArguments(",").WithLocation(1, 16)
            ],
            "union U([: A] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_IdentifierExpected, ":").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_SyntaxError, ":").WithArguments("]").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_TypeExpected, ":").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, ":").WithLocation(1, 10),
                Diagnostic(ErrorCode.ERR_SyntaxError, "]").WithArguments(",").WithLocation(1, 13),
                Diagnostic(ErrorCode.ERR_TypeExpected, "[").WithLocation(1, 20),
                Diagnostic(ErrorCode.ERR_SyntaxError, "string").WithArguments(",").WithLocation(1, 24),
                Diagnostic(ErrorCode.ERR_SyntaxError, ")").WithArguments(",").WithLocation(1, 30)
            ],
            "union U([param: : A] int, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_IdentifierExpected, ":").WithLocation(1, 17),
                Diagnostic(ErrorCode.ERR_SyntaxError, ":").WithArguments("]").WithLocation(1, 17),
                Diagnostic(ErrorCode.ERR_TypeExpected, ":").WithLocation(1, 17),
                Diagnostic(ErrorCode.ERR_CloseParenExpected, ":").WithLocation(1, 17),
                Diagnostic(ErrorCode.ERR_SyntaxError, "]").WithArguments(",").WithLocation(1, 20),
                Diagnostic(ErrorCode.ERR_TypeExpected, "[").WithLocation(1, 27),
                Diagnostic(ErrorCode.ERR_SyntaxError, "string").WithArguments(",").WithLocation(1, 31),
                Diagnostic(ErrorCode.ERR_SyntaxError, ")").WithArguments(",").WithLocation(1, 37)
            ],
            "union U([A] int [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_SyntaxError, "string").WithArguments(",").WithLocation(1, 21)
            ],
            "union U(, [A] int);" =>
            [
                Diagnostic(ErrorCode.ERR_TypeExpected, ",").WithLocation(1, 9)
            ],
            "union U([A] int,, [B] string);" =>
            [
                Diagnostic(ErrorCode.ERR_TypeExpected, ",").WithLocation(1, 17)
            ],
            "union U([A] int,);" =>
            [
                Diagnostic(ErrorCode.ERR_TypeExpected, ")").WithLocation(1, 17)
            ],
            _ => throw new System.InvalidOperationException(source)
        };
        tree.GetDiagnostics().Verify(expected);
    }

    [Theory]
    [InlineData("union U([] int, [A] string);", "]", 10)]
    [InlineData("union U([param:] int, [A] string);", "]", 16)]
    [InlineData("union U([A,,B] int, [A] string);", ",", 12)]
    public void InvalidUnionCaseAttributeRecoversAtNextCase(string source, string token, int column)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source, TestOptions.RegularPreview);
        tree.GetDiagnostics().Verify(Diagnostic(ErrorCode.ERR_IdentifierExpected, token).WithLocation(1, column));

        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var cases = declaration.ParameterList.Parameters;
        Assert.Equal(2, cases.Count);
        Assert.Equal("A", cases[1].AttributeLists.Single().Attributes.Single().Name.ToString());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void EmptyUnionCaseAttributeListTree()
    {
        var source = "union U([] int, [A] string);";
        UsingTree(source, TestOptions.RegularPreview,
            Diagnostic(ErrorCode.ERR_IdentifierExpected, "]").WithLocation(1, 10));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.AttributeList);
                        {
                            N(SyntaxKind.OpenBracketToken);
                            M(SyntaxKind.Attribute);
                            {
                                M(SyntaxKind.IdentifierName);
                                {
                                    M(SyntaxKind.IdentifierToken);
                                }
                            }
                            N(SyntaxKind.CloseBracketToken);
                        }
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.IntKeyword);
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.AttributeList);
                        {
                            N(SyntaxKind.OpenBracketToken);
                            N(SyntaxKind.Attribute);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "A");
                                }
                            }
                            N(SyntaxKind.CloseBracketToken);
                        }
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.StringKeyword);
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void UnionCaseAttributeAliasQualifier()
    {
        var source = "union U([param:: A] int);";
        var tree = SyntaxFactory.ParseSyntaxTree(source, TestOptions.RegularPreview);
        tree.GetDiagnostics().Verify();
        var parameter = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        var list = parameter.AttributeLists.Single();
        Assert.Null(list.Target);
        Assert.IsType<AliasQualifiedNameSyntax>(list.Attributes.Single().Name);
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Theory]
    [InlineData("[A] global::N.T", "global::N.T")]
    [InlineData("[A] N.G<int, string>", "N.G<int, string>")]
    [InlineData("[A] int?", "int?")]
    [InlineData("[A] string?[]", "string?[]")]
    [InlineData("[A] (int left, string right)", "(int left, string right)")]
    [InlineData("[A] /* comment */ int", "int")]
    public void UnionCaseAttributesWithTypeShapes(string caseText, string typeText)
    {
        var source = $"union U({caseText}, bool);";
        var tree = SyntaxFactory.ParseSyntaxTree(source, TestOptions.RegularPreview);
        tree.GetDiagnostics().Verify();

        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var cases = declaration.ParameterList.Parameters;
        Assert.Equal(2, cases.Count);
        Assert.Single(cases[0].AttributeLists);
        Assert.Equal(typeText, cases[0].Type.ToString());
        Assert.Equal(SyntaxKind.None, cases[0].Identifier.Kind());
        Assert.Empty(cases[1].AttributeLists);
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Theory, CombinatorialData]
    public void UnionCaseAttributesWithDirectives(bool defineDebug)
    {
        var source = """
            partial union U(
            #if DEBUG
                [A]
            #else
                [B]
            #endif
                int);
            """;
        var options = TestOptions.RegularPreview.WithPreprocessorSymbols(defineDebug ? new[] { "DEBUG" } : System.Array.Empty<string>());
        var tree = SyntaxFactory.ParseSyntaxTree(source, options);
        tree.GetDiagnostics().Verify();
        var parameter = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single().ParameterList.Parameters.Single();
        Assert.Equal(defineDebug ? "A" : "B", parameter.AttributeLists.Single().Attributes.Single().Name.ToString());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void UnionCaseAttributesInCSharp14()
    {
        var source = "union U([A] int);";
        var tree = SyntaxFactory.ParseSyntaxTree(source, TestOptions.Regular14);
        tree.GetDiagnostics().Verify(Diagnostic(ErrorCode.ERR_IdentifierExpected, ")").WithLocation(1, 16));
        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<LocalFunctionStatementSyntax>());
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Theory, CombinatorialData]
    public void Union_01(bool useCSharp15)
    {
        var src = """
union U1(E1);
""";
        UsingTree(src, TestOptions.Regular14,
            // (1,12): error CS1001: Identifier expected
            // union U1(E1);
            Diagnostic(ErrorCode.ERR_IdentifierExpected, ")").WithLocation(1, 12)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "union");
                    }
                    N(SyntaxKind.IdentifierToken, "U1");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.Parameter);
                        {
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "E1");
                            }
                            M(SyntaxKind.IdentifierToken);
                        }
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void Union_02()
    {
        var src = """
record union U1(E1);
""";

        UsingTree(src, TestOptions.RegularPreview,
            // (1,14): error CS1514: { expected
            // record union U1(E1);
            Diagnostic(ErrorCode.ERR_LbraceExpected, "U1").WithLocation(1, 14),
            // (1,14): error CS1513: } expected
            // record union U1(E1);
            Diagnostic(ErrorCode.ERR_RbraceExpected, "U1").WithLocation(1, 14),
            // (1,14): error CS8803: Top-level statements must precede namespace and type declarations.
            // record union U1(E1);
            Diagnostic(ErrorCode.ERR_TopLevelStatementAfterNamespaceOrType, "U1(E1);").WithLocation(1, 14)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.RecordDeclaration);
            {
                N(SyntaxKind.RecordKeyword);
                N(SyntaxKind.IdentifierToken, "union");
                M(SyntaxKind.OpenBraceToken);
                M(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.ExpressionStatement);
                {
                    N(SyntaxKind.InvocationExpression);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "U1");
                        }
                        N(SyntaxKind.ArgumentList);
                        {
                            N(SyntaxKind.OpenParenToken);
                            N(SyntaxKind.Argument);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "E1");
                                }
                            }
                            N(SyntaxKind.CloseParenToken);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Union_03(bool useCSharp15)
    {
        var src = """
union M()
{
    return default;
}
""";
        UsingTree(src, TestOptions.Regular14);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "union");
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.ReturnStatement);
                        {
                            N(SyntaxKind.ReturnKeyword);
                            N(SyntaxKind.DefaultLiteralExpression);
                            {
                                N(SyntaxKind.DefaultKeyword);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview,
            // (1,9): error CS1031: Type expected
            // union M()
            Diagnostic(ErrorCode.ERR_TypeExpected, ")").WithLocation(1, 9),
            // (3,5): error CS1519: Invalid token 'return' in a member declaration
            //     return default;
            Diagnostic(ErrorCode.ERR_InvalidMemberDecl, "return").WithArguments("return").WithLocation(3, 5)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "M");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    M(SyntaxKind.Parameter);
                    {
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Union_04(bool useCSharp15)
    {
        var src = """
partial union U1(E1);
""";
        UsingTree(src, TestOptions.Regular14,
            // (1,20): error CS1001: Identifier expected
            // partial union U1(E1);
            Diagnostic(ErrorCode.ERR_IdentifierExpected, ")").WithLocation(1, 20)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.MethodDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "union");
                }
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Union_05(bool useCSharp15)
    {
        var src = """
ref union U1(E1);
""";
        UsingTree(src, TestOptions.Regular14,
            // (1,16): error CS1001: Identifier expected
            // ref union U1(E1);
            Diagnostic(ErrorCode.ERR_IdentifierExpected, ")").WithLocation(1, 16)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.RefType);
                    {
                        N(SyntaxKind.RefKeyword);
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "union");
                        }
                    }
                    N(SyntaxKind.IdentifierToken, "U1");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.Parameter);
                        {
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "E1");
                            }
                            M(SyntaxKind.IdentifierToken);
                        }
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.RefKeyword);
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        var comp = CreateCompilation([src, "struct E1;", UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        comp.VerifyDiagnostics(
            // (1,11): error CS0106: The modifier 'ref' is not valid for this item
            // ref union U1(E1);
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "U1").WithArguments("ref").WithLocation(1, 11)
            );

    }

    [Theory, CombinatorialData]
    public void Union_06(bool useCSharp15)
    {
        var src = """
ref partial union U1(E1);
""";
        UsingTree(src, TestOptions.Regular14,
            // (1,24): error CS1001: Identifier expected
            // ref partial union U1(E1);
            Diagnostic(ErrorCode.ERR_IdentifierExpected, ")").WithLocation(1, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.MethodDeclaration);
            {
                N(SyntaxKind.RefKeyword);
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "union");
                }
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        UsingTree(src, useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.RefKeyword);
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        CreateCompilation(
            [src, "struct E1;", UnionAttributeSource, IUnionSource],
            parseOptions: useCSharp15 ? TestOptions.Regular15 : TestOptions.RegularPreview).VerifyDiagnostics(
                // (1,19): error CS0106: The modifier 'ref' is not valid for this item
                // ref partial union U1(E1);
                Diagnostic(ErrorCode.ERR_BadMemberFlag, "U1").WithArguments("ref").WithLocation(1, 19));
    }

    [Fact]
    public void Union_07()
    {
        var src = """
union U1(E1, E2, E3);
""";
        UsingTree(src, TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E2");
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E3");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void Union_08()
    {
        var src = """
union U1(E1) : I1(1);
""";
        UsingTree(src, TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.BaseList);
                {
                    N(SyntaxKind.ColonToken);
                    N(SyntaxKind.PrimaryConstructorBaseType);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "I1");
                        }
                        N(SyntaxKind.ArgumentList);
                        {
                            N(SyntaxKind.OpenParenToken);
                            N(SyntaxKind.Argument);
                            {
                                N(SyntaxKind.NumericLiteralExpression);
                                {
                                    N(SyntaxKind.NumericLiteralToken, "1");
                                }
                            }
                            N(SyntaxKind.CloseParenToken);
                        }
                    }
                }
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();

        var comp = CreateCompilation([src, "struct E1; interface I1;", UnionAttributeSource, IUnionSource], parseOptions: TestOptions.RegularPreview);
        comp.VerifyDiagnostics(
            // (1,18): error CS8861: Unexpected argument list.
            // union U1(E1) : I1(1);
            Diagnostic(ErrorCode.ERR_UnexpectedArgumentList, "(1)").WithLocation(1, 18)
            );
    }

    [Fact]
    public void Union_09()
    {
        var src = """
[Attr1]
public union U1<T1>(E1) : I1, I2 where T1 : class
{
    public void M1() { }
}
""";
        UsingTree(src, TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.AttributeList);
                {
                    N(SyntaxKind.OpenBracketToken);
                    N(SyntaxKind.Attribute);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "Attr1");
                        }
                    }
                    N(SyntaxKind.CloseBracketToken);
                }
                N(SyntaxKind.PublicKeyword);
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.TypeParameterList);
                {
                    N(SyntaxKind.LessThanToken);
                    N(SyntaxKind.TypeParameter);
                    {
                        N(SyntaxKind.IdentifierToken, "T1");
                    }
                    N(SyntaxKind.GreaterThanToken);
                }
                N(SyntaxKind.ParameterList);
                {
                    N(SyntaxKind.OpenParenToken);
                    N(SyntaxKind.Parameter);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "E1");
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                N(SyntaxKind.BaseList);
                {
                    N(SyntaxKind.ColonToken);
                    N(SyntaxKind.SimpleBaseType);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "I1");
                        }
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.SimpleBaseType);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "I2");
                        }
                    }
                }
                N(SyntaxKind.TypeParameterConstraintClause);
                {
                    N(SyntaxKind.WhereKeyword);
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "T1");
                    }
                    N(SyntaxKind.ColonToken);
                    N(SyntaxKind.ClassConstraint);
                    {
                        N(SyntaxKind.ClassKeyword);
                    }
                }
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PublicKeyword);
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M1");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void Union_10()
    {
        var src = """
union U1;
""";
        UsingTree(src, TestOptions.RegularPreview);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.UnionDeclaration);
            {
                N(SyntaxKind.UnionKeyword);
                N(SyntaxKind.IdentifierToken, "U1");
                N(SyntaxKind.SemicolonToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void Union_11()
    {
        var src = """
record union U1;
""";
        UsingTree(src, TestOptions.RegularPreview,
            // (1,14): error CS1514: { expected
            // record union U1;
            Diagnostic(ErrorCode.ERR_LbraceExpected, "U1").WithLocation(1, 14),
            // (1,14): error CS1513: } expected
            // record union U1;
            Diagnostic(ErrorCode.ERR_RbraceExpected, "U1").WithLocation(1, 14),
            // (1,14): error CS8803: Top-level statements must precede namespace and type declarations.
            // record union U1;
            Diagnostic(ErrorCode.ERR_TopLevelStatementAfterNamespaceOrType, "U1;").WithLocation(1, 14)
            );

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.RecordDeclaration);
            {
                N(SyntaxKind.RecordKeyword);
                N(SyntaxKind.IdentifierToken, "union");
                M(SyntaxKind.OpenBraceToken);
                M(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.ExpressionStatement);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "U1");
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }
}
