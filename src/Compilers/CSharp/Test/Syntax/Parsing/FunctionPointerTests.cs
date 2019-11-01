// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests.Parsing
{
    public class FunctionPointerTests : ParsingTests
    {
        public FunctionPointerTests(ITestOutputHelper output) : base(output)
        {
        }

        [Fact]
        public void SimpleFunctionPointerTest()
        {
            UsingStatement("func* int(string, Foo) ptr;", options: TestOptions.RegularPreview);
            N(SyntaxKind.LocalDeclarationStatement);
            {
                N(SyntaxKind.VariableDeclaration);
                {
                    N(SyntaxKind.FunctionPointerType);
                    {
                        N(SyntaxKind.FuncKeyword);
                        N(SyntaxKind.AsteriskToken);
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.IntKeyword);
                        }
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.StringKeyword);
                        }
                        N(SyntaxKind.CommaToken);
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "Foo");
                        }
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.VariableDeclarator);
                    {
                        N(SyntaxKind.IdentifierToken, "ptr");
                    }
                }
                N(SyntaxKind.SemicolonToken);
            }
            EOF();
        }

        [Theory]
        [InlineData("cdecl", SyntaxKind.CdeclKeyword)]
        [InlineData("managed", SyntaxKind.ManagedKeyword)]
        [InlineData("stdcall", SyntaxKind.StdcallKeyword)]
        [InlineData("thiscall", SyntaxKind.ThiscallKeyword)]
        [InlineData("unmanaged", SyntaxKind.UnmanagedKeyword)]
        public void SupportedCallingConventions(string conventionString, SyntaxKind conventionKind)
        {
            UsingStatement($"func* {conventionString} int(string, Foo) ptr;", options: TestOptions.RegularPreview);
            N(SyntaxKind.LocalDeclarationStatement);
            {
                N(SyntaxKind.VariableDeclaration);
                {
                    N(SyntaxKind.FunctionPointerType);
                    {
                        N(SyntaxKind.FuncKeyword);
                        N(SyntaxKind.AsteriskToken);
                        N(conventionKind);
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.IntKeyword);
                        }
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.PredefinedType);
                        {
                            N(SyntaxKind.StringKeyword);
                        }
                        N(SyntaxKind.CommaToken);
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "Foo");
                        }
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.VariableDeclarator);
                    {
                        N(SyntaxKind.IdentifierToken, "ptr");
                    }
                }
                N(SyntaxKind.SemicolonToken);
            }
            EOF();
        }

        [Fact]
        public void InvalidCallingConvetionTupleReturnType()
        {
            UsingStatement($"func* invalidcallingconvetion (int, string)() ptr;", options: TestOptions.RegularPreview);
        }

        [Fact]
        public void LangVersion8()
        {
            UsingStatement("func* cdecl int(string, Foo) ptr;", options: TestOptions.Regular8,
                    // (1,1): error CS1073: Unexpected token ')'
                    // func* cdecl int(string, Foo) ptr;
                    Diagnostic(ErrorCode.ERR_UnexpectedToken, "func* cdecl int(string, Foo").WithArguments(")").WithLocation(1, 1),
                    // (1,13): error CS1003: Syntax error, ',' expected
                    // func* cdecl int(string, Foo) ptr;
                    Diagnostic(ErrorCode.ERR_SyntaxError, "int").WithArguments(",", "int").WithLocation(1, 13),
                    // (1,28): error CS1002: ; expected
                    // func* cdecl int(string, Foo) ptr;
                    Diagnostic(ErrorCode.ERR_SemicolonExpected, ")").WithLocation(1, 28));
            N(SyntaxKind.LocalDeclarationStatement);
            {
                N(SyntaxKind.VariableDeclaration);
                {
                    N(SyntaxKind.PointerType);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "func");
                        }
                        N(SyntaxKind.AsteriskToken);
                    }
                    N(SyntaxKind.VariableDeclarator);
                    {
                        N(SyntaxKind.IdentifierToken, "cdecl");
                    }
                    N(SyntaxKind.CommaToken);
                    N(SyntaxKind.VariableDeclarator);
                    {
                        N(SyntaxKind.IdentifierToken, "Foo");
                    }
                }
                M(SyntaxKind.SemicolonToken);
            }
            EOF();
        }
    }
}
