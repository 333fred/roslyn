// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

// Copied from:
// https://github.com/dotnet/runtime/blob/80a8cdbdb9c36ccab97374afcb2f0491ae58a94d/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/IUnion.cs

#if NET11_0_OR_GREATER

using System.Runtime.CompilerServices;

#pragma warning disable RS0016 // Add public types and members to the declared API (this is a supporting forwarder for an internal polyfill API)
[assembly: TypeForwardedTo(typeof(UnionAttribute))]
#pragma warning restore RS0016 // Add public types and members to the declared API

#else

namespace System.Runtime.CompilerServices
{
    /// <summary>
    /// Indicates that a class or struct is a union type, enabling compiler support for union behaviors.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Any class or struct annotated with this attribute is recognized by the C# compiler as a union type.
    /// Union types may support behaviors such as implicit conversions from case types, pattern matching
    /// that unwraps the union's contents, and switch exhaustiveness checking.
    /// </para>
    /// </remarks>
    /// <seealso cref="IUnion" />
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false, Inherited = false)]
    internal sealed class UnionAttribute : Attribute
    {
    }
}

#endif
