// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

// Copied from:
// https://github.com/dotnet/runtime/blob/80a8cdbdb9c36ccab97374afcb2f0491ae58a94d/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/IsClosedTypeAttribute.cs

#if NET11_0_OR_GREATER


using System.Runtime.CompilerServices;

#pragma warning disable RS0016 // Add public types and members to the declared API (this is a supporting forwarder for an internal polyfill API)
[assembly: TypeForwardedTo(typeof(IsClosedTypeAttribute))]
#pragma warning restore RS0016 // Add public types and members to the declared API

#else

#nullable enable

using System.ComponentModel;

namespace System.Runtime.CompilerServices
{
    /// <summary>
    /// Reserved for use by a compiler for tracking metadata.
    /// This attribute should not be used by developers in source code.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    [AttributeUsage(AttributeTargets.Class, Inherited = false)]
    internal sealed class IsClosedTypeAttribute : Attribute
    {
        private Type[] _derivedTypes = Type.EmptyTypes;

        /// <summary>Initializes the attribute.</summary>
        public IsClosedTypeAttribute()
        {
        }

        /// <summary>Gets or sets the derived types of the closed type.</summary>
        /// <value>An array of the derived types of the closed type. A <see langword="null" /> value is normalized to an empty array.</value>
        public Type[] DerivedTypes
        {
            get => _derivedTypes;
            set => _derivedTypes = value ?? Type.EmptyTypes;
        }
    }
}
#endif
