// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.PooledObjects;
using System.Text;
using System.Diagnostics;
using System.Reflection.Metadata;
using System.Collections.Immutable;

namespace Microsoft.Cci
{
    internal static class TypeNameSerializer
    {
        internal static string GetSerializedTypeName(this ITypeReference typeReference, EmitContext context)
        {
            bool isAssemblyQualified = true;
            return GetSerializedTypeName(typeReference, context, ref isAssemblyQualified);
        }

        internal static string GetSerializedTypeName(this ITypeReference typeReference, EmitContext context, ref bool isAssemblyQualified)
        {
            var pooled = PooledStringBuilder.GetInstance();
            StringBuilder sb = pooled.Builder;
            IArrayTypeReference arrType = typeReference as IArrayTypeReference;
            if (arrType != null)
            {
                typeReference = arrType.GetElementType(context);
                bool isAssemQual = false;
                AppendSerializedTypeName(sb, typeReference, ref isAssemQual, context);
                if (arrType.IsSZArray)
                {
                    sb.Append("[]");
                }
                else
                {
                    sb.Append('[');
                    if (arrType.Rank == 1)
                    {
                        sb.Append('*');
                    }

                    sb.Append(',', (int)arrType.Rank - 1);

                    sb.Append(']');
                }

                goto done;
            }

            IPointerTypeReference pointer = typeReference as IPointerTypeReference;
            if (pointer != null)
            {
                typeReference = pointer.GetTargetType(context);
                bool isAssemQual = false;
                AppendSerializedTypeName(sb, typeReference, ref isAssemQual, context);
                sb.Append('*');
                goto done;
            }

            if (typeReference is IFunctionPointerTypeReference { Signature: { } functionPointerSignature })
            {
                // type
                //   : methodspec callConv type '*' '(' sigArgs0 ')'

                // methodspec
                //   : 'method'
                //   ;
                sb.Append("method ");

                serializeCallingConvention(sb, functionPointerSignature);

                serializeReturnOrParameter(
                    context,
                    sb,
                    functionPointerSignature.GetType(context),
                    functionPointerSignature.ReturnValueCustomModifiers,
                    functionPointerSignature.ReturnValueIsByRef,
                    functionPointerSignature.RefCustomModifiers);

                sb.Append("*(");

                // sigArgs0
                //   : /* EMPTY */
                //   | sigArgs1
                //   ;

                // sigArgs1
                //   : sigArg
                //   | sigArgs1 ',' sigArg
                //   ;

                // sigArg
                //   : paramAttr type
                //   ... /* Not supported in function pointers */
                //   ;

                // paramAttr
                //   : /* EMPTY */
                //   ... /* Not supported in function pointers */
                //   ;

                // We don't support paramAttrs on function pointer types or marshall clauses, so we ignore those bits.

                foreach (var param in functionPointerSignature.GetParameters(context))
                {
                    if (param.Index != 0)
                    {
                        sb.Append(", ");
                    }

                    serializeReturnOrParameter(context, sb, param.GetType(context), param.CustomModifiers, param.IsByReference, param.RefCustomModifiers);
                }

                sb.Append(')');
            }


            INamespaceTypeReference namespaceType = typeReference.AsNamespaceTypeReference;
            if (namespaceType != null)
            {
                var name = namespaceType.NamespaceName;
                if (name.Length != 0)
                {
                    sb.Append(name);
                    sb.Append('.');
                }

                sb.Append(GetMangledAndEscapedName(namespaceType));
                goto done;
            }


            if (typeReference.IsTypeSpecification())
            {
                ITypeReference uninstantiatedTypeReference = typeReference.GetUninstantiatedGenericType(context);

                ArrayBuilder<ITypeReference> consolidatedTypeArguments = ArrayBuilder<ITypeReference>.GetInstance();
                typeReference.GetConsolidatedTypeArguments(consolidatedTypeArguments, context);

                bool uninstantiatedTypeIsAssemblyQualified = false;
                sb.Append(GetSerializedTypeName(uninstantiatedTypeReference, context, ref uninstantiatedTypeIsAssemblyQualified));
                sb.Append('[');
                bool first = true;
                foreach (ITypeReference argument in consolidatedTypeArguments)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(',');
                    }

                    bool isAssemQual = true;
                    AppendSerializedTypeName(sb, argument, ref isAssemQual, context);
                }
                consolidatedTypeArguments.Free();

                sb.Append(']');
                goto done;
            }

            INestedTypeReference nestedType = typeReference.AsNestedTypeReference;
            if (nestedType != null)
            {
                bool nestedTypeIsAssemblyQualified = false;
                sb.Append(GetSerializedTypeName(nestedType.GetContainingType(context), context, ref nestedTypeIsAssemblyQualified));
                sb.Append('+');
                sb.Append(GetMangledAndEscapedName(nestedType));
                goto done;
            }

// TODO: error
done:
            if (isAssemblyQualified)
            {
                AppendAssemblyQualifierIfNecessary(sb, UnwrapTypeReference(typeReference, context), out isAssemblyQualified, context);
            }

            return pooled.ToStringAndFree();

            static void serializeCallingConvention(StringBuilder sb, ISignature functionPointerSignature)
            {
                // callConv
                //   : 'explicit' callKind
                //   | 'instance' callKind
                //   | callKind
                //   ;
                if ((functionPointerSignature.CallingConvention & CallingConvention.ExplicitThis) == CallingConvention.ExplicitThis)
                {
                    sb.Append("explicit ");
                }
                else if ((functionPointerSignature.CallingConvention & CallingConvention.HasThis) == CallingConvention.HasThis)
                {
                    sb.Append("instance ");
                }

                // callKind
                //   : /* EMPTY */
                //   | 'default' /* Equivalent to EMPTY */
                //   | 'vararg'
                //   | 'unmanaged' 'cdecl'
                //   | 'unmanaged' 'stdcall'
                //   | 'unmanaged' 'thiscall'
                //   | 'unmanaged' 'fastcall'
                //   | 'unmanaged'
                //   ;
                switch (functionPointerSignature.CallingConvention.ToSignatureConvention())
                {
                    case SignatureCallingConvention.Default:
                        // Empty is equivalent to default, so skip on encoding it.
                        break;
                    case SignatureCallingConvention.CDecl:
                        sb.Append("unmanaged cdecl ");
                        break;
                    case SignatureCallingConvention.StdCall:
                        sb.Append("unmanaged stdcall ");
                        break;
                    case SignatureCallingConvention.ThisCall:
                        sb.Append("unmanaged thiscall ");
                        break;
                    case SignatureCallingConvention.FastCall:
                        sb.Append("unmanaged fastcall ");
                        break;
                    case SignatureCallingConvention.Unmanaged:
                        sb.Append("unmanaged ");
                        break;
                    case SignatureCallingConvention.VarArgs:
                        sb.Append("vararg ");
                        break;
                    default:
                        // Calling convention is invalid (probably a combination of other bits).
                        // Treat it as default
                        break;
                }
            }

            static void serializeReturnOrParameter(
                EmitContext context,
                StringBuilder sb,
                ITypeReference type,
                ImmutableArray<ICustomModifier> typeCustomModifiers,
                bool byRef,
                ImmutableArray<ICustomModifier> refCustomModifiers)
            {
                // type
                //   : type '&'
                //   | type 'modreq' '(' className ')'
                //   | type 'modopt' '(' className ')'
                //   ... /* All the other type formats */
                //   ;

                // To serialize the return or parameter type correctly, we start serializing the type itself.
                // Then, all the modopts/reqs on the type.
                // Then, the ref specifier, if it exists.
                // Then, all the modopts/reqs on the ref.

                // This order may seem counter-intuitive: after all, we consider the ref to come before the type, and indeed when deserializing
                // metadata the order is ref custom modifiers, ref specifier, custom modifiers, type. However, there is no expansion of the grammar
                // that allows that order in textual format, and this is the order that ILAsm/ILDAsm print and understand.

                bool isAssemQual = false;
                AppendSerializedTypeName(sb, type, ref isAssemQual, context);
                serializeCustomModifiers(context, sb, typeCustomModifiers);

                Debug.Assert(refCustomModifiers.IsEmpty || byRef);
                if (byRef)
                {
                    sb.Append("& ");
                }

                serializeCustomModifiers(context, sb, refCustomModifiers);

                static void serializeCustomModifiers(EmitContext context, StringBuilder sb, ImmutableArray<ICustomModifier> customModifiers)
                {
                    foreach (var mod in customModifiers)
                    {
                        sb.Append(mod.IsOptional ? "modopt" : "modreq");
                        sb.Append("(");
                        bool isAssemQual1 = false;
                        var modType = mod.GetModifier(context);
                        AppendSerializedTypeName(sb, modType, ref isAssemQual1, context);
                        sb.Append(") ");
                    }
                }
            }
        }

        private static void AppendSerializedTypeName(StringBuilder sb, ITypeReference type, ref bool isAssemQualified, EmitContext context)
        {
            string argTypeName = GetSerializedTypeName(type, context, ref isAssemQualified);
            if (isAssemQualified)
            {
                sb.Append('[');
            }

            sb.Append(argTypeName);
            if (isAssemQualified)
            {
                sb.Append(']');
            }
        }

        private static void AppendAssemblyQualifierIfNecessary(StringBuilder sb, ITypeReference typeReference, out bool isAssemQualified, EmitContext context)
        {
            INestedTypeReference nestedType = typeReference.AsNestedTypeReference;
            if (nestedType != null)
            {
                AppendAssemblyQualifierIfNecessary(sb, nestedType.GetContainingType(context), out isAssemQualified, context);
                return;
            }

            IGenericTypeInstanceReference genInst = typeReference.AsGenericTypeInstanceReference;
            if (genInst != null)
            {
                AppendAssemblyQualifierIfNecessary(sb, genInst.GetGenericType(context), out isAssemQualified, context);
                return;
            }

            IArrayTypeReference arrType = typeReference as IArrayTypeReference;
            if (arrType != null)
            {
                AppendAssemblyQualifierIfNecessary(sb, arrType.GetElementType(context), out isAssemQualified, context);
                return;
            }

            IPointerTypeReference pointer = typeReference as IPointerTypeReference;
            if (pointer != null)
            {
                AppendAssemblyQualifierIfNecessary(sb, pointer.GetTargetType(context), out isAssemQualified, context);
                return;
            }

            isAssemQualified = false;
            IAssemblyReference referencedAssembly = null;
            INamespaceTypeReference namespaceType = typeReference.AsNamespaceTypeReference;
            if (namespaceType != null)
            {
                referencedAssembly = namespaceType.GetUnit(context) as IAssemblyReference;
            }

            if (referencedAssembly != null)
            {
                var containingAssembly = context.Module.GetContainingAssembly(context);

                if (containingAssembly == null || !ReferenceEquals(referencedAssembly, containingAssembly))
                {
                    sb.Append(", ");
                    sb.Append(MetadataWriter.StrongName(referencedAssembly));
                    isAssemQualified = true;
                }
            }
        }

        private static string GetMangledAndEscapedName(INamedTypeReference namedType)
        {
            var pooled = PooledStringBuilder.GetInstance();
            StringBuilder mangledName = pooled.Builder;

            const string needsEscaping = "\\[]*.+,& ";
            foreach (var ch in namedType.Name)
            {
                if (needsEscaping.IndexOf(ch) >= 0)
                {
                    mangledName.Append('\\');
                }

                mangledName.Append(ch);
            }

            if (namedType.MangleName && namedType.GenericParameterCount > 0)
            {
                mangledName.Append(MetadataHelpers.GetAritySuffix(namedType.GenericParameterCount));
            }

            return pooled.ToStringAndFree();
        }

        /// <summary>
        /// Strip off *, &amp;, and [].
        /// </summary>
        private static ITypeReference UnwrapTypeReference(ITypeReference typeReference, EmitContext context)
        {
            while (true)
            {
                IArrayTypeReference arrType = typeReference as IArrayTypeReference;
                if (arrType != null)
                {
                    typeReference = arrType.GetElementType(context);
                    continue;
                }

                IPointerTypeReference pointer = typeReference as IPointerTypeReference;
                if (pointer != null)
                {
                    typeReference = pointer.GetTargetType(context);
                    continue;
                }

                return typeReference;
            }
        }

        /// <summary>
        /// Qualified name of namespace.
        /// e.g. "A.B.C"
        /// </summary>
        internal static string BuildQualifiedNamespaceName(INamespace @namespace)
        {
            Debug.Assert(@namespace != null);

            if (@namespace.ContainingNamespace == null)
            {
                return @namespace.Name;
            }

            var namesReversed = ArrayBuilder<string>.GetInstance();
            do
            {
                string name = @namespace.Name;
                if (name.Length != 0)
                {
                    namesReversed.Add(name);
                }

                @namespace = @namespace.ContainingNamespace;
            }
            while (@namespace != null);

            var result = PooledStringBuilder.GetInstance();

            for (int i = namesReversed.Count - 1; i >= 0; i--)
            {
                result.Builder.Append(namesReversed[i]);

                if (i > 0)
                {
                    result.Builder.Append('.');
                }
            }

            namesReversed.Free();
            return result.ToStringAndFree();
        }
    }
}
