// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Microsoft.DiaSymReader
{
    [ComVisible(false)]
    // Buffer pointers must address the declared number of elements and remain fixed during a call.
    // Returned metadata pointers are borrowed from the importer's backing image and share its lifetime.
    [ComImport]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [Guid("7DAC8207-D3AE-4c75-9B67-92801A497D44")]
    internal interface IMetadataImport
    {
        [PreserveSig]
        unsafe void CloseEnum(void* enumHandle);

        [PreserveSig]
        unsafe int CountEnum(void* enumHandle, out int count);

        [PreserveSig]
        unsafe int ResetEnum(void* enumHandle, int position);

        [PreserveSig]
        unsafe int EnumTypeDefs(
            ref void* enumHandle,
            [Out] int* typeDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumInterfaceImpls(
            ref void* enumHandle,
            int typeDef,
            [Out] int* interfaceImpls,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumTypeRefs(
            ref void* enumHandle,
            [Out] int* typeRefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        int FindTypeDefByName(
            string name,
            int enclosingClass,
            out int typeDef); // must be specified

        [PreserveSig]
        unsafe int GetScopeProps(
            [Out] char* name,
            int bufferLength,
            [Out] int* nameLength,
            [Out] Guid* mvid);

        [PreserveSig]
        int GetModuleFromScope(
            out int moduleDef); // must be specified

        [PreserveSig]
        unsafe int GetTypeDefProps(
            int typeDef,
            [Out] char* qualifiedName,
            int qualifiedNameBufferLength,
            [Out] int* qualifiedNameLength,
            [Out] TypeAttributes* attributes,
            [Out] int* baseType);

        [PreserveSig]
        unsafe int GetInterfaceImplProps(
            int interfaceImpl,
            [Out] int* typeDef,
            [Out] int* interfaceDefRefSpec);

        [PreserveSig]
        unsafe int GetTypeRefProps(
            int typeRef,
            [Out] int* resolutionScope, // ModuleRef or AssemblyRef
            [Out] char* qualifiedName,
            int qualifiedNameBufferLength,
            [Out] int* qualifiedNameLength);

        /// <summary>
        /// Resolves type reference.
        /// </summary>
        /// <param name="typeRef">The TypeRef metadata token to return the referenced type information for.</param>
        /// <param name="scopeInterfaceId">The IID of the interface to return in scope. Typically, this would be IID_IMetaDataImport.</param>
        /// <param name="scope">An interface to the module scope in which the referenced type is defined.</param>
        /// <param name="typeDef">A pointer to a TypeDef token that represents the referenced type.</param>
        /// <remarks>
        /// TypeDefs define a type within a scope. TypeRefs refer to type-defs in other scopes
        /// and allow you to import a type from another scope. This function attempts to determine
        /// which type-def a type-ref points to.
        /// 
        /// This resolve (type-ref, this cope) --> (type-def=*ptd, other scope=*ppIScope)
        /// 
        /// However, this resolution requires knowing what modules have been loaded, which is not decided
        /// until runtime via loader / fusion policy. Thus this interface can't possibly be correct since
        /// it doesn't have that knowledge. Furthermore, when inspecting metadata from another process
        /// (such as a debugger inspecting the debuggee's metadata), this API can be truly misleading.
        /// 
        /// This API usage should be avoided.
        /// </remarks>
        [PreserveSig]
        int ResolveTypeRef(
            int typeRef,
            [In] ref Guid scopeInterfaceId,
            [MarshalAs(UnmanagedType.Interface)] out object scope, // must be specified
            out int typeDef); // must be specified

        [PreserveSig]
        unsafe int EnumMembers(
            ref void* enumHandle,
            int typeDef,
            [Out] int* memberDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumMembersWithName(
            ref void* enumHandle,
            int typeDef,
            string name,
            [Out] int* memberDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumMethods(
            ref void* enumHandle,
            int typeDef,
            [Out] int* methodDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumMethodsWithName(
            ref void* enumHandle,
            int typeDef,
            string name,
            [Out] int* methodDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumFields(
            ref void* enumHandle,
            int typeDef,
            [Out] int* fieldDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumFieldsWithName(
            ref void* enumHandle,
            int typeDef,
            string name,
            [Out] int* fieldDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumParams(
            ref void* enumHandle,
            int methodDef,
            [Out] int* paramDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumMemberRefs(
            ref void* enumHandle,
            int parentToken,
            [Out] int* memberRefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumMethodImpls(
            ref void* enumHandle,
            int typeDef,
            [Out] int* implementationTokens,
            [Out] int* declarationTokens,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumPermissionSets(
            ref void* enumHandle,
            int token, // TypeDef, MethodDef or Assembly
            uint action, // DeclarativeSecurityAction
            [Out] int* declSecurityTokens,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int FindMember(
            int typeDef,
            string name,
            [In] byte* signature,
            int signatureLength,
            out int memberDef);

        [PreserveSig]
        unsafe int FindMethod(
            int typeDef,
            string name,
            [In] byte* signature,
            int signatureLength,
            out int methodDef);

        [PreserveSig]
        unsafe int FindField(
            int typeDef,
            string name,
            [In] byte* signature,
            int signatureLength,
            out int fieldDef);

        [PreserveSig]
        unsafe int FindMemberRef(
            int typeDef,
            string name,
            [In] byte* signature,
            int signatureLength,
            out int memberRef);

        [PreserveSig]
        unsafe int GetMethodProps(
           int methodDef,
           [Out] int* declaringTypeDef,
           [Out] char* name,
           int nameBufferLength,
           [Out] int* nameLength,
           [Out] MethodAttributes* attributes,
           [Out] byte** signature, // returns pointer to signature blob
           [Out] int* signatureLength,
           [Out] int* relativeVirtualAddress,
           [Out] MethodImplAttributes* implAttributes);

        [PreserveSig]
        unsafe int GetMemberRefProps(
            int memberRef,
            [Out] int* declaringType, // TypeDef or TypeRef
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] byte** signature, // returns pointer to signature blob
            [Out] int* signatureLength);

        [PreserveSig]
        unsafe int EnumProperties(
           ref void* enumHandle,
           int typeDef,
           [Out] int* properties,
           int bufferLength,
           [Out] int* count);

        [PreserveSig]
        unsafe uint EnumEvents(
           ref void* enumHandle,
           int typeDef,
           [Out] int* events,
           int bufferLength,
           [Out] int* count);

        [PreserveSig]
        unsafe int GetEventProps(
            int @event,
            [Out] int* declaringTypeDef,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] int* attributes,
            [Out] int* eventType,
            [Out] int* adderMethodDef,
            [Out] int* removerMethodDef,
            [Out] int* raiserMethodDef,
            [Out] int* otherMethodDefs,
            int otherMethodDefBufferLength,
            [Out] int* methodMethodDefsLength);

        [PreserveSig]
        unsafe int EnumMethodSemantics(
            ref void* enumHandle,
            int methodDef,
            [Out] int* eventsAndProperties,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int GetMethodSemantics(
            int methodDef,
            int eventOrProperty,
            [Out] int* semantics);

        [PreserveSig]
        unsafe int GetClassLayout(
            int typeDef,
            [Out] int* packSize,  // 1, 2, 4, 8, or 16
            [Out] MetadataImportFieldOffset* fieldOffsets,
            int bufferLength,
            [Out] int* count,
            [Out] int* typeSize);

        [PreserveSig]
        unsafe int GetFieldMarshal(
            int fieldDef,
            [Out] byte** nativeTypeSignature, // returns pointer to signature blob
            [Out] int* nativeTypeSignatureLengvth);

        [PreserveSig]
        unsafe int GetRVA(
            int methodDef,
            [Out] int* relativeVirtualAddress,
            [Out] int* implAttributes);

        [PreserveSig]
        unsafe int GetPermissionSetProps(
            int declSecurity,
            [Out] uint* action,
            [Out] byte** permissionBlob, // returns pointer to permission blob
            [Out] int* permissionBlobLength);

        [PreserveSig]
        unsafe int GetSigFromToken(
            int standaloneSignature,
            [Out] byte** signature, // returns pointer to signature blob
            [Out] int* signatureLength);

        [PreserveSig]
        unsafe int GetModuleRefProps(
            int moduleRef,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength);

        [PreserveSig]
        unsafe int EnumModuleRefs(
            ref void* enumHandle,
            [Out] int* moduleRefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int GetTypeSpecFromToken(
            int typeSpec,
            [Out] byte** signature, // returns pointer to signature blob
            [Out] int* signatureLength);

        [PreserveSig]
        unsafe int GetNameFromToken(
            int token,
            [Out] byte* nameUtf8); // name on the #String heap

        [PreserveSig]
        unsafe int EnumUnresolvedMethods(
            ref void* enumHandle,
            [Out] int* methodDefs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int GetUserString(
            int userStringToken,
            [Out] char* buffer,
            int bufferLength,
            [Out] int* length);

        [PreserveSig]
        unsafe int GetPinvokeMap(
            int memberDef,  // FieldDef, MethodDef
            [Out] int* attributes,
            [Out] char* importName,
            int importNameBufferLength,
            [Out] int* importNameLength,
            [Out] int* moduleRef);

        [PreserveSig]
        unsafe int EnumSignatures(
            ref void* enumHandle,
            [Out] int* signatureTokens,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumTypeSpecs(
            ref void* enumHandle,
            [Out] int* typeSpecs,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int EnumUserStrings(
            ref void* enumHandle,
            [Out] int* userStrings,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        int GetParamForMethodIndex(
            int methodDef,
            int sequenceNumber,
            out int parameterToken); // must be specified

        [PreserveSig]
        unsafe int EnumCustomAttributes(
            ref void* enumHandle,
            int parent,
            int attributeType,
            [Out] int* customAttributes,
            int bufferLength,
            [Out] int* count);

        [PreserveSig]
        unsafe int GetCustomAttributeProps(
            int customAttribute,
            [Out] int* parent,
            [Out] int* constructor,  // MethodDef, MethodRef
            [Out] byte** value, // returns pointer to a value blob
            [Out] int* valueLength);

        [PreserveSig]
        int FindTypeRef(
            int resolutionScope,
            string name,
            out int typeRef); // must be specified

        [PreserveSig]
        unsafe int GetMemberProps(
            int member, // Field or Property
            [Out] int* declaringTypeDef,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] int* attributes,
            [Out] byte** signature, // returns pointer to signature blob 
            [Out] int* signatureLength,
            [Out] int* relativeVirtualAddress,
            [Out] int* implAttributes,
            [Out] int* constantType,
            [Out] byte** constantValue, // returns pointer to constant value blob
            [Out] int* constantValueLength);

        [PreserveSig]
        unsafe int GetFieldProps(
            int fieldDef,
            [Out] int* declaringTypeDef,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] int* attributes,
            [Out] byte** signature, // returns pointer to signature blob 
            [Out] int* signatureLength,
            [Out] int* constantType,
            [Out] byte** constantValue, // returns pointer to constant value blob
            [Out] int* constantValueLength);

        [PreserveSig]
        unsafe int GetPropertyProps(
            int propertyDef,
            [Out] int* declaringTypeDef,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] int* attributes,
            [Out] byte** signature, // returns pointer to signature blob 
            [Out] int* signatureLength,
            [Out] int* constantType,
            [Out] byte** constantValue, // returns pointer to constant value blob
            [Out] int* constantValueLength,
            [Out] int* setterMethodDef,
            [Out] int* getterMethodDef,
            [Out] int* outerMethodDefs,
            int outerMethodDefsBufferLength,
            [Out] int* otherMethodDefCount);

        [PreserveSig]
        unsafe int GetParamProps(
            int parameter,
            [Out] int* declaringMethodDef,
            [Out] int* sequenceNumber,
            [Out] char* name,
            int nameBufferLength,
            [Out] int* nameLength,
            [Out] int* attributes,
            [Out] int* constantType,
            [Out] byte** constantValue, // returns pointer to constant value blob
            [Out] int* constantValueLength);

        [PreserveSig]
        unsafe int GetCustomAttributeByName(
            int parent,
            string name,
            [Out] byte** value, // returns pointer to a value blob
            [Out] int* valueLength);

        [PreserveSig]
        bool IsValidToken(int token);

        [PreserveSig]
        int GetNestedClassProps(
            int nestedClass,
            out int enclosingClass);

        [PreserveSig]
        unsafe int GetNativeCallConvFromSig(
            [In] byte* signature,
            int signatureLength,
            [Out] int* callingConvention);

        [PreserveSig]
        int IsGlobal(
            int token,
            [Out] bool value);
    }
}
