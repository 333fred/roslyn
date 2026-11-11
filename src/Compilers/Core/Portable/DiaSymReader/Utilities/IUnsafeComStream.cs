// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;
using STATSTG = System.Runtime.InteropServices.ComTypes.STATSTG;

namespace Microsoft.DiaSymReader
{
    /// <summary>
    /// This is a re-definition of COM's IStream interface. The important change is that
    /// the Read and Write methods take an <see cref="IntPtr"/> instead of a byte[] to avoid the
    /// allocation cost when called from native code.
    /// </summary>
    // Buffers and optional result pointers must remain valid and fixed for the duration of a call.
    [Guid("0000000c-0000-0000-C000-000000000046"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown), ComImport]
    internal interface IUnsafeComStream
    {
        // ISequentialStream portion
        unsafe void Read(byte* pv, int cb, int* pcbRead);
        unsafe void Write(byte* pv, int cb, int* pcbWritten);

        // IStream portion
        unsafe void Seek(long dlibMove, int dwOrigin, long* plibNewPosition);
        void SetSize(long libNewSize);
        unsafe void CopyTo(IStream pstm, long cb, int* pcbRead, int* pcbWritten);
        void Commit(int grfCommitFlags);
        void Revert();
        void LockRegion(long libOffset, long cb, int dwLockType);
        void UnlockRegion(long libOffset, long cb, int dwLockType);
        void Stat(out STATSTG pstatstg, int grfStatFlag);
        void Clone(out IStream ppstm);
    }
}
