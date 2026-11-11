// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Runtime.InteropServices;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Host;

internal sealed partial class TemporaryStorageService
{
    private sealed class DirectMemoryAccessStreamReader : TextReaderWithLength
    {
        private unsafe char* _position;
        private readonly unsafe char* _end;

        // The caller must keep length characters at src readable and fixed in memory for the reader's lifetime.
        public unsafe DirectMemoryAccessStreamReader(char* src, int length)
            : base(length)
        {
            unsafe
            {
                RoslynDebug.Assert(src != null);
                RoslynDebug.Assert(length >= 0);

                _position = src;
                _end = _position + length;
            }
        }

        public override int Peek()
        {
            unsafe
            {
                if (_position >= _end)
                {
                    return -1;
                }

                return *_position;
            }
        }

        public override int Read()
        {
            unsafe
            {
                if (_position >= _end)
                {
                    return -1;
                }

                return *_position++;
            }
        }

        public override int Read(char[] buffer, int index, int count)
        {
            unsafe
            {
                if (buffer == null)
                {
                    throw new ArgumentNullException(nameof(buffer));
                }

                if (index < 0 || index >= buffer.Length)
                {
                    throw new ArgumentOutOfRangeException(nameof(index));
                }

                if (count < 0 || (index + count) > buffer.Length)
                {
                    throw new ArgumentOutOfRangeException(nameof(count));
                }

                count = Math.Min(count, (int)(_end - _position));
                if (count > 0)
                {
                    Marshal.Copy((IntPtr)_position, buffer, index, count);
                    _position += count;
                }

                return count;
            }
        }
    }
}
