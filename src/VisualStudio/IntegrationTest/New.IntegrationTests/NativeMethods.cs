// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Microsoft.VisualStudio.IntegrationTest.Utilities.Interop;

internal static class NativeMethods
{
    private const string User32 = "User32.dll";

    #region user32.dll

    public const uint GA_PARENT = 1;
    public const uint GA_ROOT = 2;
    public const uint GA_ROOTOWNER = 3;

    public const uint GW_HWNDFIRST = 0;
    public const uint GW_HWNDLAST = 1;
    public const uint GW_HWNDNEXT = 2;
    public const uint GW_HWNDPREV = 3;
    public const uint GW_OWNER = 4;
    public const uint GW_CHILD = 5;
    public const uint GW_ENABLEDPOPUP = 6;

    public const int SM_CXSCREEN = 0;
    public const int SM_CYSCREEN = 1;

    public const uint WM_GETTEXT = 0x000D;
    public const uint WM_GETTEXTLENGTH = 0x000E;

    [StructLayout(LayoutKind.Sequential)]
    public struct POINT
    {
        public int x;
        public int y;
    }

    [UnmanagedFunctionPointer(CallingConvention.Winapi, SetLastError = false)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public delegate bool WNDENUMPROC(IntPtr hWnd, IntPtr lParam);

    [DllImport(User32, SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static safe extern bool EnumWindows([MarshalAs(UnmanagedType.FunctionPtr)] WNDENUMPROC lpEnumFunc, IntPtr lParam);

    [DllImport(User32)]
    public static safe extern IntPtr GetAncestor(IntPtr hWnd, uint gaFlags);

    [DllImport(User32, SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static safe extern bool GetCursorPos(out POINT point);

    public static System.Windows.Point GetCursorPos()
    {
        if (!GetCursorPos(out var point))
        {
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error());
        }

        return new System.Windows.Point(point.x, point.y);
    }

    [DllImport(User32, SetLastError = true)]
    public static safe extern IntPtr GetParent(IntPtr hWnd);

    [DllImport(User32, CharSet = CharSet.Unicode)]
    public static safe extern int GetSystemMetrics(int nIndex);

    [DllImport(User32, SetLastError = true)]
    public static safe extern IntPtr GetWindow(IntPtr hWnd, uint uCmd);

    [DllImport(User32, CharSet = CharSet.Unicode, SetLastError = true)]
    // Pointer-valued message arguments must remain valid until the receiving window finishes processing the message.
    public static unsafe extern IntPtr SendMessage(IntPtr hWnd, uint uMsg, IntPtr wParam, IntPtr lParam);

    [DllImport(User32, CharSet = CharSet.Unicode, SetLastError = true)]
    // The message must accept a writable string buffer, with wParam no larger than its capacity.
    public static unsafe extern IntPtr SendMessage(IntPtr hWnd, uint uMsg, IntPtr wParam, [Out, MarshalAs(UnmanagedType.LPWStr)] StringBuilder lParam);

    #endregion
}
