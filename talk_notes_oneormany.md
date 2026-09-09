// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable enable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.Collections
{
    /// <summary>
    /// Represents a single item or many items (including none).
    /// </summary>
    /// <remarks>
    /// Used when a collection usually contains a single item but sometimes might contain multiple.
    /// </remarks>
    [DebuggerDisplay("{GetDebuggerDisplay(),nq}")]
    [DebuggerTypeProxy(typeof(OneOrMany<>.DebuggerProxy))]
    [Union]
    internal readonly struct OneOrMany<T> : OneOrMany<T>.IUnionMembers
    {
        public static readonly OneOrMany<T> Empty = new OneOrMany<T>(ImmutableArray<T>.Empty);

        private readonly T? _one;
        private readonly ImmutableArray<T> _many;

        public OneOrMany(T one)
        {
            _one = one;
            _many = default;
        }

        public OneOrMany(ImmutableArray<T> many)
        {
            if (many.IsDefault)
            {
                throw new ArgumentNullException(nameof(many));
            }

            if (many is [var item])
            {
                _one = item;
                _many = default;
            }
            else
            {
                _one = default;
                _many = many;
            }
        }

        public bool IsDefault
            => _one == null && _many.IsDefault;

        public T this[int index]
        {
            get
            {
                return this switch
                {
                    T when index != 0 => throw new InvalidOperationException(),
                    T one => one,
                    ImmutableArray<T> many => many[index]
                };
            }
        }

        public int Count
            => this switch { T => 1, var many => many.Count };

        public bool IsEmpty
            => Count == 0;

        object IUnionMembers.Value => _many.IsDefault ? _one : _many;
        bool IUnionMembers.TryGetValue([MaybeNullWhen(false)] out T one)
        {
            if (_many.IsDefault)
            {
                one = _one;
                return true;
            }

            one = default;
            return false;
        }

        bool IUnionMembers.TryGetValue(out ImmutableArray<T> many)
        {
            if (!_many.IsDefault)
            {
                many = _many;
                return true;
            }

            many = default;
            return false;
        }

        public OneOrMany<T> Add(T item)
            => this switch
            {
                T one => OneOrMany.Create(one, item),
                ImmutableArray<T> { Length: 0 } => item,
                ImmutableArray<T> many => many.Add(item)
            };

        public void AddRangeTo(ArrayBuilder<T> builder)
        {
            switch (this)
            {
                case T one:
                    builder.Add(one);
                    break;
                case ImmutableArray<T> many:
                    builder.AddRange(many);
                    break;
            }
        }

        public bool Contains(T item)
            => this switch
            {
                T one => EqualityComparer<T>.Default.Equals(item, one),
                ImmutableArray<T> many => many.Contains(item)
            };

        public OneOrMany<T> RemoveAll(T item)
        {
            return this switch
            {
                T one => EqualityComparer<T>.Default.Equals(item, _one) ? Empty : one,
                ImmutableArray<T> many => _many.WhereAsArray(static (value, item) => !EqualityComparer<T>.Default.Equals(value, item), item)
            };
        }

        public OneOrMany<TResult> Select<TResult>(Func<T, TResult> selector)
        {
            return this switch
            {
                T one => selector(one),
                ImmutableArray<T> many => many.SelectAsArray(selector)
            };
        }

        public OneOrMany<TResult> Select<TResult, TArg>(Func<T, TArg, TResult> selector, TArg arg)
        {
            return this switch
            {
                T one => selector(one, arg),
                ImmutableArray<T> many => many.SelectAsArray(selector, arg)
            };
        }

        public T First() => this[0];

        public T? FirstOrDefault()
            => this switch { T one => one, ImmutableArray<T> many => many.FirstOrDefault() };

        public T? FirstOrDefault(Func<T, bool> predicate)
        {
            return this switch
            {
                T one => predicate(one) ? one : default,
                ImmutableArray<T> many => many.FirstOrDefault(predicate)
            };
        }

        public T? FirstOrDefault<TArg>(Func<T, TArg, bool> predicate, TArg arg)
        {
            return this switch
            {
                T one => predicate(one, arg) ? one : default,
                ImmutableArray<T> many => many.FirstOrDefault(predicate, arg)
            };
        }

        public static OneOrMany<T> CastUp<TDerived>(OneOrMany<TDerived> from) where TDerived : class, T
        {
            return from switch
            {
                T one => one,
                ImmutableArray<TDerived> many => ImmutableArray<T>.CastUp(many)
            };
        }

        public bool All(Func<T, bool> predicate)
            => this switch { T one => predicate(one), ImmutableArray<T> many => many.All(predicate) };

        public bool All<TArg>(Func<T, TArg, bool> predicate, TArg arg)
            => this switch { T one => predicate(one, arg), ImmutableArray<T> many => many.All(predicate, arg) };

        public bool Any()
            => !IsEmpty;

        public bool Any(Func<T, bool> predicate)
            => this switch { T one => predicate(one), ImmutableArray<T> many => many.Any(predicate) };

        public bool Any<TArg>(Func<T, TArg, bool> predicate, TArg arg)
            => this switch { T one => predicate(one, arg), ImmutableArray<T> many => many.Any(predicate, arg) };

        public ImmutableArray<T> ToImmutable()
            => this switch { T one => [one], ImmutableArray<T> many => many };

        public T[] ToArray()
            => this switch { T one => [one], ImmutableArray<T> many => many.ToArray() };

        public bool SequenceEqual(OneOrMany<T> other, IEqualityComparer<T>? comparer = null)
        {
            comparer ??= EqualityComparer<T>.Default;

            return (this, other) switch
            {
                (T one, T otherOne) => comparer.Equals(one, otherOne),
                (ImmutableArray<T> many, ImmutableArray<T> otherMany) => many.Length == otherMany.Length && many.SequenceEqual(otherMany),
                _ => false
            };
        }

        public bool SequenceEqual(ImmutableArray<T> other, IEqualityComparer<T>? comparer = null)
            => SequenceEqual(OneOrMany.Create(other), comparer);

        public bool SequenceEqual(IEnumerable<T> other, IEqualityComparer<T>? comparer = null)
        {
            comparer ??= EqualityComparer<T>.Default;

            if (this is ImmutableArray<T> many)
            {
                return many.SequenceEqual(other, comparer);
            }

            var first = true;
            foreach (var otherItem in other)
            {
                if (!first || !comparer.Equals(_one, otherItem))
                {
                    return false;
                }

                first = false;
            }

            return true;
        }

        public Enumerator GetEnumerator()
            => new(this);

        internal struct Enumerator
        {
            private readonly OneOrMany<T> _collection;
            private int _index;

            internal Enumerator(OneOrMany<T> collection)
            {
                _collection = collection;
                _index = -1;
            }

            public bool MoveNext()
            {
                _index++;
                return _index < _collection.Count;
            }

            public T Current => _collection[_index];
        }

        private sealed class DebuggerProxy(OneOrMany<T> instance)
        {
            private readonly OneOrMany<T> _instance = instance;

            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            public T[] Items => _instance.ToArray();
        }

        private string GetDebuggerDisplay()
            => "Count = " + Count;

        public interface IUnionMembers
        {
            public static OneOrMany<T> Create(T one)
            {
                return OneOrMany.Create(one);
            }

            public static OneOrMany<T> Create(ImmutableArray<T> many)
            {
                return OneOrMany.Create(many);
            }

            public object Value { get; }
            public bool TryGetValue([MaybeNullWhen(false)] out T one);
            public bool TryGetValue(out ImmutableArray<T> many);
        }
    }

    internal static class OneOrMany
    {
        public static OneOrMany<T> Create<T>(T one)
            => new OneOrMany<T>(one);

        public static OneOrMany<T> Create<T>(T one, T two)
            => new OneOrMany<T>(ImmutableArray.Create(one, two));

        public static OneOrMany<T> OneOrNone<T>(T? one)
            => one is null ? OneOrMany<T>.Empty : new OneOrMany<T>(one);

        public static OneOrMany<T> Create<T>(ImmutableArray<T> many)
            => new OneOrMany<T>(many);

        public static bool SequenceEqual<T>(this ImmutableArray<T> array, OneOrMany<T> other, IEqualityComparer<T>? comparer = null)
            => Create(array).SequenceEqual(other, comparer);

        public static bool SequenceEqual<T>(this IEnumerable<T> array, OneOrMany<T> other, IEqualityComparer<T>? comparer = null)
            => other.SequenceEqual(array, comparer);
    }
}
