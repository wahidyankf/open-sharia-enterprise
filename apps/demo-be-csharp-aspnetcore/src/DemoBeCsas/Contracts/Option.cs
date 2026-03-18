// Minimal shim for Org.OpenAPITools.Client.Option<T> required by generated contract models.
// The OpenAPI Generator (csharp client generator) embeds Option<T> usage in every model's
// JsonConverter. This shim satisfies that dependency without pulling in the full client library.

#nullable enable

namespace Org.OpenAPITools.Client
{
    /// <summary>
    /// Represents an optional value that distinguishes "not set" from "set to null".
    /// Used by generated OpenAPI model JSON converters to detect missing vs. null fields.
    /// The implicit conversion to T allows generated property getters to return the wrapped
    /// value directly (e.g., <c>public string? Unit { get { return this.UnitOption; } }</c>).
    /// </summary>
    public struct Option<T>
    {
        /// <summary>
        /// Initializes a set Option with the specified value.
        /// </summary>
        public Option(T value)
        {
            Value = value;
            IsSet = true;
        }

        /// <summary>
        /// Gets whether this Option has been explicitly set.
        /// </summary>
        public bool IsSet { get; }

        /// <summary>
        /// Gets the value of this Option. Only meaningful when <see cref="IsSet"/> is true.
        /// </summary>
        public T Value { get; }

        /// <summary>
        /// Implicitly converts an Option to its underlying value.
        /// Enables generated property getters such as:
        ///   <c>public string? Unit { get { return this.UnitOption; } }</c>
        /// </summary>
        public static implicit operator T(Option<T> option) => option.Value;
    }
}
