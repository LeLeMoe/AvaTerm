namespace AvaTerm.Core.Parsing;

public readonly ref struct VtParameters
{
    public ref struct Enumerator
    {
        private readonly VtParameters _parameters;
        private int _index;

        internal Enumerator(VtParameters parameters)
        {
            _parameters = parameters;
            _index = -1;
        }

        public readonly ReadOnlySpan<int?> Current => _parameters[_index];

        public bool MoveNext()
        {
            if (_index + 1 >= _parameters.Length)
            {
                return false;
            }

            _index++;
            return true;
        }
    }

    private readonly ReadOnlySpan<int?> _values;
    private readonly ReadOnlySpan<Range> _groups;

    internal VtParameters(ReadOnlySpan<int?> values, ReadOnlySpan<Range> groups)
    {
        _values = values;
        _groups = groups;
    }

    public int Length => _groups.Length;
    
    public bool IsEmpty => _groups.IsEmpty;

    public ReadOnlySpan<int?> this[int index] => _values[_groups[index]];

    public Enumerator GetEnumerator() => new(this);
}