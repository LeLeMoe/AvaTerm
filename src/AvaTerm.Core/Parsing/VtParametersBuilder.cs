using System.Diagnostics;

namespace AvaTerm.Core.Parsing;

internal sealed class VtParametersBuilder
{
    private readonly int _maxValueDiv10;
    private readonly int _maxValueMod10;

    private readonly int?[] _valuesBuffer;
    private readonly Range[] _groupsBuffer;
    private int _valueCount;
    private int _groupCount;
    private bool _failed;

    public VtParametersBuilder(int maxValue, int maxValueCount)
    {
        Debug.Assert(maxValue >= 0);
        Debug.Assert(maxValueCount > 0);

        (_maxValueDiv10, _maxValueMod10) = Math.DivRem(maxValue, 10);
        _valuesBuffer = new int?[maxValueCount];
        _groupsBuffer = new Range[maxValueCount];
    }

    public bool TryAppend(byte input)
    {
        Debug.Assert(input is >= 0x30 and <= 0x3B);

        if (_failed)
        {
            return false;
        }

        if (_valueCount == 0)
        {
            _valuesBuffer[0] = null;
            _groupsBuffer[0] = 0..1;
            _valueCount = 1;
            _groupCount = 1;
        }

        switch (input)
        {
            case >= 0x30 and <= 0x39:
                ref var slot = ref _valuesBuffer[_valueCount - 1];
                var value = slot.GetValueOrDefault();
                var digit = input - 0x30;

                if (value > _maxValueDiv10 || (value == _maxValueDiv10 && digit > _maxValueMod10))
                {
                    _failed = true;
                    return false;
                }

                slot = value * 10 + digit;

                break;

            case 0x3A:
                if (_valueCount >= _valuesBuffer.Length)
                {
                    _failed = true;
                    return false;
                }

                _valuesBuffer[_valueCount] = null;
                _groupsBuffer[_groupCount - 1] = new Range(_groupsBuffer[_groupCount - 1].Start, _valueCount + 1);
                ++_valueCount;

                break;

            case 0x3B:
                if (_valueCount >= _valuesBuffer.Length)
                {
                    _failed = true;
                    return false;
                }

                _valuesBuffer[_valueCount] = null;
                _groupsBuffer[_groupCount] = new Range(_valueCount, _valueCount + 1);
                ++_valueCount;
                ++_groupCount;

                break;
        }

        return true;
    }

    public VtParameters Build()
    {
        Debug.Assert(!_failed);

        return new VtParameters(_valuesBuffer.AsSpan(0, _valueCount), _groupsBuffer.AsSpan(0, _groupCount));
    }

    public void Reset()
    {
        _valueCount = 0;
        _groupCount = 0;
        _failed = false;
    }
}