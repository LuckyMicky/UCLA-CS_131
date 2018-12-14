import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State
{
	//	Private varibles and helper function
	private AtomicIntegerArray value;
	private byte maxval;
	private void copyOperator(byte[] v)
	{
		int len = v.length;
		value = new AtomicIntegerArray(len);
		for (int i = 0; i < len; i++)
			value.set(i, v[i]);
	}

	GetNSet(byte[] v)
	{
		copyOperator(v);
		maxval = 127;
	}
	GetNSet(byte[] v, byte m)
	{
		copyOperator(v);
		maxval = m;
	}

	public int size() {	return value.length(); }

	public byte[] current()
	{
		byte[] result = new byte[size()];
		for (int i = 0; i < size(); i++)
			result[i] = (byte) value.get(i);
		return result;
	}

	public boolean swap(int i, int j)
	{
		int ith = value.get(i);
		int jth = value.get(j);
		if (ith <=0 || jth >= maxval)
			return false;
		value.set(i, ith - 1);
		value.set(j, jth + 1);
		return true;
	}
}