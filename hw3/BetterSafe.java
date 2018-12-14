import java.util.concurrent.locks.ReentrantLock;
class BetterSafe implements State
{
	//	private
	private byte[] value;
	private byte maxval;
	//	lock
	//	Syntax copied from:
	//	http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/
        //           locks/ReentrantLock.html
	private final ReentrantLock lock = new ReentrantLock();

	BetterSafe(byte[] v) { value = v; maxval = 127; }
	BetterSafe(byte[] v, byte m) { value = v; maxval = m; }

	public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j)
    {
    	lock.lock();	// block until condition holds
    	if (value[i] <= 0 || value[j] >= maxval) 
    	{
    		lock.unlock();
    		return false;
    	}
    	value[i]--;
		value[j]++;
		lock.unlock();
		return true;
    }
}