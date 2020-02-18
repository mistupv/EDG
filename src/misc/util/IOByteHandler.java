package misc.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteOrder;
import java.util.Arrays;


public class IOByteHandler
{
	private static final char[] binaryArray = "01".toCharArray();
	private static final char[] octalArray = "012345678".toCharArray();
	private static final char[] hexArray = "0123456789ABCDEF".toCharArray();
	private static final ByteOrder defaultByteOrder = ByteOrder.LITTLE_ENDIAN;



	private byte[] byteArray = new byte[0];


	public byte[] getBytes()
	{ return byteArray; }
	public void setBytes(final byte[] byteArray)
	{ this.byteArray = byteArray; }
	public int length()
	{ return byteArray.length; }

	public void resize(final int newLength)
	{
		byteArray = Arrays.copyOf(byteArray, newLength);
	}
	public void shiftBytes(final int byteIndexFrom, final int byteIndexTo, final int bytesToShift)
	{
		copyBytes(byteIndexFrom, byteIndexTo, bytesToShift);
		write(new byte[bytesToShift], byteIndexFrom);
	}
	public void shiftBits(final int byteIndexFrom, final int bitIndexFrom, final int byteIndexTo, final int bitIndexTo, final int bitsToShift)
	{
		copyBits(byteIndexFrom, bitIndexFrom, byteIndexTo, bitIndexTo, bitsToShift);
		writeBits(new boolean[bitsToShift], byteIndexFrom, bitIndexFrom);
	}
	public void copyBytes(final int byteIndexFrom, final int byteIndexTo, final int bytesToShift)
	{
		for (int i = byteIndexTo - 1; i >= byteIndexFrom; i--)
			write(readByte(i), i + bytesToShift);
	}
	public void copyBits(final int byteIndexFrom, final int bitIndexFrom, final int byteIndexTo, final int bitIndexTo, final int bitsToShift)
	{
		if (byteIndexFrom > byteIndexTo || byteIndexFrom == byteIndexTo && bitIndexFrom >= bitIndexTo)
			return;

		if (byteIndexFrom == byteIndexTo)
			writeBits(readBits(byteIndexFrom, bitIndexFrom, bitIndexTo), byteIndexTo + (bitIndexFrom + bitsToShift)/8, (bitIndexFrom + bitsToShift)%8);

		writeBits(readBits(byteIndexTo, 0, bitIndexTo), byteIndexTo + (bitsToShift)/8, (bitsToShift)%8);
		for (int i = byteIndexTo - 1; i > byteIndexFrom; i--)
			writeBits(readBits(i, 8), i + (bitsToShift)/8, (bitsToShift)%8);
		writeBits(readBits(byteIndexFrom, bitIndexFrom, 8 - bitIndexFrom), byteIndexFrom + (bitIndexFrom + bitsToShift)/8, (bitIndexFrom + bitsToShift)%8);
	}

	public void load(final InputStream inputStream) throws IOException
	{
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		for (int b; (b = inputStream.read()) != -1; )
			baos.write(b);

		byteArray = baos.toByteArray();
	}
	public void save(final OutputStream outputStream) throws IOException
	{
		outputStream.write(byteArray);
	}

	public boolean readBit(final int byteIndex)
	{
		return readBit(byteIndex, 0);
	}
	public boolean readBit(final int byteIndex, final int bitIndex)
	{
		return (byteArray[byteIndex] >>> bitIndex & 0x01) != 0;
	}
	public boolean[] readBits()
	{
		return readBits(0, 0, 8*byteArray.length, defaultByteOrder);
	}
	public boolean[] readBits(final int byteIndex, final int length)
	{
		return readBits(byteIndex, 0, length, defaultByteOrder);
	}
	public boolean[] readBits(final int byteIndex, final int bitIndex, final int length)
	{
		return readBits(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public boolean[] readBits(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final boolean[] result = new boolean[length];

		if (byteOrder == ByteOrder.BIG_ENDIAN)
		{
			final int lastByteIndex = byteIndex + (bitIndex + length - 1)/8;
			final int lastByteLastBitIndex = (bitIndex + length)%8;
			for (int i = 0, j = lastByteIndex, k = j == byteIndex ? bitIndex : 0; i < length; i++, k++)
			{
				if (j == lastByteIndex && k == lastByteLastBitIndex)
				{
					j--;
					k = j == byteIndex ? bitIndex : 0;
				}
				if (k == 8)
				{
					j--;
					k = j == byteIndex ? bitIndex : 0;
				}
				result[i] = readBit(j, k);
			}
		}
		else
			for (int i = 0, j = byteIndex, k = bitIndex; i < length; i++, k++)
			{
				if (k == 8)
				{
					j++;
					k = 0;
				}
				result[i] = readBit(j, k);
			}

		return result;
	}
	public byte readByte(final int byteIndex)
	{
		return byteArray[byteIndex];
	}
	public byte readByte(final int byteIndex, final int bitIndex)
	{
		return readByte(byteIndex, bitIndex, defaultByteOrder);
	}
	public byte readByte(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final int a = byteArray[byteIndex] & 0xFF;

		if (bitIndex == 0)
			return (byte) a;
		else
		{
			int b = byteArray[byteIndex + 1] & 0xFF;

			return (byte) (byteOrder == ByteOrder.BIG_ENDIAN ? (a & 0xFF << bitIndex) << 8 | b & 0xFF >>> 8 - bitIndex
															 : b << 8 - bitIndex | a >>> bitIndex);
		}
	}
	public byte[] readBytes()
	{
		return byteArray;
	}
	public byte[] readBytes(final int byteIndex, final int length)
	{
		return readBytes(byteIndex, 0, length, defaultByteOrder);
	}
	public byte[] readBytes(final int byteIndex, final int bitIndex, final int length)
	{
		return readBytes(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public byte[] readBytes(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final byte[] result = new byte[length];

		for (int i = 0; i < length; i++)
			result[i] = readByte(byteIndex + i, bitIndex, byteOrder);

		return result;
	}
	public boolean readBool(final int byteIndex)
	{
		return byteArray[byteIndex] != 0;
	}
	public boolean readBool(final int byteIndex, final int bitIndex)
	{
		return readBool(byteIndex, bitIndex, defaultByteOrder);
	}
	public boolean readBool(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		return readByte(byteIndex, bitIndex, byteOrder) != 0;
	}
	public boolean[] readBools()
	{
		return readBools(0, 0, byteArray.length, defaultByteOrder);
	}
	public boolean[] readBools(final int byteIndex, final int length)
	{
		return readBools(byteIndex, 0, length, defaultByteOrder);
	}
	public boolean[] readBools(final int byteIndex, final int bitIndex, final int length)
	{
		return readBools(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public boolean[] readBools(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final boolean[] result = new boolean[length];

		for (int i = 0; i < length; i++)
			result[i] = readBool(byteIndex + i, bitIndex, byteOrder);

		return result;
	}
	public char readChar(final int byteIndex)
	{
		return (char) byteArray[byteIndex];
	}
	public char readChar(final int byteIndex, final int bitIndex)
	{
		return readChar(byteIndex, bitIndex, defaultByteOrder);
	}
	public char readChar(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		return (char) readByte(byteIndex, bitIndex, byteOrder);
	}
	public char[] readChars()
	{
		return readChars(0, 0, byteArray.length, defaultByteOrder);
	}
	public char[] readChars(final int byteIndex, final int length)
	{
		return readChars(byteIndex, 0, length, defaultByteOrder);
	}
	public char[] readChars(final int byteIndex, final int bitIndex, final int length)
	{
		return readChars(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public char[] readChars(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final char[] result = new char[length];

		for (int i = 0; i < length; i++)
			result[i] = readChar(byteIndex + i, bitIndex, byteOrder);

		return result;
	}
	public short readShort(final int byteIndex)
	{
		return readShort(byteIndex, 0, defaultByteOrder);
	}
	public short readShort(final int byteIndex, final int bitIndex)
	{
		return readShort(byteIndex, bitIndex, defaultByteOrder);
	}
	public short readShort(final int byteIndex, final ByteOrder byteOrder)
	{
		return readShort(byteIndex, 0, byteOrder);
	}
	public short readShort(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		int a = byteArray[byteIndex    ] & 0xFF;
		int b = byteArray[byteIndex + 1] & 0xFF;

		if (bitIndex == 0)
			return (short) (byteOrder == ByteOrder.BIG_ENDIAN ? a << 8 | b
															  : b << 8 | a);
		else
		{
			int c = byteArray[byteIndex + 2] & 0xFF;

			return (short) (byteOrder == ByteOrder.BIG_ENDIAN ? (a & 0xFF << bitIndex) << 8 | b << bitIndex | c & 0xFF >>> 8 - bitIndex
															  : c << 16 - bitIndex | b << 8 - bitIndex | a >>> bitIndex);
		}
	}
	public short[] readShorts()
	{
		return readShorts(0, 0, byteArray.length / 2, defaultByteOrder);
	}
	public short[] readShorts(final ByteOrder byteOrder)
	{
		return readShorts(0, 0, byteArray.length / 2, byteOrder);
	}
	public short[] readShorts(final int byteIndex, final int length)
	{
		return readShorts(byteIndex, 0, length, defaultByteOrder);
	}
	public short[] readShorts(final int byteIndex, final int bitIndex, final int length)
	{
		return readShorts(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public short[] readShorts(final int byteIndex, final int length, final ByteOrder byteOrder)
	{
		return readShorts(byteIndex, 0, length, byteOrder);
	}
	public short[] readShorts(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		short[] result = new short[length];

		for (int i = 0; i < length; i++)
			result[i] = readShort(byteIndex + 2 * i, bitIndex, byteOrder);

		return result;
	}
	public int readInt(final int byteIndex)
	{
		return readInt(byteIndex, 0, defaultByteOrder);
	}
	public int readInt(final int byteIndex, final int bitIndex)
	{
		return readInt(byteIndex, bitIndex, defaultByteOrder);
	}
	public int readInt(final int byteIndex, final ByteOrder byteOrder)
	{
		return readInt(byteIndex, 0, byteOrder);
	}
	public int readInt(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		int a = byteArray[byteIndex    ] & 0xFF;
		int b = byteArray[byteIndex + 1] & 0xFF;
		int c = byteArray[byteIndex + 2] & 0xFF;
		int d = byteArray[byteIndex + 3] & 0xFF;

		if (bitIndex == 0)
			return (byteOrder == ByteOrder.BIG_ENDIAN ? a << 24 | b << 16 | c << 8 | d
													  : d << 24 | c << 16 | b << 8 | a);
		else
		{
			int e = byteArray[byteIndex + 4] & 0xFF;

			return (byteOrder == ByteOrder.BIG_ENDIAN ? (a & 0xFF << bitIndex) << 24 | b << 16 + bitIndex | c << 8 + bitIndex | d << bitIndex | e & 0xFF >>> 8 - bitIndex
													  : e << 32 - bitIndex | d << 24 - bitIndex | c << 16 - bitIndex | b << 8 - bitIndex | a >>> bitIndex);
		}
	}
	public int[] readInts()
	{
		return readInts(0, 0, byteArray.length / 4, defaultByteOrder);
	}
	public int[] readInts(final ByteOrder byteOrder)
	{
		return readInts(0, 0, byteArray.length / 4, byteOrder);
	}
	public int[] readInts(final int byteIndex, final int length)
	{
		return readInts(byteIndex, 0, length, defaultByteOrder);
	}
	public int[] readInts(final int byteIndex, final int bitIndex, final int length)
	{
		return readInts(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public int[] readInts(final int byteIndex, final int length, final ByteOrder byteOrder)
	{
		return readInts(byteIndex, 0, length, byteOrder);
	}
	public int[] readInts(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		int[] result = new int[length];

		for (int i = 0; i < length; i++)
			result[i] = readInt(byteIndex + 4 * i, bitIndex, byteOrder);

		return result;
	}
	public long readLong(final int byteIndex)
	{
		return readLong(byteIndex, 0, defaultByteOrder);
	}
	public long readLong(final int byteIndex, final int bitIndex)
	{
		return readLong(byteIndex, bitIndex, defaultByteOrder);
	}
	public long readLong(final int byteIndex, final ByteOrder byteOrder)
	{
		return readLong(byteIndex, 0, byteOrder);
	}
	public long readLong(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		int a = byteArray[byteIndex    ] & 0xFF;
		int b = byteArray[byteIndex + 1] & 0xFF;
		int c = byteArray[byteIndex + 2] & 0xFF;
		int d = byteArray[byteIndex + 3] & 0xFF;
		int e = byteArray[byteIndex + 4] & 0xFF;
		int f = byteArray[byteIndex + 5] & 0xFF;
		int g = byteArray[byteIndex + 6] & 0xFF;
		int h = byteArray[byteIndex + 7] & 0xFF;

		if (bitIndex == 0)
			return byteOrder == ByteOrder.BIG_ENDIAN ? a << 56 | b << 48 | c << 40 | d << 32 | e << 24 | f << 16 | g << 8 | h
															 : h << 56 | g << 48 | f << 40 | e << 32 | d << 24 | c << 16 | b << 8 | a;
		else
		{
			int i = byteArray[byteIndex + 8];

			return byteOrder == ByteOrder.BIG_ENDIAN ? (a & 0xFF << bitIndex) << 56 | b << 48 + bitIndex | c << 40 + bitIndex | d << 32 + bitIndex | e << 24 + bitIndex | f << 16 + bitIndex | g << 8 + bitIndex | h << bitIndex | i & 0xFF >>> 8 - bitIndex
															 : i << 64 - bitIndex | h << 56 - bitIndex | g << 48 - bitIndex | f << 40 - bitIndex | e << 32 - bitIndex | d << 24 - bitIndex | c << 16 - bitIndex | b << 8 - bitIndex | a >>> bitIndex;
		}
	}
	public long[] readLongs()
	{
		return readLongs(0, 0, byteArray.length / 8, defaultByteOrder);
	}
	public long[] readLongs(final ByteOrder byteOrder)
	{
		return readLongs(0, 0, byteArray.length / 8, byteOrder);
	}
	public long[] readLongs(final int byteIndex, final int length)
	{
		return readLongs(byteIndex, 0, length, defaultByteOrder);
	}
	public long[] readLongs(final int byteIndex, final int bitIndex, final int length)
	{
		return readLongs(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public long[] readLongs(final int byteIndex, final int length, final ByteOrder byteOrder)
	{
		return readLongs(byteIndex, 0, length, byteOrder);
	}
	public long[] readLongs(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		long[] result = new long[length];

		for (int i = 0; i < length; i++)
			result[i] = readLong(byteIndex + 8 * i, byteOrder);

		return result;
	}
	public float readFloat(final int byteIndex)
	{
		return readFloat(byteIndex, 0, defaultByteOrder);
	}
	public float readFloat(final int byteIndex, final int bitIndex)
	{
		return readFloat(byteIndex, bitIndex, defaultByteOrder);
	}
	public float readFloat(final int byteIndex, final ByteOrder byteOrder)
	{
		return readFloat(byteIndex, 0, byteOrder);
	}
	public float readFloat(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final int asInt = readInt(byteIndex, bitIndex, byteOrder);

		return Float.intBitsToFloat(asInt);
	}
	public float[] readFloats()
	{
		return readFloats(0, 0, byteArray.length / 4, defaultByteOrder);
	}
	public float[] readFloats(final ByteOrder byteOrder)
	{
		return readFloats(0, 0, byteArray.length / 4, byteOrder);
	}
	public float[] readFloats(final int byteIndex, final int length)
	{
		return readFloats(byteIndex, 0, length, defaultByteOrder);
	}
	public float[] readFloats(final int byteIndex, final int bitIndex, final int length)
	{
		return readFloats(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public float[] readFloats(final int byteIndex, final int length, final ByteOrder byteOrder)
	{
		return readFloats(byteIndex, 0, length, defaultByteOrder);
	}
	public float[] readFloats(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		float[] result = new float[length];

		for (int i = 0; i < length; i++)
			result[i] = readFloat(byteIndex + 4 * i, bitIndex, byteOrder);

		return result;
	}
	public double readDouble(final int byteIndex)
	{
		return readDouble(byteIndex, 0, defaultByteOrder);
	}
	public double readDouble(final int byteIndex, final int bitIndex)
	{
		return readDouble(byteIndex, bitIndex, defaultByteOrder);
	}
	public double readDouble(final int byteIndex, final ByteOrder byteOrder)
	{
		return readDouble(byteIndex, 0, byteOrder);
	}
	public double readDouble(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final long asLong = readLong(byteIndex, bitIndex, byteOrder);

		return Double.longBitsToDouble(asLong);
	}
	public double[] readDoubles()
	{
		return readDoubles(0, 0, byteArray.length / 8, defaultByteOrder);
	}
	public double[] readDoubles(final ByteOrder byteOrder)
	{
		return readDoubles(0, 0, byteArray.length / 4, byteOrder);
	}
	public double[] readDoubles(final int byteIndex, final int length)
	{
		return readDoubles(byteIndex, 0, length, defaultByteOrder);
	}
	public double[] readDoubles(final int byteIndex, final int bitIndex, final int length)
	{
		return readDoubles(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public double[] readDoubles(final int byteIndex, final int length, final ByteOrder byteOrder)
	{
		return readDoubles(byteIndex, 0, length, byteOrder);
	}
	public double[] readDoubles(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		double[] result = new double[length];

		for (int i = 0; i < length; i++)
			result[i] = readDouble(byteIndex + 8 * i, bitIndex, byteOrder);

		return result;
	}
	public String readString()
	{
		return new String(byteArray);
	}
	public String readString(final int byteIndex, final int length)
	{
		return readString(byteIndex, 0, length, defaultByteOrder);
	}
	public String readString(final int byteIndex, final int bitIndex, final int length)
	{
		return readString(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public String readString(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		StringBuilder stringBuilder = new StringBuilder();

		for (int i = 0; i < length; i++)
			stringBuilder.append(readChar(byteIndex + i, bitIndex, byteOrder));

		return stringBuilder.toString();
	}
	public String readBinary(final int byteIndex)
	{
		return readBinary(byteIndex, 0, defaultByteOrder);
	}
	public String readBinary(final int byteIndex, final int bitIndex)
	{
		return readBinary(byteIndex, 0, defaultByteOrder);
	}
	public String readBinary(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final StringBuilder stringBuilder = new StringBuilder();
		final byte v = readByte(byteIndex, bitIndex, byteOrder);

		stringBuilder.append(binaryArray[(v & 0x80) >>> 7]);
		stringBuilder.append(binaryArray[(v & 0x40) >>> 6]);
		stringBuilder.append(binaryArray[(v & 0x20) >>> 5]);
		stringBuilder.append(binaryArray[(v & 0x10) >>> 4]);
		stringBuilder.append(binaryArray[(v & 0x08) >>> 3]);
		stringBuilder.append(binaryArray[(v & 0x04) >>> 2]);
		stringBuilder.append(binaryArray[(v & 0x02) >>> 1]);
		stringBuilder.append(binaryArray[ v & 0x01]);

		return stringBuilder.toString();
	}
	public String[] readBinaries()
	{
		return readBinaries(0, 0, byteArray.length, defaultByteOrder);
	}
	public String[] readBinaries(final int byteIndex, final int length)
	{
		return readBinaries(byteIndex, 0, length, defaultByteOrder);
	}
	public String[] readBinaries(final int byteIndex, final int bitIndex, final int length)
	{
		return readBinaries(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public String[] readBinaries(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final String[] result = new String[length];

		for (int i = 0; i < length; i++)
			result[i] = readBinary(byteIndex + i, bitIndex, byteOrder);

		return result;
	}
	public String readOctal(final int byteIndex)
	{
		return readOctal(byteIndex, 0, defaultByteOrder);
	}
	public String readOctal(final int byteIndex, final int bitIndex)
	{
		return readOctal(byteIndex, bitIndex, defaultByteOrder);
	}
	public String readOctal(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final StringBuilder stringBuilder = new StringBuilder();
		final byte v = readByte(byteIndex, bitIndex, byteOrder);

		stringBuilder.append(octalArray[(v & 0xC0) >>> 6]);
		stringBuilder.append(octalArray[(v & 0x38) >>> 3]);
		stringBuilder.append(octalArray[ v & 0x07]);

		return stringBuilder.toString();
	}
	public String[] readOctals()
	{
		return readOctals(0, 0, byteArray.length, defaultByteOrder);
	}
	public String[] readOctals(final int byteIndex, final int length)
	{
		return readOctals(byteIndex, 0, length, defaultByteOrder);
	}
	public String[] readOctals(final int byteIndex, final int bitIndex, final int length)
	{
		return readOctals(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public String[] readOctals(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final String[] result = new String[length];

		for (int i = 0; i < length; i++)
			result[i] = readOctal(byteIndex + i, bitIndex, byteOrder);

	    return result;
	}
	public String readHex(final int byteIndex)
	{
		return readHex(byteIndex, 0, defaultByteOrder);
	}
	public String readHex(final int byteIndex, final int bitIndex)
	{
		return readHex(byteIndex, bitIndex, defaultByteOrder);
	}
	public String readHex(final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final StringBuilder stringBuilder = new StringBuilder();
		final byte v = readByte(byteIndex, bitIndex, byteOrder);

		stringBuilder.append(hexArray[(v & 0xF0) >>> 4]);
		stringBuilder.append(hexArray[ v & 0x0F]);

		return stringBuilder.toString();
	}
	public String[] readHexs()
	{
		return readHexs(0, 0, byteArray.length, defaultByteOrder);
	}
	public String[] readHexs(final int byteIndex, final int length)
	{
		return readHexs(byteIndex, 0, length, defaultByteOrder);
	}
	public String[] readHexs(final int byteIndex, final int bitIndex, final int length)
	{
		return readHexs(byteIndex, bitIndex, length, defaultByteOrder);
	}
	public String[] readHexs(final int byteIndex, final int bitIndex, final int length, final ByteOrder byteOrder)
	{
		final String[] result = new String[length];

		for (int i = 0; i < length; i++)
			result[i] = readHex(byteIndex + i, bitIndex, byteOrder);

	    return result;
	}

	public void writeBit(final boolean data, final int byteIndex)
	{
		writeBit(data, byteIndex, 0);
	}
	public void writeBit(final boolean data, final int byteIndex, final int bitIndex)
	{
		ensureCapacity(byteIndex + 1);

		final byte a = byteArray[byteIndex];
		final byte mask = (byte) (0xFF ^ 0x01 << bitIndex);

		byteArray[byteIndex] = (byte) (a & mask | (data ? 0x01 : 0x00) << bitIndex);
	}
	public void writeBits(final boolean[] data, final int byteIndex)
	{
		writeBits(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeBits(final boolean[] data, final int byteIndex, final int bitIndex)
	{
		writeBits(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeBits(final boolean[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final int lastByteIndex = byteIndex + (bitIndex + data.length - 1)/8;
		ensureCapacity(lastByteIndex);

		if (byteOrder == ByteOrder.BIG_ENDIAN)
		{
			final int lastByteLastBitIndex = (bitIndex + data.length)%8;
			for (int i = 0, j = lastByteIndex, k = j == byteIndex ? bitIndex : 0; i < data.length; i++, k++)
			{
				if (j == lastByteIndex && k == lastByteLastBitIndex)
				{
					j--;
					k = j == byteIndex ? bitIndex : 0;
				}
				if (k == 8)
				{
					j--;
					k = j == byteIndex ? bitIndex : 0;
				}
				writeBit(data[i], j, k);
			}
		}
		else
			for (int i = 0, j = byteIndex, k = bitIndex; i < data.length; i++, k++)
			{
				if (k == 8)
				{
					j++;
					k = 0;
				}
				writeBit(data[i], j, k);
			}
	}
	public void write(final byte data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final byte data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final byte data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
		{
			ensureCapacity(byteIndex + 1);

			byteArray[byteIndex] = data;
		}
		else
		{
			ensureCapacity(byteIndex + 2);

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = (byte) (data & 0xFF << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (byteArray[byteIndex + 1] & 0xFF << bitIndex | data & 0xFF >>> 8 - bitIndex);
			}
			else
			{
				byteArray[byteIndex    ] = (byte) (data << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (byteArray[byteIndex + 1] & 0xFF << bitIndex | (data & 0xFF) >>> 8 - bitIndex);
			}
		}
	}
	public void write(final byte[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final byte[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final byte[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + i, bitIndex, byteOrder);
	}
	public void write(final boolean data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final boolean data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final boolean data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((byte) (data ? 1 : 0), byteIndex, bitIndex, byteOrder);
	}
	public void write(final boolean[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final boolean[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final boolean[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + i, bitIndex, byteOrder);
	}
	public void write(final short data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final short data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final short data, final int byteIndex, final ByteOrder byteOrder)
	{
		write(data, byteIndex, 0, byteOrder);
	}
	public void write(final short data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
		{
			ensureCapacity(byteIndex + 2);

			final byte a = (byte) ((data & 0xFF00)  >>> 8);
			final byte b = (byte) ( data & 0x00FF        );

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = a;
				byteArray[byteIndex + 1] = b;
			}
			else
			{
				byteArray[byteIndex    ] = b;
				byteArray[byteIndex + 1] = a;
			}
		}
		else
		{
			ensureCapacity(byteIndex + 3);

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = (byte) (data >>> 8 & 0xFF << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (data >>> bitIndex);
				byteArray[byteIndex + 2] = (byte) (byteArray[byteIndex + 2] & 0xFF << bitIndex | data & 0xFF >>> 8 - bitIndex);
			}
			else
			{
				byteArray[byteIndex    ] = (byte) (data << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (data >>> 8 - bitIndex);
				byteArray[byteIndex + 2] = (byte) (byteArray[byteIndex + 2] & 0xFF << bitIndex | data >>> (16 - bitIndex) & 0xFF >>> 8 - bitIndex);
			}
		}
	}
	public void write(final short[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final short[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final short[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + 2 * data.length);
		else
			ensureCapacity(byteIndex + 2 * data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + 2 * i, bitIndex, byteOrder);
	}
	public void write(final int data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final int data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final int data, final int byteIndex, final ByteOrder byteOrder)
	{
		write(data, byteIndex, 0, byteOrder);
	}
	public void write(final int data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
		{
			ensureCapacity(byteIndex + 4);

			final byte a = (byte) ((data & 0xFF000000) >>> 24);
			final byte b = (byte) ((data & 0x00FF0000) >>> 16);
			final byte c = (byte) ((data & 0x0000FF00) >>>  8);
			final byte d = (byte) ( data & 0x000000FF        );

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = a;
				byteArray[byteIndex + 1] = b;
				byteArray[byteIndex + 2] = c;
				byteArray[byteIndex + 3] = d;
			}
			else
			{
				byteArray[byteIndex    ] = d;
				byteArray[byteIndex + 1] = c;
				byteArray[byteIndex + 2] = b;
				byteArray[byteIndex + 3] = a;
			}
		}
		else
		{
			ensureCapacity(byteIndex + 5);

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = (byte) (data >>> 24 & 0xFF << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (data >>> (16 + bitIndex));
				byteArray[byteIndex + 2] = (byte) (data >>> (8  + bitIndex));
				byteArray[byteIndex + 3] = (byte) (data >>> bitIndex);
				byteArray[byteIndex + 4] = (byte) (byteArray[byteIndex + 4] & 0xFF << bitIndex | data & 0xFF >>> 8 - bitIndex);
			}
			else
			{
				byteArray[byteIndex    ] = (byte) (data << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (data >>> (8  - bitIndex));
				byteArray[byteIndex + 2] = (byte) (data >>> (16 - bitIndex));
				byteArray[byteIndex + 3] = (byte) (data >>> (24 - bitIndex));
				byteArray[byteIndex + 4] = (byte) (byteArray[byteIndex + 4] & 0xFF << bitIndex | data >>> (32 - bitIndex)  & 0xFF >>> 8 - bitIndex);
			}
		}
	}
	public void write(final int[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final int[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final int[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + 4 * data.length);
		else
			ensureCapacity(byteIndex + 4 * data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + 4 * i, bitIndex, byteOrder);
	}
	public void write(final long data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final long data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final long data, final int byteIndex, final ByteOrder byteOrder)
	{
		write(data, byteIndex, 0, byteOrder);
	}
	public void write(final long data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		final int high = (int) (data >>> 32);
		final int low  = (int) (data       );

		if (bitIndex == 0)
		{
			ensureCapacity(byteIndex + 8);

			final byte a = (byte) ((high & 0xFF000000) >>> 24);
			final byte b = (byte) ((high & 0x00FF0000) >>> 16);
			final byte c = (byte) ((high & 0x0000FF00) >>>  8);
			final byte d = (byte) ( high & 0x000000FF        );
			final byte e = (byte) ((low  & 0xFF000000) >>> 24);
			final byte f = (byte) ((low  & 0x00FF0000) >>> 16);
			final byte g = (byte) ((low  & 0x0000FF00) >>>  8);
			final byte h = (byte) ( low  & 0x000000FF       );

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = a;
				byteArray[byteIndex + 1] = b;
				byteArray[byteIndex + 2] = c;
				byteArray[byteIndex + 3] = d;
				byteArray[byteIndex + 4] = e;
				byteArray[byteIndex + 5] = f;
				byteArray[byteIndex + 6] = g;
				byteArray[byteIndex + 7] = h;
			}
			else
			{
				byteArray[byteIndex    ] = h;
				byteArray[byteIndex + 1] = g;
				byteArray[byteIndex + 2] = f;
				byteArray[byteIndex + 3] = e;
				byteArray[byteIndex + 4] = d;
				byteArray[byteIndex + 5] = c;
				byteArray[byteIndex + 6] = b;
				byteArray[byteIndex + 7] = a;
			}
		}
		else
		{
			ensureCapacity(byteIndex + 9);

			if (byteOrder == ByteOrder.BIG_ENDIAN)
			{
				byteArray[byteIndex    ] = (byte) (high >>> 24 & 0xFF << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (high >>> (16 + bitIndex));
				byteArray[byteIndex + 2] = (byte) (high >>> (8  + bitIndex));
				byteArray[byteIndex + 3] = (byte) (high >>> bitIndex);
				byteArray[byteIndex + 4] = (byte) (low  >>> 24 & 0xFF << bitIndex | high & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 5] = (byte) (low  >>> (16 + bitIndex));
				byteArray[byteIndex + 6] = (byte) (low  >>> (8  + bitIndex));
				byteArray[byteIndex + 7] = (byte) (low  >>> bitIndex);
				byteArray[byteIndex + 8] = (byte) (byteArray[byteIndex + 4] & 0xFF << bitIndex | low & 0xFF >>> 8 - bitIndex);
			}
			else
			{
				byteArray[byteIndex    ] = (byte) (low  << bitIndex | byteArray[byteIndex] & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 1] = (byte) (low  >>> (8  - bitIndex));
				byteArray[byteIndex + 2] = (byte) (low  >>> (16 - bitIndex));
				byteArray[byteIndex + 3] = (byte) (low  >>> (24 - bitIndex));
				byteArray[byteIndex + 4] = (byte) (high << bitIndex | low >>> (32 - bitIndex)  & 0xFF >>> 8 - bitIndex);
				byteArray[byteIndex + 5] = (byte) (high >>> (8  - bitIndex));
				byteArray[byteIndex + 6] = (byte) (high >>> (16 - bitIndex));
				byteArray[byteIndex + 7] = (byte) (high >>> (24 - bitIndex));
				byteArray[byteIndex + 8] = (byte) (byteArray[byteIndex + 4] & 0xFF << bitIndex | high >>> (32 - bitIndex)  & 0xFF >>> 8 - bitIndex);
			}
		}
	}
	public void write(final long[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final long[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final long[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + 8 * data.length);
		else
			ensureCapacity(byteIndex + 8 * data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + 8 * i, bitIndex, byteOrder);
	}
	public void write(final float data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final float data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final float data, final int byteIndex, final ByteOrder byteOrder)
	{
		write(data, byteIndex, 0, byteOrder);
	}
	public void write(final float data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((int) data, byteIndex, bitIndex, byteOrder);
	}
	public void write(final float[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final float[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final float[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + 4 * data.length);
		else
			ensureCapacity(byteIndex + 4 * data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + 4 * i, bitIndex, byteOrder);
	}
	public void write(final double data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final double data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final double data, final int byteIndex, final ByteOrder byteOrder)
	{
		write(data, byteIndex, 0, byteOrder);
	}
	public void write(final double data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((long) data, byteIndex, bitIndex, byteOrder);
	}
	public void write(final double[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final double[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final double[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + 8 * data.length);
		else
			ensureCapacity(byteIndex + 8 * data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + 8 * i, bitIndex, byteOrder);
	}
	public void write(final char data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final char data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final char data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((byte) data, byteIndex, bitIndex, byteOrder);
	}
	public void write(final char[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final char[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final char[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			write(data[i], byteIndex + i, bitIndex, byteOrder);
	}
	public void write(final String data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final String data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final String data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write(data.getBytes(), byteIndex, bitIndex, byteOrder);
	}
	public void write(final String[] data, final int byteIndex)
	{
		write(data, byteIndex, 0, defaultByteOrder);
	}
	public void write(final String[] data, final int byteIndex, final int bitIndex)
	{
		write(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void write(final String[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		for (int i = 0, writebyteIndex = byteIndex; i < data.length; i++)
		{
			write(data[i], writebyteIndex, bitIndex, byteOrder);
			writebyteIndex += data[i].length();
		}
	}
	public void writeBinary(final String data, final int byteIndex)
	{
		writeBinary(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeBinary(final String data, final int byteIndex, final int bitIndex)
	{
		writeBinary(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeBinary(final String data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((byte) Integer.parseInt(data, 2), byteIndex, bitIndex, byteOrder);
	}
	public void writeBinaries(final String[] data, final int byteIndex)
	{
		writeBinaries(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeBinaries(final String[] data, final int byteIndex, final int bitIndex)
	{
		writeBinaries(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeBinaries(final String[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			writeBinary(data[i], byteIndex + i, bitIndex, byteOrder);
	}
	public void writeOctal(final String data, final int byteIndex)
	{
		writeOctal(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeOctal(final String data, final int byteIndex, final int bitIndex)
	{
		writeOctal(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeOctal(final String data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((byte) Integer.parseInt(data, 8), byteIndex, bitIndex, byteOrder);
	}
	public void writeOctals(final String[] data, final int byteIndex)
	{
		writeOctals(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeOctals(final String[] data, final int byteIndex, final int bitIndex)
	{
		writeOctals(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeOctals(final String[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			writeOctal(data[i], byteIndex + i, bitIndex, byteOrder);
	}
	public void writeHex(final String data, final int byteIndex)
	{
		writeOctal(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeHex(final String data, final int byteIndex, final int bitIndex)
	{
		writeOctal(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeHex(final String data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		write((byte) Integer.parseInt(data, 16), byteIndex, bitIndex, byteOrder);
	}
	public void writeHexs(final String[] data, final int byteIndex)
	{
		writeHexs(data, byteIndex, 0, defaultByteOrder);
	}
	public void writeHexs(final String[] data, final int byteIndex, final int bitIndex)
	{
		writeHexs(data, byteIndex, bitIndex, defaultByteOrder);
	}
	public void writeHexs(final String[] data, final int byteIndex, final int bitIndex, final ByteOrder byteOrder)
	{
		if (bitIndex == 0)
			ensureCapacity(byteIndex + data.length);
		else
			ensureCapacity(byteIndex + data.length + 1);

		for (int i = 0; i < data.length; i++)
			writeHex(data[i], byteIndex + i, bitIndex, byteOrder);
	}

	private void ensureCapacity(final int size)
	{
		if (byteArray.length < size)
			byteArray = Arrays.copyOf(byteArray, size);
	}
}