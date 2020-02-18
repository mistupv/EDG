package misc.java;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

public class Reflection
{
	public static Class<?> getClass(String className)
	{
		switch (className)
		{
			case "String":
				return String.class;
			case "boolean":
				return boolean.class;
			case "byte":
				return byte.class;
			case "int":
				return int.class;
			case "long":
				return long.class;
			case "float":
				return float.class;
			case "double":
				return double.class;
			case "char":
				return char.class;
		}

		try
		{
			final ClassLoader classLoader = Reflection.class.getClassLoader();
			final Class<?> clazz = classLoader.loadClass(className);

			return clazz;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		return null;
	}
	public static Object getInstance(Class<?> clazz, Class<?>[] parameterTypes, Object[] parameterValues)
	{
		try
		{
			final Constructor<?> constructor = clazz.getConstructor(parameterTypes);

			constructor.setAccessible(true);

			return constructor.newInstance(parameterValues);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		return null;
	}
	public static Method getMethod(Class<?> clazz, String name, Class<?>[] parameterTypes)
	{
		try
		{
			final Method method = clazz.getMethod(name, parameterTypes);

			method.setAccessible(true);

			return method;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		return null;
	}
	public static Object getValue(String type, String value)
	{
		switch (type)
		{
			case "String":
				return value;
			case "boolean":
				return (boolean) Boolean.parseBoolean(value);
			case "byte":
				return (byte) Byte.parseByte(value);
			case "int":
				return (int) Integer.parseInt(value);
			case "long":
				return (long) Long.parseLong(value);
			case "float":
				return (float) Float.parseFloat(value);
			case "double":
				return (double) Double.parseDouble(value);
			case "char":
				return (char) value.charAt(0);
		}
		return null;
	}
}