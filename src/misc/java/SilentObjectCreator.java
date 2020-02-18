package misc.java;

import sun.reflect.ReflectionFactory;
import java.lang.reflect.Constructor;

public class SilentObjectCreator
{
	public static <T> T create(Class<T> clazz)
	{
		return SilentObjectCreator.create(clazz, Object.class);
	}
	private static <T> T create(Class<T> clazz, Class<? super T> parent)
	{
		try
		{
			final ReflectionFactory rf = ReflectionFactory.getReflectionFactory();
			final Constructor<? super T> objDef = parent.getDeclaredConstructor();
			final Constructor<?> intConstr = rf.newConstructorForSerialization(clazz, objDef);

			return clazz.cast(intConstr.newInstance());
		}
		catch (Exception e)
		{
			throw new IllegalStateException("Cannot create object", e);
		}
	}
}