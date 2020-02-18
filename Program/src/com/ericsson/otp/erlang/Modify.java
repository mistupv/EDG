package com.ericsson.otp.erlang;

import java.lang.reflect.Field;

import com.ericsson.otp.erlang.AbstractNode;

public class Modify
{
	public static void changeHost(AbstractNode node)
	{
		try
		{
			final String alive = node.alive();
			final String host = node.host();
			final int dotIndex = host.indexOf(".");
			if (dotIndex == -1)
				return;
			final String newHost = host.substring(0, dotIndex);

			final Field hostField = AbstractNode.class.getDeclaredField("host");
			hostField.setAccessible(true);
			hostField.set(node, newHost);

			final Field nodeField = AbstractNode.class.getDeclaredField("node");
			nodeField.setAccessible(true);
			nodeField.set(node, alive + "@" + newHost);
		}
		catch (Exception e)
		{
			System.out.println(e);
		}
	}
}