package eknife.misc;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Enumeration;

public final class Cronometro
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	private static Hashtable<String, Cronometro> cronometro = new Hashtable<String, Cronometro>();

	/****************************************************************/
	/************************ Main functions ************************/
	/****************************************************************/
	private static Cronometro getCronometro(String clave)
	{
		if (Cronometro.cronometro.get(clave) == null)
			Cronometro.cronometro.put(clave, new Cronometro());

		return Cronometro.cronometro.get(clave);
	}
	public static void empezar(String clave)
	{
		Cronometro.getCronometro(clave).empezar();
	}
	public static void terminar(String clave)
	{
		Cronometro.getCronometro(clave).terminar();
	}
	public static long obtenerContador(String clave)
	{
		return Cronometro.getCronometro(clave).getContador();
	}
	public static long obtenerTiempo(String clave)
	{
		return Cronometro.getCronometro(clave).getMilisegundos();
	}

	public static void mostrarCronometros()
	{
		System.out.println();
		System.out.println("--------------------------------");
		System.out.println("----------- Tiempos ------------");
		System.out.println("--------------------------------");

		String key;
		Cronometro cronometro;

		ArrayList<String> claves = new ArrayList<String>();
		ArrayList<Cronometro> cronometros = new ArrayList<Cronometro>();

		Enumeration<String> keys = Cronometro.cronometro.keys();
		for (int i = 0; i < Cronometro.cronometro.size(); i++)
		{
			key = keys.nextElement();
			cronometro = Cronometro.cronometro.get(key);

			int j = 0;
			for (; j < cronometros.size(); j++)
				if (cronometros.get(j).getMilisegundos() < cronometro.getMilisegundos())
					break;

			claves.add(j, key);
			cronometros.add(j, cronometro);
		}

		for (int i = 0; i < cronometros.size(); i++)
		{
			cronometro = cronometros.get(i);
			final long contador = cronometro.getContador();
			final long milisegundos = cronometro.getMilisegundos();
			System.out.println(claves.get(i) + " x " + contador + " : " + milisegundos + " -> " + 1000.0 * milisegundos / contador);
		}
		System.out.println();
	}
	public static void borrarCronometros()
	{
		Cronometro.cronometro.clear();
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private long milisegundos = 0;
	private long inicioMilisegundos = 0;
	private long vecesLlamado = 0;
	private long contador = 0;

	/****************************************************************/
	/************************** Constructor *************************/
	/****************************************************************/
	private Cronometro()
	{
		
	}

	/****************************************************************/
	/************************ Main functions ************************/
	/****************************************************************/
	private void empezar()
	{
		if (this.vecesLlamado == 0)
			this.inicioMilisegundos = System.currentTimeMillis();
		this.vecesLlamado++;
		this.contador++;
	}
	private void terminar()
	{
		this.vecesLlamado--;
		if (this.vecesLlamado == 0)
			this.milisegundos += System.currentTimeMillis() - this.inicioMilisegundos;
	}
	private long getContador()
	{
		return this.contador;
	}
	private long getMilisegundos()
	{
		return this.milisegundos;
	}
}