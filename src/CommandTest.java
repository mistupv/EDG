import eknife.EKnife;

public class CommandTest
{
	public static void main(String[] args)
	{
		args = new String[12];
		args[0] = "-ln";
		args[1] = "Erlang";
		args[2] = "-ip";
		args[3] = "/Users/Fenix/Desktop/Dropbox/Implementaciones/David/Trabajo/Program Slicing/MagicTool/tmp/CompleteSlice.txt";
		args[4] = "-op";
		args[5] = "/Users/Fenix/Desktop/Dropbox/Implementaciones/David/Trabajo/Program Slicing/MagicTool/tmp/CompleteSlice_temp.txt";
		args[6] = "-li";
		args[7] = "20";
		args[8] = "-na";
		args[9] = "C";
		args[10] = "-oc";
		args[11] = "1";

		EKnife.main(args);
	}
}