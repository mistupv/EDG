package misc.other;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.ImageIO;

import misc.Misc;

public class ScreenCapturer
{
	public static boolean capture(String outputPath)
	{
		final File outputFile = new File(outputPath);

		return ScreenCapturer.capture(outputFile);
	}
	public static boolean capture(File outputFile)
	{
		final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		final Rectangle captureArea = new Rectangle(screenSize);

		return ScreenCapturer.capture(captureArea, outputFile);
	}
	public static boolean capture(Rectangle captureArea, String outputPath)
	{
		final File outputFile = new File(outputPath);

		return ScreenCapturer.capture(captureArea, outputFile);
	}
	public static boolean capture(Rectangle captureArea, File outputFile)
	{
		try
		{
			final Robot robot = new Robot();
			final BufferedImage bufImg = robot.createScreenCapture(captureArea);
			final File outputDirectory = outputFile.isFile() ? outputFile.getParentFile() : outputFile;

			Misc.createFolder(outputDirectory);

			return ImageIO.write(bufImg, "jpg", outputFile);
		}
		catch (Exception e)
		{
			return false;
		}
	}
}