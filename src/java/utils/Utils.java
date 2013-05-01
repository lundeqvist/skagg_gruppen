package utils;

import java.text.DateFormat;
import java.util.Date;
import javax.swing.*;

public class Utils
{
	
	
	public static void updateChat(JTextArea chatOutput, JTextField chatInput)
	{
		String input = chatInput.getText();
		if(!input.equals(""))
		{
			String currentOutput = chatOutput.getText();
			DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
			Date date = new Date();
			String newOutput = currentOutput + " [" + df.format(date) + "]: " + input + "\n";
			chatOutput.setText(newOutput);
			chatInput.setText("");
		}
	}
	
	public static int serverConnect()
	{
		return 0;
	}
}
