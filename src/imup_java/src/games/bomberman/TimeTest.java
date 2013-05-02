package games.bomberman;

import java.text.DateFormat;
import java.util.Date;

public class TimeTest {
	
	public static void main(String[] args)
	{
		DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
		Date date = new Date();
		System.out.println(df.format(date));
	}
}
