package games.bomberman;

import javax.swing.*;

public class MORunner {

	
	public static void main(String[] args)
	{
		MovingObjects mo = new MovingObjects();
		JFrame f = new JFrame("Testing");
		f.add(mo);
		f.setVisible(true);
		f.setSize(600,600);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
	}
}
