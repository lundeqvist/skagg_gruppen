package games.memory;

import javax.swing.*;

import games.game.*;

@SuppressWarnings("serial")
public class Memory extends Game
{
	private int[][] cards;
	private int nums;
	
	public Memory(String name, String id, int rows, int cols)
	{
		super(name, id, rows, cols, 500, 500);
		if(rows*cols % 2 == 1)
			throw new IllegalArgumentException();
		
		init_cards();
	}
	
	private void init_cards()
	{
		cards = new int[gameRows][gameCols];
		nums = (gameRows*gameCols)/2;
		for(int i = 1; i<=nums; i++)
		{
			while(true)
			{
				Math.random();
				break;
			}
			while(true)
			{
				break;
			}
		}
	}
	
	public static void main(String[] args)
	{
		new Memory("Lol","TheGame",4,4);
	}
	
}
