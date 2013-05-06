package games.tictactoe;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import games.game.*;

@SuppressWarnings("serial")
public class TicTacToe extends Game {
	
	private int counter;
	private GameControl gc;
	private TttPlayer x,o;
	
	public TicTacToe(int rows, int cols, GameControl gc)
	{
            super("TicTacToe","TTTno1",rows,cols,400,400);
		counter = 0;
		x = gc.getPlayerX();
		o = gc.getPlayerO();
		this.gc = gc;
 
                for(int i = 0; i<gameRows; i++)
                {
                    for(int j = 0; j<gameCols; j++)
                    {
                        gameGrid[i][j].addActionListener(new ButtonListener());
                        gameGrid[i][j].setActionCommand(""+i+j);
                    }
                }
	}
        private TicTacToe getTTT()
        {
            return this;
        }
	
	public int getRows()
	{
		return this.gameRows;
	}
	
	public int getCols()
	{
		return this.gameCols;
	}
	
	public JButton[][] getGrid()
	{
		return this.gameGrid;
	}
	
	public void run()
	{
		setVisible(true);
	}

	public void reset()
	{
		for(int i = 0; i<gameRows; i++)
		{
			for(int j = 0; j<gameCols; j++)
			{
				gameGrid[i][j].setText("");
			}
		}
		counter = 0;
	}
	
        @Override
	public String toString()
	{
		String text = "";
		for(int i = 0; i<gameRows; i++)
		{
			for(int j = 0; j<gameCols; j++)
			{
				text = text + gameGrid[i][j].getText();
			}
		}
		return text;
	}
	
	private class ButtonListener implements ActionListener
	{

		@Override
		public void actionPerformed(ActionEvent e) {
			
			switch(((JButton) e.getSource()).getText())
			{
				case "X":
					System.out.println("Already taken!");
					break;
				case "O":	
					System.out.println("Already taken!");
					break;
				default:
					if(counter % 2 == 0)
					{
						((JButton) e.getSource()).setText(x.getType());
						gc.winCheck(x.getType(),getTTT());
					}
					else
					{
						((JButton) e.getSource()).setText(o.getType());
						gc.winCheck(o.getType(),getTTT());	
					}
					counter++;
					break;
			}	
		}
	}
}
