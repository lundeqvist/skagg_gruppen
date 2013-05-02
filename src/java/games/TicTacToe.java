package games;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

@SuppressWarnings("serial")
public class TicTacToe extends JFrame {
	
	private JPanel panel;
	private JButton[][] buttonGrid;
	private int  gameRows, gameCols, counter;
	private GameControl gc;
	private TttPlayer x,o;
	private TicTacToe ttt;
	
	public TicTacToe(GameControl gc)
	{
		new TicTacToe(3,3,gc);
	}
	
	public TicTacToe(int rows, int cols, GameControl gc)
	{
		ttt = this;
		counter = 0;
		gameRows = rows;
		gameCols = cols;
		x = gc.getPlayerX();
		o = gc.getPlayerO();
		panel = new JPanel();
		panel.setLayout(new GridLayout(gameRows,gameCols,1,1));
		buttonGrid = new JButton[gameRows][gameCols];
		for(int i=0;i<gameRows;i++)
		{
			for(int j=0;j<gameCols;j++)
			{
				buttonGrid[i][j] = new JButton();
				buttonGrid[i][j].addActionListener(new ButtonListener());
				panel.add(buttonGrid[i][j]);
			}
		}
		add(panel);
		setLocationRelativeTo(null);
		setSize(400,400);
		//setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.gc = gc;
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
		return this.buttonGrid;
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
				buttonGrid[i][j].setText("");
			}
		}
		counter = 0;
	}
	
	public String toString()
	{
		String text = "";
		for(int i = 0; i<gameRows; i++)
		{
			for(int j = 0; j<gameCols; j++)
			{
				text = text + buttonGrid[i][j].getText();
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
						gc.winCheck(x.getType(),ttt);
					}
					else
					{
						((JButton) e.getSource()).setText(o.getType());
						gc.winCheck(o.getType(),ttt);	
					}
					counter++;
					break;
			}	
		}
	}
}
