package games;

import java.awt.GridLayout;
import javax.swing.*;

@SuppressWarnings("serial")
public abstract class Game extends JFrame
{
    //protected eller private? Vad är bäst måntro?
	protected String gameName, gameID;
	protected int gameRows, gameCols, frameWidth, frameHeight;
	protected JButton[][] gameGrid;
	protected JPanel mainPanel;
	
	public Game(String name, String id, int rows, int cols, int width, int height)
	{
		gameName = name;
                gameID = id;
		gameRows = rows;
		gameCols = cols;
		frameWidth = width;
		frameHeight = height;
		init_baseGUI();
	}
	
	private void init_baseGUI()
	{
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(gameRows,gameCols,2,2));
		
		gameGrid = new JButton[gameRows][gameCols];
		
		for(int i = 0; i<gameRows; i++)
		{
			for(int j = 0; j<gameCols; j++)
			{
				gameGrid[i][j] = new JButton();
				mainPanel.add(gameGrid[i][j]);
			}
		}
		add(mainPanel);
                setLocationRelativeTo(null);
		setSize(frameWidth,frameHeight);
		setResizable(false);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setTitle(gameName);
                setVisible(true);
	}
	
	public String getName()
	{
		return gameName;
	}
        
        public String getGameID()
        {
            return gameID;
        }
}
