package games;

import communication.*;
import java.awt.GridLayout;
import javax.swing.*;
import player.*;

public abstract class Game extends JFrame implements Runnable {

    protected String gameName, gameID;
    protected int gameRows, gameCols, frameWidth, frameHeight;
    protected JButton[][] gameGrid;
    protected JPanel mainPanel;
    protected Player[] players;
    protected CommunicationWithErlang converter;

    public Game(String name, String id, int rows, int cols, int width, int height) {
        gameName = name;
        gameID = id;
        gameRows = rows;
        gameCols = cols;
        frameWidth = width;
        frameHeight = height;
        init_baseGUI();
    }

    private void init_baseGUI() {
        mainPanel = new JPanel();
        mainPanel.setLayout(new GridLayout(gameRows, gameCols, 2, 2));

        gameGrid = new JButton[gameRows][gameCols];

        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j] = new JButton();
                mainPanel.add(gameGrid[i][j]);
            }
        }
        add(mainPanel);
        setLocationRelativeTo(null);
        setSize(frameWidth, frameHeight);
        setResizable(false);
        setTitle(gameName);
        setVisible(true);
    }

    public String getName() {
        return this.gameName;
    }

    public String getGameID() {
        return this.gameID;
    }
}
