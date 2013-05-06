package games.tictactoeHost;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import games.Game;

@SuppressWarnings("serial")
public class TicTacToe extends Game {

    private int counter;
    private GameControl gc;
    private TttPlayer x, o;


    public TicTacToe(int rows, int cols, GameControl gc) {
        super("TicTacToe", "GAMEID", rows, cols, 400, 400);
        counter = 0;
        x = gc.getPlayerX();
        o = gc.getPlayerO();
        this.gc = gc;
        
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setActionCommand("" + i + j);
                gameGrid[i][j].addActionListener(new ButtonListener());
            }
        }
    }
    
    public int getRows() {
        return this.gameRows;
    }

    public int getCols() {
        return this.gameCols;
    }

    public JButton[][] getGrid() {
        return this.gameGrid;
    }


    public void reset() {
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setText("");
            }
        }
        counter = 0;
    }

    public String toString() {
        String text = "";
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                text = text + gameGrid[i][j].getText();
            }
        }
        return text;
    }

    
    
    
    private class ServerListener {
        // HÄMTA FÖRFRÅGAN FRÅN SERVER
        public void serverPerformed() {
            while (true) {
                String playerType;
                {gameID, playerID, newPosition} = receive_message();
                {i,j} = newPosition;        
                
                if (x.getName().equals(playerID)) {
                    playerType = "X";
                }
                else {
                    playerType = "O";
                }
                
                        ((JButton) gameGrid[i][j]).setText(playerType);
                        gc.winCheck(playerType, getTTT());
                }
            }
        }
    }
    
    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
        // SKICKA TILL SERVER
        send_moveRequest(e.getActionCommand(), get_playerID(), getgameID());
        }
    }
}