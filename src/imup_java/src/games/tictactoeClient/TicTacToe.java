package games.tictactoeClient;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import games.Game;

@SuppressWarnings("serial")
public class TicTacToe extends Game {

    private int counter;
    private TttPlayer x, o;


    public TicTacToe(String player1, String player2, String gameID) {
        super("TicTacToe", gameID, 3, 3, 400, 400);
        x = new TttPlayer(player1, "X");
        o = new TttPlayer(player2, "O");
        
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setActionCommand("" + i + j);
                gameGrid[i][j].addActionListener(new ButtonListener());
            }
        }
    }
    
    public int getRows() {
        return gameRows;
    }

    public int getCols() {
        return gameCols;
    }

    public JButton[][] getGrid() {
        return gameGrid;
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
                {gameID, playerID, newPosition} = receive_moveRequest();
                {i,j} = newPosition;
                
                String playerType = (x.getPlayerID().equals(playerID) ? "X" : "O");
                ((JButton) gameGrid[i][j]).setText(playerType);
           
                    
                    
                wincheck = receive_cmd();
                switch (wincheck) {
                case "-1":
                    JOptionPane.showMessageDialog(null, "The board is full!");
                    break;
                case "0":
                    break;
                default:
                    JOptionPane.showMessageDialog(null, wincheck + " wins the game!");
                    break;
                }
            }
       }
   }
    
    
    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            // SKICKA TILL SERVER
            if (!(((JButton) e.getSource()).getText().equals("X") || 
                    ((JButton) e.getSource()).getText().equals("O"))) {
                send_MoveReq(e.getActionCommand(), o.getPlayerID(), getGameID());
            }
        }
    }
}