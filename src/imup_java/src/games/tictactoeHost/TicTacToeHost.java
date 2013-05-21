package games.tictactoeHost;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import games.Game;
import utils.*;
import communication.*;

@SuppressWarnings("serial")
public class TicTacToeHost extends Game {

    private int counter;
    private TttPlayer x, o;
    private GameControl gc;
    private OtpMbox mailbox;

    public TicTacToeHost(String player1, String player2, String gameID) {
        super("TicTacToe", gameID, 3, 3, 400, 400);
        converter = new CommunicationWithErlang();
        mailbox = converter.createMailbox(gameID, player1);
        x = new TttPlayer(player1, "X");
        o = new TttPlayer(player2, "O");
        Utils.sendMessage(mailbox, converter, gameID, x.getPlayerID(), "{join_game}");

        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setActionCommand("" + i + j);
                gameGrid[i][j].addActionListener(new ButtonListener());
            }
        }
        
        Thread listener = new Thread(new GameControl(this, x, o));
        listener.start();
    }

    public void run() {
        Thread listener = new Thread(new ServerListener());
        listener.start();
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

    private class ServerListener implements Runnable {

        public ServerListener() {
        }

        private void serverListener() {
            while (true) {
                Arguments arguments = Utils.receiveMessage(mailbox, converter);
                String position = arguments.getArguments()[0];
                int[] xy = Utils.splitCoordinates(position);
                String wincheck = arguments.getArguments()[1];
                String playerType = (x.getPlayerID().equals(arguments.getPlayerID()) ? "X" : "O");

                ((JButton) gameGrid[xy[0]][xy[1]]).setText(playerType);
                switch (wincheck) {
                    case "'-1'":
                        JOptionPane.showMessageDialog(null, "The board is full!");
                        break;
                    case "'0'":
                        break;
                    default:
                        JOptionPane.showMessageDialog(null, arguments.getPlayerID() + " wins the game!");
                        break;
                }
            }
        }

        @Override
        public void run() {
            serverListener();
        }
    }

    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!(((JButton) e.getSource()).getText().equals("X")
                    || ((JButton) e.getSource()).getText().equals("O"))) {
                String[] selectedUsers = {"elias"};
                Utils.sendMessageToUsers(mailbox, converter, "tttHost", x.getPlayerID(), "{" + e.getActionCommand() + "}",selectedUsers);
            }
        }
    }
}