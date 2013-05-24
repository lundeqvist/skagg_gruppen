package games.tictactoeClient;

import com.ericsson.otp.erlang.OtpMbox;
import communication.*;
import games.Game;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import utils.*;

public class TicTacToeClient extends Game {

    private TttPlayer x, o;
    private OtpMbox mailbox;

    public TicTacToeClient(String player1, String player2, String gameID) {
        super("TicTacToe", gameID, 3, 3, 400, 400);
        this.gameID = gameID;
        converter = new CommunicationWithErlang(player2);        
        mailbox = converter.createMailbox(gameID, player2);
        x = new TttPlayer(player1, "X");
        o = new TttPlayer(player2, "O");
       
        Utils.sendMessage(mailbox, converter, gameID, o.getPlayerID(), "{join_game}");
        System.out.println(o.getPlayerID() + " joined the game " + gameID); 
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setActionCommand("" + i + j);
                gameGrid[i][j].addActionListener(new ButtonListener());
            }
        }
    }

    @Override
    public void run() {
        Thread listener = new Thread(new ServerListener());
        listener.start();
    }


    @Override
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
        public ServerListener() {}

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
                        dispose();
                        break;
                    case "'0'":
                        break;
                    default:
                        JOptionPane.showMessageDialog(null, arguments.getPlayerID() + " wins the game!");
                        dispose();
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
                Utils.sendMessage(mailbox, converter, gameID+"host", o.getPlayerID(), "{" + e.getActionCommand() + "}");
            }
        }
    }
}