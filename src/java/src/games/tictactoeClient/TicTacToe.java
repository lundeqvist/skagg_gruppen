package games.tictactoeClient;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import games.Game;
import communication.Converter;

@SuppressWarnings("serial")
public class TicTacToe extends Game {

    private int counter;
    private TttPlayer x, o;
    private Converter converter;
    private OtpMbox mailbox;

    public TicTacToe(String player1, String player2, String gameID) {
        super("TicTacToe", gameID, 3, 3, 400, 400);
        converter = new Converter();
        mailbox = converter.createMailbox(gameID);
        converter.send_messagePing(player2, gameID, mailbox);
        x = new TttPlayer(player1, "X");
        o = new TttPlayer(player2, "O");

        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                gameGrid[i][j].setActionCommand("" + i + j);
                gameGrid[i][j].addActionListener(new ButtonListener());
            }
        }
        //x.setGameWindow(gameGrid, gameID);
        //o.setGameWindow(gameGrid, gameID);
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

    public OtpErlangTuple convertToErlang(String gameID, String playerID, String message, String winCheck) {
        System.out.println("Convert to erlang" + message);
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(playerID);
        id[1] = new OtpErlangAtom(gameID);
        OtpErlangObject[] msg = new OtpErlangObject[5];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom("move");
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(message);
        msg[4] = new OtpErlangAtom(winCheck);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        //mailbox.send("pong", server, tuple);
        return tuple;
    }

    public String[] convertToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        OtpErlangPid fromPid = (OtpErlangPid) (rtuple.elementAt(0));
        OtpErlangObject playerID = rtuple.elementAt(1);
        OtpErlangObject position = rtuple.elementAt(3);
        OtpErlangObject winCheck = rtuple.elementAt(4);
        System.out.println("Message: " + playerID + " received from:  "
                + fromPid.toString());
        String[] message = new String[2];
        message[0] = playerID.toString();
        message[1] = position.toString();
        message[2] = winCheck.toString();
        System.out.println("Convert to java " + message);
        return message;
    }

    public String[] receiveMessage() {
        OtpErlangObject robj = converter.receive(mailbox);
        return convertToJava(robj);
    }

    private class ServerListener {
        // HÄMTA FÖRFRÅGAN FRÅN SERVER

        public void serverPerformed() {
            while (true) {
                String[] message = receiveMessage();
                String playerType = (x.getPlayerID().equals(message[1]) ? "X" : "O");
                ((JButton) gameGrid[i][j]).setText(playerType);
                switch (message[2]) {
                    case "-1":
                        JOptionPane.showMessageDialog(null, "The board is full!");
                        break;
                    case "0":
                        break;
                    default:
                        JOptionPane.showMessageDialog(null, message[2] + " wins the game!");
                        break;
                }
            }
        }
    }

    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            // SKICKA TILL SERVER
            if (!(((JButton) e.getSource()).getText().equals("X")
                    || ((JButton) e.getSource()).getText().equals("O"))) {
                //winCheck måste skickas med så länge vi inte har en fungerande server
                converter.send(convertToErlang(getGameID(), o.getPlayerID(), e.getActionCommand(),winCheck), mailbox);
            }
        }
    }
}