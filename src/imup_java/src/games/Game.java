package games;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import communication.*;
import java.awt.GridLayout;
import javax.swing.*;
import player.*;
import utils.*;

@SuppressWarnings("serial")
public abstract class Game extends JFrame implements Runnable {
    //protected eller private? Vad är bäst måntro?

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
    /*
    public static void sendChattMessage(String input, CommunicationWithErlang converter, OtpMbox mailbox, String playerID, String gameID) {
        if (!input.equals("")) {
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
            Date date = new Date();
            String newOutput = " [" + df.format(date) + "]: " + input + "\n";
            sendMessage(mailbox, converter, gameID, playerID, newOutput);
        }
    }

    public static void receiveChattMessage(JTextArea chatOutput, CommunicationWithErlang converter, OtpMbox mailbox, String playerID, String gameID) {
        Arguments arguments = receiveMessage(mailbox, converter);
        String[] message = arguments.getArguments();
        String currentOutput = chatOutput.getText();
        chatOutput.setText(currentOutput + "\n" + message[3]);
    }
    */
}
