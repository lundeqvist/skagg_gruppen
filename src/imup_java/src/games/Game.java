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
public abstract class Game extends JFrame {
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
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setTitle(gameName);
        setVisible(true);
    }

    public String getName() {
        return this.gameName;
    }

    public String getGameID() {
        return this.gameID;
    }

    public OtpErlangTuple convertToErlang(OtpMbox mailbox, String gameID, String playerID, String arguments) {
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(gameID);
        msg[2] = new OtpErlangAtom(playerID);
        msg[3] = new OtpErlangAtom(arguments);
        return new OtpErlangTuple(msg);
    }
    
    public Arguments convertToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        String GameID = rtuple.elementAt(1).toString();
        String PlayerID = rtuple.elementAt(2).toString();
        String ArgumentsString = rtuple.elementAt(3).toString();
        return new Arguments(GameID, PlayerID, ArgumentsString);
    }
    
    public Arguments receiveMessage(OtpMbox mailbox) {
        return convertToJava(converter.receive(mailbox));
    }
    
    public void sendMessage(OtpMbox mailbox, String gameID, String playerID, String arguments) {
        converter.send(convertToErlang(mailbox, gameID, playerID, arguments), mailbox);
    }
    
    
}
