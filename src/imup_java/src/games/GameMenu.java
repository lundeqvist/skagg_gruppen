package games;

import com.ericsson.otp.erlang.OtpMbox;
import communication.*;
import games.tictactoeClient.*;
import games.tictactoeHost.*;
import java.awt.event.*;
import javax.swing.*;
import utils.*;

/**
 * GameMenu is the main process who creates and handles the OnlineList and
 * GameMenuChat
 */
public class GameMenu extends JFrame {

    private JMenuBar menuBar;
    private JMenu menu;
    private JMenuItem menuItem;
    private JButton gameButton;
    private JPanel mainPanel;
    private GameMenuChat GMCI;
    private OnlineList onlineList;
    private String playerID;
    private String[] users;
    private OtpMbox mailbox, mailboxUsers;
    private CommunicationWithErlang converter;
    private int gameCounter = 0;

    /**
     * @param mailbox is the mailbox that were used in Client that OnlineList
     * will need to communicate with the server to update the onlinelist.
     *
     * @param playerID
     * @param ip
     * @param port
     * @param users String representation of all the users who is online.
     */
    public GameMenu(OtpMbox mailbox, final String playerID, String ip, String port, String[] users) {
        super("IMUP");
        this.playerID = playerID;
        this.mailboxUsers = mailbox;
        this.users = users;
        converter = new CommunicationWithErlang(playerID);
        this.mailbox = converter.createMailbox("gameMenu", playerID);
        init_menubar();
        init_content();
        setJMenuBar(menuBar);
        setVisible(true);
        setSize(800, 400);
        setResizable(false);
        setLocationRelativeTo(null);
    }

    /**
     * Initiates all the GUI, GameMenuChat and OnlineList.
     */
    private void init_content() {
        mainPanel = new JPanel();
        getContentPane().add(mainPanel);
        mainPanel.setLayout(null);

        //Chat
        GMCI = new GameMenuChat(playerID);
        GMCI.setMainPanel(mainPanel);
        Thread t = new Thread(GMCI);
        t.start();

        //Onlinelist
        onlineList = new OnlineList(mailboxUsers, playerID, users);
        onlineList.setMainPanel(mainPanel);
        Thread tOnline = new Thread(onlineList);
        tOnline.start();

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                Utils.sendMessage(mailbox, converter, "onlinelist", playerID, "{remove_player}");                  
                System.out.println("Exit");
                System.exit(0);
            }
        });

        Thread listener = new Thread(new ServerListener());
        listener.start();
        
        
        ImageIcon iconTicTacToe = new ImageIcon(getClass().getResource("games.gif"));
        gameButton = new JButton("TicTacToe", iconTicTacToe);
        gameButton.setBounds(260, 10, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
        ImageIcon iconChess = new ImageIcon(getClass().getResource("chess.png"));
        gameButton = new JButton(iconChess);
        gameButton.setBounds(260, 148, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
    }

    private void init_menubar() {
        menuBar = new JMenuBar();
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_ALT);
        menuBar.add(menu);

        menuItem = new JMenuItem("Exit");
        menuItem.setMnemonic(KeyEvent.VK_X);
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.ALT_MASK));
        menuItem.addActionListener(new ButtonListener());
        menu.add(menuItem);

        menu = new JMenu("Help");
        menu.setMnemonic(KeyEvent.VK_N);

        menuItem = new JMenuItem("Info");
        menuItem.setMnemonic(KeyEvent.VK_F1);
        menuItem.addActionListener(new ButtonListener());
        menu.add(menuItem);
        menuBar.add(menu);
    }

    public void info() {
        JOptionPane.showMessageDialog(this, "Looks like you have problems, do you want some help?");
    }

    /**
     * Listens to all buttons and creates the game with the user(s) that is
     * picked.
     */
    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "TicTacToe":
                    String[] selectedUsers = onlineList.getSelectedUsers();
                    if (Utils.contains(selectedUsers, playerID)) {
                        JOptionPane.showMessageDialog(null, "You can't invite yourself!");
                    } else {
                        String uniqueGameID = "ttt" + playerID + gameCounter;
                        OtpMbox mbox = converter.createMailbox(uniqueGameID, playerID);
                        Utils.sendMessage(mailbox, converter, uniqueGameID, playerID, "{join_game}");
                        Utils.sendMessageToUsers(mbox, converter, uniqueGameID, playerID, "{confirminvite, TicTacToe}", selectedUsers);
                        Thread t = new Thread(new ServerListenerInvite(mbox, uniqueGameID, playerID, selectedUsers, selectedUsers.length));
                        t.start();
                        gameCounter++;
                    }
                    break;
                case "Exit":
                    Utils.sendMessage(mailbox, converter, "onlinelist", playerID, "{remove_player}");
                    System.exit(0);
                    break;
                case "Info":
                    info();
                    break;
                default:
                    System.out.println("Not implemented yet");
                    break;
            }
        }
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    private class ServerListener implements Runnable {
        
        private ServerListener() {}

        private void serverListener() {
            while (true) {
                Arguments arguments = Utils.receiveMessage(mailbox, converter);
                String answer;
                switch (arguments.getArguments()[0]) {
                    case "confirminvite":
                        int answerint = JOptionPane.showConfirmDialog(null, arguments.getPlayerID() + " has invited you to play " + arguments.getArguments()[1] + ".", "Invite", JOptionPane.YES_NO_OPTION);
                        answer = answerint == 0 ? "yes" : "no";
                        String[] user = {arguments.getPlayerID()};
                        Utils.sendMessageToUsers(mailbox, converter, arguments.getGameID(), playerID, "{" + answer + "}", user);
                        break;
                    case "startup":
                        Thread t = new Thread(new TicTacToeClient(arguments.getPlayerID(), playerID, arguments.getArguments()[1]));
                        t.start();
                    default:
                        break;
                }
            }
        }

        @Override
        public void run() {
            serverListener();
        }
    }

    private class ServerListenerInvite implements Runnable {

        private OtpMbox mailbox;
        private CommunicationWithErlang converter;
        String[] selectedUsers;
        int numberOfPlayers;
        String gameID, playerID;

        private ServerListenerInvite(OtpMbox mailbox, String gameID, String playerID, String[] selectedUsers, int numberOfPlayers) {
            this.gameID = gameID;
            this.playerID = playerID;
            converter = new CommunicationWithErlang(playerID);
            this.mailbox = mailbox;
            this.selectedUsers = selectedUsers;
            this.numberOfPlayers = numberOfPlayers;
        }

        private void serverListenerInvite() {
            while (true) {
                if (numberOfPlayers == 0) {
                    Utils.sendMessageToUsers(mailbox, converter, gameID, playerID, "{startup, " + gameID + "}", selectedUsers);
                    Thread t = new Thread(new TicTacToeHost(mailbox, playerID, selectedUsers[0], gameID));
                    t.start();                    
                    return;
                } else {
                    Arguments arguments = Utils.receiveMessage(mailbox, converter);
                    String answer = arguments.getArguments()[0];
                    if (answer.equals("yes")) {
                        numberOfPlayers--;
                    } else {
                        return;
                    }
                }
            }
        }

        @Override
        public void run() {
            serverListenerInvite();
        }
    }
}