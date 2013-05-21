package games;

import com.ericsson.otp.erlang.OtpMbox;
import java.awt.event.*;
import javax.swing.*;
import communication.*;
import utils.*;
import games.tictactoeHost.*;
import games.tictactoeClient.*;

/**
 * GameMenu is the main process who creates and handles the OnlineList and
 * GameMenuChat
 */
@SuppressWarnings("serial")
public class GameMenu extends JFrame {

    private JMenuBar menuBar;
    private JMenu menu, submenu;
    private JMenuItem menuItem;
    private JButton gameButton, connectButton;
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
    public GameMenu(OtpMbox mailbox, String playerID, String ip, String port, String[] users) {
        super("IMUP");
        this.playerID = playerID;
        this.mailboxUsers = mailbox;
        this.users = users;
        converter = new CommunicationWithErlang();
        this.mailbox = converter.createMailbox("gameMenu", playerID);

        init_menubar();
        init_content();
        setJMenuBar(menuBar);

        setVisible(true);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(800, 400);
        setResizable(false);
        setLocationRelativeTo(null);
        addKeyListener(new WindowListener());
    }

    /**
     * Initiates all the GUI, GameMenuChat and OnlineList.
     */
    private void init_content() {
        mainPanel = new JPanel();
        getContentPane().add(mainPanel);
        mainPanel.setLayout(null);

        //Chatt
        GMCI = new GameMenuChat(playerID);
        GMCI.setMainPanel(mainPanel);
        Thread t = new Thread(GMCI);
        t.start();

        onlineList = new OnlineList(mailboxUsers, playerID, users);
        onlineList.setMainPanel(mainPanel);
        Thread tOnline = new Thread(onlineList);
        tOnline.start();
        
        Thread listener = new Thread(new ServerListener());
        listener.start();

        //Här skulle det nog vara bra om online listan skapades som en egen tråd

        ImageIcon iconTicTacToe = new ImageIcon(getClass().getResource("games.gif"));
        gameButton = new JButton("TicTacToe", iconTicTacToe);
        //gameButton = new JButton("Games");
        gameButton.setBounds(260, 10, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
        ImageIcon iconBomber = new ImageIcon(getClass().getResource("bomberman.png"));
        gameButton = new JButton(iconBomber);
        gameButton.setBounds(398, 10, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
        ImageIcon iconChess = new ImageIcon(getClass().getResource("chess.png"));
        gameButton = new JButton(iconChess);
        gameButton.setBounds(260, 148, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
        ImageIcon iconTroll = new ImageIcon(getClass().getResource("troll.png"));
        gameButton = new JButton(iconTroll);
        gameButton.setBounds(398, 148, 128, 128);
        gameButton.addActionListener(new ButtonListener());
        mainPanel.add(gameButton);
        connectButton = new JButton("Connect to Server!");
        connectButton.setBounds(260, 286, 266, 50);
        connectButton.addActionListener(new ButtonListener());
        mainPanel.add(connectButton);
    }

    private void init_menubar() {
        menuBar = new JMenuBar();
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_ALT);
        menuBar.add(menu);

        //a group of JMenuItems
        menuItem = new JMenuItem("Settings", KeyEvent.VK_1);
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1, ActionEvent.ALT_MASK));
        menuItem.addActionListener(new ButtonListener());
        menu.add(menuItem);

        menuItem = new JMenuItem("Exit");
        menuItem.setMnemonic(KeyEvent.VK_X);
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.ALT_MASK));
        menuItem.addActionListener(new ButtonListener());
        menu.add(menuItem);

        //a submenu
        menu.addSeparator();
        submenu = new JMenu("Submenu");
        submenu.setMnemonic(KeyEvent.VK_S);

        menuItem = new JMenuItem("Pancakes");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2, ActionEvent.ALT_MASK));
        submenu.add(menuItem);

        menuItem = new JMenuItem("Saft");
        submenu.add(menuItem);
        menu.add(submenu);

        menu = new JMenu("Help");
        menu.setMnemonic(KeyEvent.VK_N);

        menuItem = new JMenuItem("Info");
        menuItem.setMnemonic(KeyEvent.VK_F1);
        menuItem.addActionListener(new ButtonListener());
        menu.add(menuItem);
        menu.addKeyListener(new WindowListener());

        menuBar.add(menu);
    }

    public void go() {
        System.out.println("Running!");
    }

    public void settings() {
        System.out.println("TBfuckingI.");
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
                    OtpMbox mbox = converter.createMailbox("ticTacToe", playerID);
                    Utils.sendMessageToUsers(mbox, converter, "ticTacToe", playerID, "{confirminvite, ticTacToe}", selectedUsers);
                    Thread t = new Thread(new ServerListenerInvite(mbox, "ticTacToe", playerID, selectedUsers, selectedUsers.length));
                    t.start();
                    break;
                case "Connect to Server!":
                    break;
                case "Settings":
                    settings();
                    break;
                case "Exit":
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

    private class WindowListener implements KeyListener {

        @Override
        public void keyPressed(KeyEvent e) {
            // TODO Auto-generated method stub
            switch (e.getKeyCode()) {
                //case KeyEvent.VK_ENTER:
                //Utils.updateChat(chatOutput, chatInput);
                //break;
                case KeyEvent.VK_F1:
                    info();
                    break;
                default:

                    break;
            }
        }

        @Override
        public void keyReleased(KeyEvent arg0) {
            // TODO Auto-generated method stub
        }

        @Override
        public void keyTyped(KeyEvent arg0) {
            // TODO Auto-generated method stub
        }
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    private class ServerListener implements Runnable {
        private ServerListener() {
        }

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
            converter = new CommunicationWithErlang();
            this.mailbox = mailbox;
            //mailbox = converter.createMailbox(gameID, playerID);
            this.selectedUsers = selectedUsers;
            this.numberOfPlayers = numberOfPlayers;
        }

        private void serverListenerInvite() {
            while (true) {
                if (numberOfPlayers == 0) {
                    Utils.sendMessageToUsers(mailbox, converter, gameID, playerID, "{startup, ticTacToe1337}", selectedUsers);
                    Thread t = new Thread(new TicTacToeHost(playerID, selectedUsers[0], "ticTacToe1337"));
                    t.start();
                    return;
                } else {
                    Arguments arguments = Utils.receiveMessage(mailbox, converter);
                    String answer = arguments.getArguments()[0];
                    if (answer.equals("yes")) {
                        numberOfPlayers--;
                    } else {
                        System.exit(0);
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