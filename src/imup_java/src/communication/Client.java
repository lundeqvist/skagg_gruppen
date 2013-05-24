package communication;

/**
 *
 * @author Grupp 4
 */
import com.ericsson.otp.erlang.OtpMbox;
import games.*;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import utils.*;

/**
 * Client creates the opening window where you enter your name and IP sends it
 * to the server and then creates GameMenu
 */
public class Client {

    OtpMbox mailbox;
    CommunicationWithErlang converter;
    JTextField userNameTextField;
    JTextField userIpTextField;
    JTextField userPortTextField;
    String gameID = "client";
    String playerID;
    static Boolean isServer = false;
    JFrame firstGuiFrame;
    JFrame guiFrame;

    public Client() {
        firstGuiFrame = new JFrame();
        JPanel guiPanel = new JPanel(new GridBagLayout());
        JLabel userNameLabel = new JLabel("Name:");
        userNameTextField = new JTextField(20);
        userNameTextField.setText("Name");
        JButton startErlang = new JButton("startErlang");
        startErlang.addActionListener(new ButtonListener());
        firstGuiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        firstGuiFrame.setTitle("Client");
        firstGuiFrame.setSize(500, 200);
        firstGuiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(3, 3, 3, 3);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(10, 10, 10, 10);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;
        guiPanel.add(userNameLabel, labelGBC);
        guiPanel.add(userNameTextField, fieldGBC);
        guiPanel.add(startErlang, fieldGBC);//     
        firstGuiFrame.add(guiPanel, BorderLayout.NORTH);
        firstGuiFrame.setVisible(true);
    }

    /**
     *
     */
    private void init_GUI() {
        guiFrame = new JFrame();
        JPanel guiPanel = new JPanel(new GridBagLayout());
        JLabel userIpLabel = new JLabel("IP:");
        userIpTextField = new JTextField(20);
        userIpTextField.setText("127.0.0.1");
        JLabel userPortLabel = new JLabel("Port:");
        userPortTextField = new JTextField(20);
        userPortTextField.setText("5555");
        JButton connect = new JButton("connect");
        connect.addActionListener(new ButtonListener());
        guiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        guiFrame.setTitle(playerID);
        guiFrame.setSize(500, 200);
        guiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(3, 3, 3, 3);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(10, 10, 10, 10);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;
        guiPanel.add(userIpLabel, labelGBC);
        guiPanel.add(userIpTextField, fieldGBC);
        guiPanel.add(userPortLabel, labelGBC);
        guiPanel.add(userPortTextField, fieldGBC);
        guiPanel.add(connect, fieldGBC);
        guiFrame.add(guiPanel, BorderLayout.NORTH);
        guiFrame.setVisible(true);
    }

    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "startErlang":
                    playerID = userNameTextField.getText();
                    converter = new CommunicationWithErlang(playerID);
                    converter.runSnameTerminalStuff();
                    firstGuiFrame.setVisible(false);
                    init_GUI();
                    break;
                case "connect":
                    String ipNumber = userIpTextField.getText();
                    String portNumber = userPortTextField.getText();
                    String myIp = null;
                    try {
                        myIp = InetAddress.getLocalHost().toString();
                    } catch (UnknownHostException ex) {
                        Logger.getLogger(Client.class.getName()).log(Level.SEVERE, null, ex);
                    }
                    String arguments = "{" + ipNumber + "," + portNumber + "," + myIp + "}";
                    mailbox = converter.createMailbox("onlinelist", playerID);
                    Utils.sendMessage(mailbox, converter, "onlinelist", playerID, arguments);
                    Arguments users = Utils.receiveMessage(mailbox, converter);
                    guiFrame.setVisible(false);
                    GameMenu gMenu = new GameMenu(mailbox, playerID, ipNumber, portNumber, users.getArguments());
            }
        }
    }

    public static void main(String[] args) {
        //Hello hello = new Hello();
        Client client = new Client();
    }
}
