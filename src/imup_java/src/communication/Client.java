package communication;
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.ericsson.otp.erlang.OtpMbox;
/**
 *
 * @author Tobias
 */
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import java.io.*;
import javax.swing.AbstractButton;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;
import utils.*;
import games.*;

/**
 * Client creates the opening window where you enter your name and IP sends
 * it to the server and then creates GameMenu
 */ 
public class Client {

    OtpMbox mailbox;
    CommunicationWithErlang converter;
    JTextField userNameTextField;
    JTextField userIpTextField;
    JTextField userPortTextField;
    JFrame guiFrame;

    public Client() {
        converter = new CommunicationWithErlang(); 
        //converter.runSnameTerminalStuff();
        init_GUI();
    }
    
    private void init_GUI() {
        guiFrame = new JFrame();
        JPanel guiPanel = new JPanel(new GridBagLayout());
        JLabel userNameLabel = new JLabel("Namn:");
        userNameTextField = new JTextField(20);
        userNameTextField.setText("Name!");
        JLabel userIpLabel = new JLabel("IP:");
        userIpTextField = new JTextField(20);
        userIpTextField.setText("127.0.0.1");
        JLabel userPortLabel = new JLabel("Port:");
        userPortTextField = new JTextField(20);
        userPortTextField.setText("5555");
        JButton connect = new JButton("connect");
        connect.addActionListener(new ButtonListener());
        guiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        guiFrame.setTitle("Client");
        guiFrame.setSize(500, 250);
        guiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(3, 3, 3, 3);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(10, 10, 10, 10);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;
        guiPanel.add(userNameLabel, labelGBC);
        guiPanel.add(userNameTextField, fieldGBC);
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
                case "connect":
                    String playerID = userNameTextField.getText();
                    String ipNumber = userIpTextField.getText();
                    String portNumber = userPortTextField.getText();
                    //Det kanske ska vara nåt annat om det misslyckas med ip???
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
                    GameMenu gMenu = new GameMenu(mailbox, playerID, ipNumber, portNumber, users.getArguments()); 
                    guiFrame.dispose();
            }

        }
        /*private static String OS = System.getProperty("os.name").toLowerCase();
         public static boolean isWindows() {
         return (OS.indexOf("win") >= 0);
         }
         public static boolean isMac() {
         return (OS.indexOf("mac") >= 0);
         }
         public static boolean isUnix() {
         return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0);
         }
         public static boolean isSolaris() {
         return (OS.indexOf("sunos") >= 0);
         }*/
    }
    public static void main(String[] args) { 
        
        Client client = new Client();
    }
}
