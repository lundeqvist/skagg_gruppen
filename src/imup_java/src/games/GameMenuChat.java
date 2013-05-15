/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package games;

import com.ericsson.otp.erlang.OtpMbox;
import communication.CommunicationWithErlang;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import utils.*;

/**
 *  The graphical representation of GameMenuChat
 */
public class GameMenuChat implements Runnable {

    public JTextField chatInput;
    public JButton sendButton;
    private JTextArea chatOutput;
    private JPanel mainPanel;
    private JScrollPane chatScrollPane;
    static private CommunicationWithErlang converter;
    static private String gameID = "chattId";
    static private String playerID = "name";
    private OtpMbox mailbox;

    /**
     *  Initiates GUI and starts the receiveMessage loop.
     */
    public void init_content() {
        converter = new CommunicationWithErlang();
        chatInput = new JTextField(10);
        chatInput.setBounds(536, 286, 160, 50);
        chatInput.addKeyListener(new WindowListener());
        mainPanel.add(chatInput);
        sendButton = new JButton("Send");
        sendButton.addActionListener(new ButtonListener());
        sendButton.setBounds(706, 286, 80, 50);
        mainPanel.add(sendButton);
        chatOutput = new JTextArea(10, 16);
        chatOutput.setEditable(false);
        chatOutput.setBackground(Color.WHITE);
        chatScrollPane = new JScrollPane(chatOutput);
        chatScrollPane.setBounds(536, 10, 250, 266);
        mainPanel.add(chatScrollPane);
        mailbox = converter.createMailbox(gameID, playerID);
        receiveMessage();
    }

    /**
     *  Receives and prints when a new message is close.
     */
    public void receiveMessage() {
        while (true) {
            Arguments arguments = Utils.receiveMessage(mailbox, converter);
            String[] message = arguments.getArguments();
            String currentOutput = chatOutput.getText();
            //System.out.println(message);
            chatOutput.setText(currentOutput + "\n" + message[0]);
        }
    }

    public void setMainPanel(JPanel mainPanelFromGM) {
        mainPanel = mainPanelFromGM;
    }

    @Override
    public void run() {
        init_content();
    }

    /**
     * Sends the message to the server when the button "Send" is pressed.
     */
    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "Send":
                    String input = chatInput.getText();
                    Utils.sendMessage(mailbox, converter, gameID, playerID, "{" + input + "}");
                    chatInput.setText("");
                    break;
                default:
                    System.out.println("Not implemented yet");
                    break;
            }
        }
    }

    /**
     * Sends the message to the server when the enter button is pressed.
     */
    private class WindowListener implements KeyListener {
        @Override
        public void keyPressed(KeyEvent e) {
            // TODO Auto-generated method stub
            switch (e.getKeyCode()) {
                case KeyEvent.VK_ENTER:
                    //DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
                    //Date date = new Date();
                    //String newOutput = " [" + df.format(date) + "]: " + input + "\n";
                    String input = chatInput.getText();
                    Utils.sendMessage(mailbox, converter, gameID, playerID, "{" + input + "}");
                    chatInput.setText("");
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
}