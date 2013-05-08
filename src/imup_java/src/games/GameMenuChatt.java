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

/**
 *
 * @author Linda
 */
public class GameMenuChatt implements Runnable {

    public JTextField chatInput;
    public JButton sendButton;
    private JTextArea chatOutput;
    private JPanel mainPanel;
    private JScrollPane chatScrollPane;    
    static private CommunicationWithErlang converter;
    static private String gameID = "chattId";
    static private String playerID = "name";
    private OtpMbox mailbox;
    private utils.Utils utils;

    public void init_content() {    
        utils = new utils.Utils();
        converter = new CommunicationWithErlang();    
        chatInput = new JTextField(10);
        chatInput.setBounds(286, 286, 160, 50);
        chatInput.addKeyListener(new WindowListener());                
        mainPanel.add(chatInput);
        sendButton = new JButton("Send");
        sendButton.addActionListener(new ButtonListener());
        sendButton.setBounds(456, 286, 80, 50);
        mainPanel.add(sendButton);
        chatOutput = new JTextArea(10, 16);
        chatOutput.setEditable(false);
        chatOutput.setBackground(Color.WHITE);
        chatScrollPane = new JScrollPane(chatOutput);
        chatScrollPane.setBounds(286, 10, 250, 266);
        mainPanel.add(chatScrollPane);
        mailbox = converter.createMailbox(playerID, gameID);        
    }

    public void receiveMessage() {
        while (true) {  
            utils.receiveChattMessage(chatOutput, mailbox, playerID, gameID);            
        }
    }
       
    public void setMainPanel(JPanel mainPanelFromGM) {
        mainPanel = mainPanelFromGM;
    }

    @Override
    public void run() {        
        init_content();        
    }

    private class ButtonListener implements ActionListener {
 
        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "Send":
                    String input = chatInput.getText();
                    utils.sendChattMessage(input, mailbox, playerID, gameID); 
                    chatInput.setText("");
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
                case KeyEvent.VK_ENTER:
                    String input = chatInput.getText();
                    utils.sendChattMessage(input, mailbox, playerID, gameID); 
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