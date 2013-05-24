package games;

import com.ericsson.otp.erlang.OtpMbox;
import communication.CommunicationWithErlang;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.DateFormat;
import java.util.Date;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import utils.*;

public class GameMenuChat implements Runnable {

    public JTextField chatInput;
    public JButton sendButton;
    private JTextArea chatOutput;
    private JPanel mainPanel;
    private JScrollPane chatScrollPane;
    static private CommunicationWithErlang converter;
    static private String gameID = "chattId";
    private String playerID;
    private OtpMbox mailbox;

    public GameMenuChat(String playerID) {
        this.playerID = playerID;
    }

    public void init_content() {
        converter = new CommunicationWithErlang(playerID);
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
        Utils.sendMessage(mailbox, converter, gameID, playerID, "{join_game}");
        Thread listener = new Thread(new ServerListener());
        listener.start();
    }

    public void setMainPanel(JPanel mainPanelFromGM) {
        mainPanel = mainPanelFromGM;
    }

    @Override
    public void run() {
        init_content();
    }

    private class ServerListener implements Runnable {

        public ServerListener() {}

        private void serverListener() {
            while (true) {
                Arguments arguments = Utils.receiveMessage(mailbox, converter);
                String[] message = arguments.getArguments();
                String currentOutput = chatOutput.getText();
                chatOutput.setText(currentOutput + message[0] + "\n");
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
            switch (((AbstractButton) e.getSource()).getText()) {
                case "Send":
                    DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
                    Date date = new Date();
                    String input = chatInput.getText();
                    String newOutput = " [" + df.format(date) + "][" + playerID + "]: " + input + "\n";
                    Utils.sendMessage(mailbox, converter, gameID, playerID, "{" + newOutput + "}");
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
            switch (e.getKeyCode()) {
                case KeyEvent.VK_ENTER:
                    DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
                    Date date = new Date();
                    String input = chatInput.getText();
                    String newOutput = " [" + df.format(date) + "][" + playerID + "]: " + input + "\n";
                    Utils.sendMessage(mailbox, converter, gameID, playerID, "{" + newOutput + "}");
                    chatInput.setText("");
                    break;
                default:
                    break;
            }
        }

        @Override
        public void keyReleased(KeyEvent arg0) {}

        @Override
        public void keyTyped(KeyEvent arg0) {}
    }
}