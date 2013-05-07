/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package games;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import communication.Converter;
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
//import utils.Utils;

/**
 *
 * @author Linda
 */
public class GameMenuChattInput implements Runnable {

    public JTextField chatInput;
    public JButton sendButton;
    private JTextArea chatOutput;
    private JPanel mainPanel;
    private JScrollPane chatScrollPane;    
    static private Converter converter;
    static private String gameName = "chattId";
    static private String personName = "name";
    private OtpMbox mailbox;

    public void init_content() {
        //gMenu = new GameMenu();
        converter = new Converter();
        //mainPanel = gMenu.getMainPanel();
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
        mailbox = converter.createMailbox(gameName);
        if (mailbox == null) {
            System.out.println("Det har inte skapats någon mailbox");
        } else {
            converter.send_messagePing(personName, gameName, mailbox);
            receiveMessage();
        }
    }

    public void receiveMessage() {
        while (true) {            
            OtpErlangObject robj = converter.receive(mailbox);            
            String message = convertToJava(robj);           
            System.out.println("Received mssage " + message);
            String currentOutput = chatOutput.getText();
            chatOutput.setText(currentOutput + "\n" + message);
            chatInput.setText("");
        }
    }

    public OtpErlangTuple convertToErlang(String personId, String spelid, String message) {
        System.out.println("Convert to erlang" + message);
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(personId);
        id[1] = new OtpErlangAtom(spelid);
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom("chatt");
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(message);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        //mailbox.send("pong", server, tuple);
        return tuple;
    }

    public String convertToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        OtpErlangPid fromPid = (OtpErlangPid) (rtuple.elementAt(0));
        OtpErlangObject rmsg = rtuple.elementAt(1);
        System.out.println("Message: " + rmsg + " received from:  "
                + fromPid.toString());
        String message = rmsg.toString();
        System.out.println("Convert to java " + message);
        return message;
    }

    public void setMainPanel(JPanel mainPanelFromGM) {
        mainPanel = mainPanelFromGM;
    }

    @Override
    public void run() {
        //gMenu.getMainPanel();
        init_content();
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "Send":
                    DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
                    Date date = new Date();
                    String newOutput = " [" + df.format(date) + "]: " + chatInput.getText();
                    converter.send(convertToErlang(personName, gameName, newOutput), mailbox);
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
                    DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
                    Date date = new Date();
                    String newOutput = " [" + df.format(date) + "]: " + chatInput.getText();
                    converter.send(convertToErlang(personName, gameName, newOutput), mailbox);
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