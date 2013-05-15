package games;

import com.ericsson.otp.erlang.OtpMbox;
import javax.swing.JPanel;
import communication.*;
import java.awt.Color;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import utils.*;

    /**
     * The graphical representation of all the users who is online.
     */
public class OnlineList implements Runnable {
    private JPanel mainPanel;
    private JList<String> onlineList;
    private CommunicationWithErlang converter;
    private OtpMbox mailbox;
    private Vector<String> userVector;
    private String playerID, gameID = "onlineID";
    
    /**
     * @param mailbox 
     * @param playerID
     * @param users 
     */
    public OnlineList(OtpMbox mailbox, String playerID, String users) {
        this.playerID = playerID;
        this.mailbox = mailbox;
        userVector = new Vector<String>();
        updateUsers(users);
        converter = new CommunicationWithErlang();
    }
 
    /**
     * Initiates the GUI.
     */
    private void init_content() {
        onlineList = new JList<String>(userVector);
        JTextArea chatOutput = new JTextArea(10, 16);
        chatOutput.setEditable(false);
        chatOutput.setBackground(Color.WHITE);
        JScrollPane jsp = new JScrollPane(chatOutput);
        jsp.setBounds(10, 10, 240, 320);
        jsp.getViewport().setView(onlineList);
        mainPanel.add(jsp);
    }
    
    public void setMainPanel(JPanel mainPanelFromGM) {
        mainPanel = mainPanelFromGM;
    }
    @Override
    public void run() {
        init_content();
        serverListener();
    }
    
    /**
     * Clears the userVector, parses the string representation of all the users
     * to a stringarray and then adds every element to the userVector.
     * 
     * @param users String representation of all the users who is online.
     */
    private void updateUsers(String users) {
        userVector.clear();
        String[] userlist = Utils.parseTupleString(users);
        for(int i=0;i<userlist.length;i++){
            userVector.addElement(userlist[i]);
        }       
    }
    
    /**
     * @return a stringarray of the selected users.
     */
    public String[] getSelectedUsers() {
       return onlineList.getSelectedValuesList().toArray(new String[4]);
    }
    
    /**
     * Listens to the server and updates the onlinelist when a new user is 
     * connected.
     */
    private void serverListener() {
        while (true) {
            Arguments arguments = Utils.receiveMessage(mailbox, converter);
            String users = arguments.getArguments()[0];
            updateUsers(users);
        }
    }
    
    /**
     * Listens if the "close-window-button" and will then let the server know
     * that the user has left the server.
     */
    private class ButtonListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
        }
    }
    
    
}
