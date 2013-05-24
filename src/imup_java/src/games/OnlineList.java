package games;

import com.ericsson.otp.erlang.OtpMbox;
import communication.*;
import java.awt.Color;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
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
    private String playerID;

    public OnlineList(OtpMbox mailbox, String playerID, String[] users) {
        this.playerID = playerID;
        this.mailbox = mailbox;
        userVector = new Vector<String>();
        updateUsers(users);
        converter = new CommunicationWithErlang(playerID);
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
        Thread listener = new Thread(new ServerListener());
        listener.start();
    }

    /**
     * Clears the userVector, parses the string representation of all the users
     * to a stringarray and then adds every element to the userVector.
     *
     * @param users String representation of all the users who is online.
     */
    private void updateUsers(String[] users) {
        userVector.clear();
        for (int i = 0; i < users.length; i++) {
            userVector.addElement(users[i]);
        }
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                onlineList.updateUI();
            }
        });
    }

    /**
     * @return a stringarray of the selected users.
     */
    public String[] getSelectedUsers() {
        int players = onlineList.getSelectedIndices().length;
        return onlineList.getSelectedValuesList().toArray(new String[players]);
    }

    /**
     * Listens to the server and updates the onlinelist when a new user is
     * connected.
     */
    private class ServerListener implements Runnable {

        public ServerListener() {}

        private void serverListener() {
            while (true) {
                Arguments arguments = Utils.receiveMessage(mailbox, converter);
                String[] users = arguments.getArguments();
                System.out.println(users.length + "");
                updateUsers(users);
            }
        }

        @Override
        public void run() {
            serverListener();
        }
    }
}
