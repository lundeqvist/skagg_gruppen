package games;

import com.ericsson.otp.erlang.OtpMbox;
import javax.swing.JPanel;
import communication.*;
import java.awt.Color;
import java.awt.event.*;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import utils.*;

public class OnlineList implements Runnable {
    private JPanel mainPanel;
    private JList onlineList;
    private CommunicationWithErlang converter;
    private OtpMbox mailbox;
    
    public OnlineList() {
        
    }
 
    private void init_content() {
        converter = new CommunicationWithErlang();
        String[] list = {"hej","ba","jo"};
        onlineList = new JList(list);
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
    }
    
    private void ServerListener() {
        while (true) {
            Arguments arguments = Utils.receiveMessage(mailbox, converter);
            String position = arguments.getArguments()[0];
            
        }
    }
    
    
}
