package communication;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class Hello {

    JTextField portTextField;
    JFrame serverguiFrame;
    JFrame guiFrame;

    public Hello() {
        guiFrame = new JFrame();
        JPanel guiPanel = new JPanel(new GridBagLayout());

        JButton host = new JButton("Host Server");
        JButton join = new JButton("Join Server");

        host.addActionListener(new Hello.ButtonListener());
        join.addActionListener(new Hello.ButtonListener());
        guiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        guiFrame.setTitle("Hello");
        guiFrame.setSize(100, 130);

        guiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(3, 3, 3, 3);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(10, 10, 10, 10);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;

        guiPanel.add(host, fieldGBC);

        guiPanel.add(join, fieldGBC);

        guiFrame.add(guiPanel, BorderLayout.NORTH);
        guiFrame.setVisible(true);
    }

    public void serverGUI() {
        serverguiFrame = new JFrame();
        JPanel serverguiPanel = new JPanel(new GridBagLayout());
        JLabel portLabel = new JLabel("Port:");
        portTextField = new JTextField(20);
        portTextField.setText("5555");
        JButton ok = new JButton("Ok");
        ok.addActionListener(new Hello.ButtonListener());
        serverguiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        serverguiFrame.setTitle("Server port");
        serverguiFrame.setSize(300, 150);
        serverguiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(3, 3, 3, 3);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(10, 10, 10, 10);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;
        serverguiPanel.add(portTextField, fieldGBC);
        serverguiPanel.add(ok, fieldGBC);
        serverguiFrame.add(serverguiPanel, BorderLayout.NORTH);
        serverguiFrame.setVisible(true);
    }

    private class ButtonListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            switch (((AbstractButton) e.getSource()).getText()) {
                case "Host Server":
                    serverGUI();
                    break;
                case "Join Server":
                    guiFrame.setVisible(false);
                    Client client = new Client();
                    break;
                case "Ok":
                    String porten = portTextField.getText();
                    //det den här returnerar är en socket som när servern ska stängas så ska den
                    //anropas med imup_server:stop
                    final String cmdLine = "erl -shell -run imup_server start 5555";
                    //final String cmdLine = "werl";

                    try {
                        Process p = Runtime.getRuntime().exec(cmdLine);
                    } catch (IOException ioe) {
                    }
                    System.out.println("Server started");
                    serverguiFrame.setVisible(false);
            }
        }
    }
}
