package utils;

import java.text.DateFormat;
import java.util.Date;
import javax.swing.*;
import communication.Client;
import communication.Converter;

public class Utils {
    static Client client = new Client();
    static Converter converter = new Converter();

    public static void updateChat(JTextArea chatOutput, JTextField chatInput) {
        String input = chatInput.getText();
        if (!input.equals("")) {
            String currentOutput = chatOutput.getText();
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
            Date date = new Date();
            String newOutput = " [" + df.format(date) + "]: " + input + "\n";
            converter.send_message(2, "playerName", "chattId", newOutput);           
            String temp = converter.receive_message("chattId");  
            chatOutput.setText(currentOutput + "\n" + temp);
            chatInput.setText("");
        }
    }

    public static int serverConnect() {
        return 0;
    }
}