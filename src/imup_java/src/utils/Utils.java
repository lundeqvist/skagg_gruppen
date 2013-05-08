package utils;

import com.ericsson.otp.erlang.*;
import java.text.DateFormat;
import java.util.Date;
import javax.swing.*;
import communication.*;

public class Utils {
    private games.Game game;
    static Client client = new Client();
    static CommunicationWithErlang converter = new CommunicationWithErlang();

    public void sendChattMessage(String input, OtpMbox mailbox, String playerID, String gameID) {
        if (!input.equals("")) {
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
            Date date = new Date();
            String newOutput = " [" + df.format(date) + "]: " + input + "\n";
            converter.send(game.convertToErlang(mailbox, gameID, playerID, "{" + newOutput + "}"), mailbox);
        }
    }

    public void receiveChattMessage(JTextArea chatOutput, OtpMbox mailbox, String playerID, String gameID) {
        OtpErlangObject temp = converter.receive(mailbox);
        String[] message = game.convertToJava(temp);
        String currentOutput = chatOutput.getText();
        chatOutput.setText(currentOutput + "\n" + message[3]);
    }

    /*public void sendMove(String gameID, String playerID, String actionCommand, OtpMbox mailbox) {
        converter.send(convertToErlang(mailbox, new Arguments(mailbox, gameID, playerID, "{" + actionCommand + "}")), mailbox);
    }

    public void receiveMove() {
        OtpErlangObject temp = converter.receive(mailbox);
        String[] message = convertToJava(temp);
    }
*/
    public static int serverConnect() {
        return 0;
    }

    /*public static String[] tupleToStringArray(OtpErlangTuple tuple) {
        String[] tupleArgs = new String[tuple.arity()];
        for (int i = 0; i < tuple.arity(); i++) {
            tupleArgs[i] = tuple.elementAt(i).toString();
        }
        return tupleArgs;
    }

    public static String[] tupleToStringArray(OtpErlangTuple tuple, int length) {
        String[] tupleArgs = new String[length];
        for (int i = 0; i < length; i++) {
            if (tuple.elementAt(i) instanceof OtpErlangTuple) {
                String[] innerTuple = tupleToStringArray((OtpErlangTuple) tuple.elementAt(i), ((OtpErlangTuple) tuple.elementAt(i)).arity());
                for (int j = i; j < i + innerTuple.length; j++) {
                    tupleArgs[j] = innerTuple[j - i];
                }
                i += innerTuple.length;
            } else {
                tupleArgs[i] = tuple.elementAt(i).toString();
            }
        }
        return tupleArgs;
    }
*/
    public static String[] parseTupleString(String tuple) {
        tuple = tuple.trim();
        tuple = tuple.substring(1, tuple.length() - 1);
        String[] parsedTuple = tuple.split(",");
        for (int i = 0; i < parsedTuple.length; i++) {
            parsedTuple[i] = parsedTuple[i].trim();
        }
        return parsedTuple;
    }
    
    public static int[] splitCoordinates(String coordinates) {
        int[] xy = new int[1];
        xy[0] = Integer.parseInt(coordinates.substring(0, 1));
        xy[1] = Integer.parseInt(coordinates.substring(1, 2));
        return xy;
    }
}
